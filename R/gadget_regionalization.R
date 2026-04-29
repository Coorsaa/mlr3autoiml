# FILE: R/gadget_regionalization.R

#' @title GADGET-style Regionalization for Gate 2
#'
#' @description
#' Internal GADGET-style regionalization used by Gate 2 to identify regions of
#' heterogeneous feature effects. The implementation uses centered ICE matrices
#' as local effect curves and can jointly regionalize multiple features by
#' minimizing the sum of within-region curve risks across the selected feature
#' set. This mirrors the key GADGET idea: decompose global effects into
#' interpretable regions when interactions make a single global curve too coarse.
#'
#' @name gadget_regionalization
#' @keywords internal
NULL

.autoiml_gadget_loss = function(pred_mat) {
  if (is.null(pred_mat) || nrow(pred_mat) == 0L || ncol(pred_mat) == 0L) {
    return(NA_real_)
  }
  mu = colMeans(pred_mat, na.rm = TRUE)
  res = pred_mat - matrix(mu, nrow = nrow(pred_mat), ncol = length(mu), byrow = TRUE)
  sum(res^2, na.rm = TRUE)
}

.autoiml_gadget_grid_mask = function(grid, bounds, feature) {
  if (is.null(grid) || length(grid) == 0L) {
    return(logical())
  }
  x = suppressWarnings(as.numeric(grid))
  ok = is.finite(x)
  if (is.null(bounds) || !feature %in% names(bounds)) {
    return(ok)
  }
  b = bounds[[feature]]
  lo = if (!is.null(b$lower) && is.finite(b$lower)) as.numeric(b$lower) else -Inf
  hi = if (!is.null(b$upper) && is.finite(b$upper)) as.numeric(b$upper) else Inf
  ok & x >= lo & x <= hi
}

.autoiml_gadget_feature_loss = function(pred_mat, grid, idx, bounds, feature) {
  if (is.null(pred_mat) || length(idx) == 0L) {
    return(NA_real_)
  }
  mask = .autoiml_gadget_grid_mask(grid, bounds, feature)
  if (length(mask) != ncol(pred_mat) || !any(mask)) {
    return(NA_real_)
  }
  .autoiml_gadget_loss(pred_mat[idx, mask, drop = FALSE])
}

.autoiml_gadget_objective = function(pred_mats, grids, idx, bounds) {
  feats = intersect(names(pred_mats), names(grids))
  if (length(feats) == 0L || length(idx) == 0L) {
    return(list(total = NA_real_, by_feature = numeric()))
  }
  losses = vapply(feats, function(ff) {
    .autoiml_gadget_feature_loss(
      pred_mat = pred_mats[[ff]],
      grid = grids[[ff]],
      idx = idx,
      bounds = bounds,
      feature = ff
    )
  }, numeric(1L))
  finite = is.finite(losses)
  list(
    total = if (any(finite)) sum(losses[finite], na.rm = TRUE) else NA_real_,
    by_feature = losses
  )
}

.autoiml_gadget_update_bounds = function(bounds, feature, split_type, threshold = NULL, side = c("left", "right")) {
  side = match.arg(side)
  out = bounds
  if (is.null(out) || !feature %in% names(out) || !identical(split_type, "numeric")) {
    return(out)
  }
  t = suppressWarnings(as.numeric(threshold))
  if (!is.finite(t)) {
    return(out)
  }
  b = out[[feature]]
  lo = if (!is.null(b$lower) && is.finite(b$lower)) as.numeric(b$lower) else -Inf
  hi = if (!is.null(b$upper) && is.finite(b$upper)) as.numeric(b$upper) else Inf
  if (side == "left") {
    hi = min(hi, t)
  } else {
    lo = max(lo, t)
  }
  out[[feature]] = list(lower = lo, upper = hi)
  out
}

.autoiml_gadget_bounds_label = function(bounds) {
  if (is.null(bounds) || length(bounds) == 0L) {
    return(NA_character_)
  }
  paste(vapply(names(bounds), function(ff) {
    b = bounds[[ff]]
    lo = if (!is.null(b$lower) && is.finite(b$lower)) sprintf("%.4g", b$lower) else "-Inf"
    hi = if (!is.null(b$upper) && is.finite(b$upper)) sprintf("%.4g", b$upper) else "Inf"
    sprintf("%s in [%s, %s]", ff, lo, hi)
  }, character(1L)), collapse = "; ")
}

.autoiml_gadget_best_split = function(X, idx, pred_mat, feature, split_candidates,
  min_bucket = 30L, n_thresholds = 5L) {

  min_bucket = as.integer(min_bucket)
  n_thresholds = as.integer(n_thresholds)

  loss_parent = .autoiml_gadget_loss(pred_mat[idx, , drop = FALSE])
  if (!is.finite(loss_parent)) {
    return(NULL)
  }

  best = NULL
  best_gain = -Inf

  for (sf in split_candidates) {
    if (!sf %in% names(X)) next
    if (identical(sf, feature)) next

    z = X[[sf]][idx]

    if (is.numeric(z) || is.integer(z)) {
      z_num = suppressWarnings(as.numeric(z))
      z_ok = z_num[is.finite(z_num)]
      if (length(z_ok) < 2L) next

      probs = seq(0.1, 0.9, length.out = n_thresholds)
      thr = unique(as.numeric(stats::quantile(z_ok, probs = probs, na.rm = TRUE, type = 1)))
      thr = thr[is.finite(thr)]
      if (length(thr) == 0L) next

      for (t in thr) {
        left_local = which(!is.na(z_num) & z_num <= t)
        left = idx[left_local]
        right = idx[setdiff(seq_along(idx), left_local)]

        if (length(left) < min_bucket || length(right) < min_bucket) next

        loss_left = .autoiml_gadget_loss(pred_mat[left, , drop = FALSE])
        loss_right = .autoiml_gadget_loss(pred_mat[right, , drop = FALSE])
        if (!is.finite(loss_left) || !is.finite(loss_right)) next

        gain = loss_parent - (loss_left + loss_right)
        if (is.finite(gain) && gain > best_gain) {
          best_gain = gain
          best = list(
            feature = sf,
            type = "numeric",
            threshold = t,
            left = left,
            right = right,
            loss_parent = loss_parent,
            loss_left = loss_left,
            loss_right = loss_right,
            gain = gain
          )
        }
      }
      next
    }

    if (is.factor(z) || is.character(z) || is.logical(z)) {
      z_chr = as.character(z)
      lev = unique(z_chr[!is.na(z_chr)])
      if (length(lev) < 2L) next
      if (length(lev) > 10L) lev = lev[seq_len(10L)]

      for (lv in lev) {
        left_local = which(!is.na(z_chr) & z_chr == lv)
        left = idx[left_local]
        right = idx[setdiff(seq_along(idx), left_local)]

        if (length(left) < min_bucket || length(right) < min_bucket) next

        loss_left = .autoiml_gadget_loss(pred_mat[left, , drop = FALSE])
        loss_right = .autoiml_gadget_loss(pred_mat[right, , drop = FALSE])
        if (!is.finite(loss_left) || !is.finite(loss_right)) next

        gain = loss_parent - (loss_left + loss_right)
        if (is.finite(gain) && gain > best_gain) {
          best_gain = gain
          best = list(
            feature = sf,
            type = "factor",
            level = lv,
            left = left,
            right = right,
            loss_parent = loss_parent,
            loss_left = loss_left,
            loss_right = loss_right,
            gain = gain
          )
        }
      }
      next
    }
  }

  best
}

.autoiml_gadget_best_split_multi = function(
  X,
  idx,
  pred_mats,
  grids,
  features,
  split_candidates,
  bounds,
  min_bucket = 30L,
  n_thresholds = 7L
) {
  min_bucket = as.integer(min_bucket)
  n_thresholds = as.integer(n_thresholds)

  obj_parent = .autoiml_gadget_objective(pred_mats, grids, idx, bounds)
  if (!is.finite(obj_parent$total)) {
    return(NULL)
  }

  best = NULL
  best_gain = -Inf

  for (sf in split_candidates) {
    if (!sf %in% names(X)) next
    z = X[[sf]][idx]

    if (is.numeric(z) || is.integer(z)) {
      z_num = suppressWarnings(as.numeric(z))
      z_ok = z_num[is.finite(z_num)]
      if (length(z_ok) < 2L) next

      probs = seq(0.1, 0.9, length.out = max(2L, n_thresholds))
      thr = unique(as.numeric(stats::quantile(z_ok, probs = probs, na.rm = TRUE, type = 1)))
      thr = thr[is.finite(thr)]
      if (length(thr) == 0L) next

      for (t in thr) {
        left_local = which(!is.na(z_num) & z_num <= t)
        left = idx[left_local]
        right = idx[setdiff(seq_along(idx), left_local)]

        if (length(left) < min_bucket || length(right) < min_bucket) next

        bounds_left = .autoiml_gadget_update_bounds(bounds, sf, "numeric", threshold = t, side = "left")
        bounds_right = .autoiml_gadget_update_bounds(bounds, sf, "numeric", threshold = t, side = "right")

        obj_left = .autoiml_gadget_objective(pred_mats, grids, left, bounds_left)
        obj_right = .autoiml_gadget_objective(pred_mats, grids, right, bounds_right)
        if (!is.finite(obj_left$total) || !is.finite(obj_right$total)) next

        child = obj_left$total + obj_right$total
        gain = obj_parent$total - child
        gain_ratio = if (is.finite(obj_parent$total) && obj_parent$total > 0) gain / obj_parent$total else NA_real_

        if (is.finite(gain) && gain > best_gain) {
          best_gain = gain
          best = list(
            feature = sf,
            type = "numeric",
            threshold = t,
            level = NA_character_,
            left = left,
            right = right,
            bounds_left = bounds_left,
            bounds_right = bounds_right,
            obj_parent = obj_parent,
            obj_left = obj_left,
            obj_right = obj_right,
            gain = gain,
            gain_ratio = gain_ratio
          )
        }
      }
      next
    }

    if (is.factor(z) || is.character(z) || is.logical(z)) {
      z_chr = as.character(z)
      lev = unique(z_chr[!is.na(z_chr)])
      if (length(lev) < 2L) next
      if (length(lev) > 12L) {
        freq = sort(table(z_chr), decreasing = TRUE)
        lev = names(freq)[seq_len(min(12L, length(freq)))]
      }

      for (lv in lev) {
        left_local = which(!is.na(z_chr) & z_chr == lv)
        left = idx[left_local]
        right = idx[setdiff(seq_along(idx), left_local)]

        if (length(left) < min_bucket || length(right) < min_bucket) next

        obj_left = .autoiml_gadget_objective(pred_mats, grids, left, bounds)
        obj_right = .autoiml_gadget_objective(pred_mats, grids, right, bounds)
        if (!is.finite(obj_left$total) || !is.finite(obj_right$total)) next

        child = obj_left$total + obj_right$total
        gain = obj_parent$total - child
        gain_ratio = if (is.finite(obj_parent$total) && obj_parent$total > 0) gain / obj_parent$total else NA_real_

        if (is.finite(gain) && gain > best_gain) {
          best_gain = gain
          best = list(
            feature = sf,
            type = "factor",
            threshold = NA_real_,
            level = lv,
            left = left,
            right = right,
            bounds_left = bounds,
            bounds_right = bounds,
            obj_parent = obj_parent,
            obj_left = obj_left,
            obj_right = obj_right,
            gain = gain,
            gain_ratio = gain_ratio
          )
        }
      }
      next
    }
  }

  best
}

.autoiml_gadget_regionalize_feature = function(
  X,
  row_ids,
  feature,
  grid,
  pred_mat,
  split_candidates = NULL,
  max_depth = 3L,
  min_bucket = 30L,
  gamma = 0,
  n_thresholds = 5L,
  seed = NULL
) {
  stopifnot(is.data.frame(X))
  stopifnot(is.matrix(pred_mat))
  stopifnot(nrow(X) == nrow(pred_mat))

  if (!is.null(seed)) set.seed(as.integer(seed))

  max_depth = as.integer(max_depth)
  min_bucket = as.integer(min_bucket)
  gamma = as.numeric(gamma)

  if (is.null(split_candidates)) {
    split_candidates = setdiff(names(X), feature)
  }

  idx_all = seq_len(nrow(X))

  regions = list()
  assignments = data.table::data.table(
    row_id = row_ids,
    region_id = NA_integer_,
    path = NA_character_
  )
  curves = list()

  region_id = 0L

  make_leaf = function(idx, path, depth) {
    region_id <<- region_id + 1L

    M = pred_mat[idx, , drop = FALSE]
    mu = colMeans(M, na.rm = TRUE)
    loss = .autoiml_gadget_loss(M)

    regions[[region_id]] <<- data.table::data.table(
      region_id = region_id,
      path = if (nzchar(path)) path else "(root)",
      depth = depth,
      n = length(idx),
      loss = loss
    )

    assignments$region_id[idx] <<- region_id
    assignments$path[idx] <<- if (nzchar(path)) path else "(root)"

    curves[[region_id]] <<- data.table::data.table(
      region_id = region_id,
      path = if (nzchar(path)) path else "(root)",
      x = as.numeric(grid),
      y = as.numeric(mu)
    )
  }

  build = function(idx, path, depth) {
    if (depth >= max_depth || length(idx) < 2L * min_bucket) {
      make_leaf(idx, path, depth)
      return(invisible(NULL))
    }

    sp = .autoiml_gadget_best_split(
      X = X,
      idx = idx,
      pred_mat = pred_mat,
      feature = feature,
      split_candidates = split_candidates,
      min_bucket = min_bucket,
      n_thresholds = n_thresholds
    )

    if (is.null(sp) || !is.finite(sp$gain) || sp$gain <= gamma) {
      make_leaf(idx, path, depth)
      return(invisible(NULL))
    }

    if (identical(sp$type, "numeric")) {
      rule_left = sprintf("%s <= %.4g", sp$feature, sp$threshold)
      rule_right = sprintf("%s > %.4g", sp$feature, sp$threshold)
    } else {
      rule_left = sprintf("%s == %s", sp$feature, sp$level)
      rule_right = sprintf("%s != %s", sp$feature, sp$level)
    }

    path_left = if (nzchar(path)) paste(path, rule_left, sep = " & ") else rule_left
    path_right = if (nzchar(path)) paste(path, rule_right, sep = " & ") else rule_right

    build(sp$left, path_left, depth + 1L)
    build(sp$right, path_right, depth + 1L)

    invisible(NULL)
  }

  build(idx_all, "", 0L)

  list(
    feature = feature,
    grid = as.numeric(grid),
    regions = data.table::rbindlist(regions, fill = TRUE),
    assignments = assignments,
    curves = data.table::rbindlist(curves, fill = TRUE)
  )
}

.autoiml_gadget_regionalize_multi = function(
  X,
  row_ids,
  features,
  grids,
  pred_mats,
  split_candidates = NULL,
  max_depth = 3L,
  min_bucket = 30L,
  gamma = 0,
  n_thresholds = 7L,
  local_keep_n = 150L,
  class_label = NULL,
  method = "pdp",
  seed = NULL
) {
  stopifnot(is.data.frame(X))
  X = data.table::as.data.table(X)

  if (!is.null(seed)) set.seed(as.integer(seed))

  features = unique(as.character(features))
  features = intersect(features, intersect(names(grids), names(pred_mats)))
  features = intersect(features, names(X))
  if (length(features) == 0L) {
    return(NULL)
  }

  pred_mats = pred_mats[features]
  grids = grids[features]

  ok = vapply(features, function(ff) {
    is.matrix(pred_mats[[ff]]) && nrow(pred_mats[[ff]]) == nrow(X) && ncol(pred_mats[[ff]]) == length(grids[[ff]])
  }, logical(1L))
  features = features[ok]
  if (length(features) == 0L) {
    return(NULL)
  }
  pred_mats = pred_mats[features]
  grids = grids[features]

  if (is.null(split_candidates)) {
    split_candidates = names(X)
  }
  split_candidates = unique(intersect(as.character(split_candidates), names(X)))
  if (length(split_candidates) == 0L) {
    split_candidates = names(X)
  }

  max_depth = max(0L, as.integer(max_depth))
  min_bucket = max(2L, as.integer(min_bucket))
  gamma = as.numeric(gamma)
  n_thresholds = max(2L, as.integer(n_thresholds))
  local_keep_n = max(0L, as.integer(local_keep_n))

  idx_all = seq_len(nrow(X))
  root_bounds = stats::setNames(lapply(features, function(ff) list(lower = -Inf, upper = Inf)), features)
  root_obj = .autoiml_gadget_objective(pred_mats, grids, idx_all, root_bounds)
  if (!is.finite(root_obj$total)) {
    return(NULL)
  }

  regions = list()
  curves = list()
  local_curves = list()
  splits = list()
  leaf_feature_losses = list()

  assignments = data.table::data.table(
    row_id = row_ids,
    region_id = NA_integer_,
    path = NA_character_
  )

  region_id = 0L
  split_id = 0L
  class_chr = if (is.null(class_label)) "regression" else as.character(class_label)[1L]
  method_chr = as.character(method)[1L]

  append_dt = function(x) {
    if (length(x) == 0L) data.table::data.table() else data.table::rbindlist(x, fill = TRUE)
  }

  make_leaf = function(idx, path, depth, bounds) {
    region_id <<- region_id + 1L
    path_chr = if (nzchar(path)) path else "(root)"
    obj = .autoiml_gadget_objective(pred_mats, grids, idx, bounds)
    bounds_label = .autoiml_gadget_bounds_label(bounds)

    regions[[region_id]] <<- data.table::data.table(
      region_id = region_id,
      path = path_chr,
      depth = as.integer(depth),
      n = length(idx),
      total_loss = as.numeric(obj$total),
      bounds = bounds_label,
      class_label = class_chr,
      method = method_chr
    )

    if (length(obj$by_feature) > 0L) {
      leaf_feature_losses[[region_id]] <<- data.table::data.table(
        region_id = region_id,
        path = path_chr,
        depth = as.integer(depth),
        n = length(idx),
        feature = names(obj$by_feature),
        loss = as.numeric(obj$by_feature),
        class_label = class_chr,
        method = method_chr
      )
    }

    assignments$region_id[idx] <<- region_id
    assignments$path[idx] <<- path_chr

    keep_idx = idx
    if (local_keep_n > 0L && length(keep_idx) > local_keep_n) {
      keep_idx = sample(keep_idx, local_keep_n)
    }

    for (ff in features) {
      mask = .autoiml_gadget_grid_mask(grids[[ff]], bounds, ff)
      if (!any(mask)) next
      x = as.numeric(grids[[ff]][mask])
      M = pred_mats[[ff]][idx, mask, drop = FALSE]
      y_region = colMeans(M, na.rm = TRUE)
      y_global = colMeans(pred_mats[[ff]][, mask, drop = FALSE], na.rm = TRUE)

      curves[[length(curves) + 1L]] <<- data.table::data.table(
        feature = ff,
        region_id = region_id,
        path = path_chr,
        depth = as.integer(depth),
        n = length(idx),
        x = x,
        y_region = as.numeric(y_region),
        y_global = as.numeric(y_global),
        class_label = class_chr,
        method = method_chr
      )

      if (local_keep_n > 0L && length(keep_idx) > 0L) {
        Mloc = pred_mats[[ff]][keep_idx, mask, drop = FALSE]
        local_curves[[length(local_curves) + 1L]] <<- data.table::data.table(
          feature = ff,
          row_id = rep(row_ids[keep_idx], each = length(x)),
          region_id = region_id,
          path = path_chr,
          x = rep(x, times = length(keep_idx)),
          local_effect = as.numeric(t(Mloc)),
          class_label = class_chr,
          method = method_chr
        )
      }
    }
  }

  build = function(idx, path, depth, bounds) {
    if (depth >= max_depth || length(idx) < 2L * min_bucket) {
      make_leaf(idx, path, depth, bounds)
      return(invisible(NULL))
    }

    sp = .autoiml_gadget_best_split_multi(
      X = X,
      idx = idx,
      pred_mats = pred_mats,
      grids = grids,
      features = features,
      split_candidates = split_candidates,
      bounds = bounds,
      min_bucket = min_bucket,
      n_thresholds = n_thresholds
    )

    if (is.null(sp) || !is.finite(sp$gain) || sp$gain <= 0) {
      make_leaf(idx, path, depth, bounds)
      return(invisible(NULL))
    }

    if (is.finite(gamma) && gamma > 0) {
      ratio = sp$gain_ratio
      if (!is.finite(ratio) || ratio < gamma) {
        make_leaf(idx, path, depth, bounds)
        return(invisible(NULL))
      }
    }

    if (identical(sp$type, "numeric")) {
      rule_left = sprintf("%s <= %.4g", sp$feature, sp$threshold)
      rule_right = sprintf("%s > %.4g", sp$feature, sp$threshold)
    } else {
      rule_left = sprintf("%s == %s", sp$feature, sp$level)
      rule_right = sprintf("%s != %s", sp$feature, sp$level)
    }

    path_left = if (nzchar(path)) paste(path, rule_left, sep = " & ") else rule_left
    path_right = if (nzchar(path)) paste(path, rule_right, sep = " & ") else rule_right

    split_id <<- split_id + 1L
    splits[[split_id]] <<- data.table::data.table(
      split_id = split_id,
      path = if (nzchar(path)) path else "(root)",
      depth = as.integer(depth),
      split_feature = sp$feature,
      split_type = sp$type,
      threshold = as.numeric(sp$threshold),
      level = as.character(sp$level),
      n_parent = length(idx),
      n_left = length(sp$left),
      n_right = length(sp$right),
      objective_parent = as.numeric(sp$obj_parent$total),
      objective_left = as.numeric(sp$obj_left$total),
      objective_right = as.numeric(sp$obj_right$total),
      gain = as.numeric(sp$gain),
      gain_ratio = as.numeric(sp$gain_ratio),
      rule_left = rule_left,
      rule_right = rule_right,
      target_features = paste(features, collapse = ", "),
      class_label = class_chr,
      method = method_chr
    )

    build(sp$left, path_left, depth + 1L, sp$bounds_left)
    build(sp$right, path_right, depth + 1L, sp$bounds_right)
    invisible(NULL)
  }

  build(idx_all, "", 0L, root_bounds)

  regions_dt = append_dt(regions)
  curves_dt = append_dt(curves)
  local_dt = append_dt(local_curves)
  splits_dt = append_dt(splits)
  leaf_loss_dt = append_dt(leaf_feature_losses)

  assignments[, `:=`(class_label = class_chr, method = method_chr)]

  global_curves = data.table::rbindlist(lapply(features, function(ff) {
    data.table::data.table(
      feature = ff,
      x = as.numeric(grids[[ff]]),
      y_global = as.numeric(colMeans(pred_mats[[ff]], na.rm = TRUE)),
      class_label = class_chr,
      method = method_chr
    )
  }), fill = TRUE)

  root_loss_dt = data.table::data.table(
    feature = names(root_obj$by_feature),
    root_loss = as.numeric(root_obj$by_feature)
  )
  terminal_loss_dt = if (nrow(leaf_loss_dt) > 0L) {
    leaf_loss_dt[, .(terminal_loss = sum(loss, na.rm = TRUE)), by = feature]
  } else {
    data.table::data.table(feature = character(), terminal_loss = numeric())
  }
  feature_metrics = merge(root_loss_dt, terminal_loss_dt, by = "feature", all.x = TRUE)
  feature_metrics[is.na(terminal_loss), terminal_loss := NA_real_]
  feature_metrics[, heterogeneity_reduction := data.table::fifelse(
    is.finite(root_loss) & root_loss > 0 & is.finite(terminal_loss),
    1 - terminal_loss / root_loss,
    NA_real_
  )]
  feature_metrics[, `:=`(class_label = class_chr, method = method_chr)]

  terminal_total = if (nrow(regions_dt) > 0L) sum(regions_dt$total_loss, na.rm = TRUE) else NA_real_
  total_metrics = data.table::data.table(
    root_loss = as.numeric(root_obj$total),
    terminal_loss = as.numeric(terminal_total),
    heterogeneity_reduction = if (is.finite(root_obj$total) && root_obj$total > 0 && is.finite(terminal_total)) {
      1 - terminal_total / root_obj$total
    } else {
      NA_real_
    },
    n_regions = if (!is.null(regions_dt)) nrow(regions_dt) else 0L,
    n_splits = if (!is.null(splits_dt)) nrow(splits_dt) else 0L,
    class_label = class_chr,
    method = method_chr
  )

  list(
    features = features,
    split_candidates = split_candidates,
    class_label = class_chr,
    method = method_chr,
    grids = grids,
    regions = regions_dt,
    assignments = assignments,
    curves = curves_dt,
    global_curves = global_curves,
    local_curves = local_dt,
    splits = splits_dt,
    leaf_feature_losses = leaf_loss_dt,
    feature_metrics = feature_metrics,
    total_metrics = total_metrics
  )
}

.autoiml_gadget_feature_view = function(gadget_multi, feature) {
  if (is.null(gadget_multi) || is.null(gadget_multi$curves)) {
    return(NULL)
  }
  feature_sel = as.character(feature)[1L]
  curves = data.table::as.data.table(gadget_multi$curves)[feature == feature_sel]
  if (nrow(curves) == 0L) {
    return(NULL)
  }
  out_curves = data.table::copy(curves)
  out_curves[, y := y_region]

  list(
    feature = feature_sel,
    grid = sort(unique(out_curves$x)),
    regions = gadget_multi$regions,
    assignments = gadget_multi$assignments,
    curves = out_curves,
    global_curves = if (!is.null(gadget_multi$global_curves)) data.table::as.data.table(gadget_multi$global_curves)[feature == feature_sel] else NULL,
    local_curves = if (!is.null(gadget_multi$local_curves)) data.table::as.data.table(gadget_multi$local_curves)[feature == feature_sel] else NULL,
    class_label = gadget_multi$class_label,
    method = gadget_multi$method
  )
}

.autoiml_gadget_risk_screen = function(
  task,
  model,
  X,
  features,
  class_label = NULL,
  grid_n = 10L,
  grid_type = "quantile",
  ice_center = "mean"
) {
  out = lapply(features, function(f) {
    pdice = tryCatch(
      .autoiml_pdp_ice_1d(
        task = task,
        model = model,
        X = X,
        feature = f,
        grid_n = grid_n,
        grid_type = grid_type,
        class_labels = class_label,
        ice_keep_n = 0L,
        ice_center = ice_center
      ),
      error = function(e) NULL
    )
    if (is.null(pdice)) {
      return(NULL)
    }
    key = if (inherits(task, "TaskClassif")) as.character(class_label)[1L] else "response"
    if (is.na(key) || !nzchar(key)) key = names(pdice$pred_mats_centered)[1L]
    M = pdice$pred_mats_centered[[key]]
    if (is.null(M)) {
      M = pdice$pred_mats_centered[[1L]]
    }
    if (is.null(M) || !is.matrix(M)) {
      return(NULL)
    }
    raw_loss = .autoiml_gadget_loss(M)
    denom = nrow(M) * ncol(M)
    risk = if (is.finite(raw_loss) && denom > 0L) raw_loss / denom else NA_real_
    data.table::data.table(
      class_label = if (inherits(task, "TaskClassif")) as.character(class_label)[1L] else "regression",
      feature = f,
      observed_risk = as.numeric(risk),
      raw_loss = as.numeric(raw_loss),
      grid_n = ncol(M),
      sample_n = nrow(M)
    )
  })
  data.table::rbindlist(out, fill = TRUE)
}

.autoiml_make_permuted_task = function(task, seed = NULL) {
  if (!is.null(seed)) set.seed(as.integer(seed))
  target = task$target_names[1L]
  cols = unique(c(task$feature_names, target))
  dt = data.table::as.data.table(task$data(cols = cols))
  dt[[target]] = sample(dt[[target]], size = nrow(dt), replace = FALSE)

  if (inherits(task, "TaskClassif")) {
    out = mlr3::TaskClassif$new(
      id = paste0(task$id, "_pint_perm"),
      backend = dt,
      target = target
    )
    pos = task$positive
    if (!is.null(pos) && pos %in% out$class_names) {
      out$positive = pos
    }
    return(out)
  }

  if (inherits(task, "TaskRegr")) {
    return(mlr3::TaskRegr$new(
      id = paste0(task$id, "_pint_perm"),
      backend = dt,
      target = target
    ))
  }

  stop("Unsupported task type for PINT.", call. = FALSE)
}

.autoiml_gadget_pint = function(
  task,
  model,
  learner,
  X,
  features,
  class_label = NULL,
  permutations = 19L,
  alpha = 0.05,
  grid_n = 10L,
  grid_type = "quantile",
  ice_center = "mean",
  seed = 1L
) {
  features = unique(as.character(features))
  features = intersect(features, names(X))
  if (length(features) == 0L) {
    return(list(screen = NULL, null = NULL, messages = "PINT skipped: no eligible numeric features."))
  }

  observed = .autoiml_gadget_risk_screen(
    task = task,
    model = model,
    X = X,
    features = features,
    class_label = class_label,
    grid_n = grid_n,
    grid_type = grid_type,
    ice_center = ice_center
  )
  if (is.null(observed) || nrow(observed) == 0L) {
    return(list(screen = NULL, null = NULL, messages = "PINT skipped: observed risks could not be computed."))
  }

  observed[, `:=`(
    null_quantile = NA_real_,
    p_value = NA_real_,
    pint_interaction = NA,
    alpha = as.numeric(alpha),
    permutations = as.integer(permutations)
  )]

  B = as.integer(permutations)
  if (B < 1L || is.null(learner) || !inherits(learner, "Learner")) {
    return(list(screen = observed, null = NULL, messages = "PINT observed risks computed; null screen skipped."))
  }

  null_list = list()
  messages = character()
  for (b in seq_len(B)) {
    ptask = tryCatch(.autoiml_make_permuted_task(task, seed = seed + b), error = function(e) e)
    if (inherits(ptask, "error")) {
      messages = c(messages, sprintf("PINT permutation %d task failed: %s", b, conditionMessage(ptask)))
      next
    }

    lrn = learner$clone(deep = TRUE)
    if (inherits(ptask, "TaskClassif") && "prob" %in% lrn$predict_types) {
      lrn$predict_type = "prob"
    }

    ok_train = tryCatch({
      lrn$train(ptask)
      TRUE
    }, error = function(e) {
      messages <<- c(messages, sprintf("PINT permutation %d training failed: %s", b, conditionMessage(e)))
      FALSE
    })
    if (!isTRUE(ok_train)) next

    risk_b = tryCatch(
      .autoiml_gadget_risk_screen(
        task = ptask,
        model = lrn,
        X = X,
        features = features,
        class_label = class_label,
        grid_n = grid_n,
        grid_type = grid_type,
        ice_center = ice_center
      ),
      error = function(e) {
        messages <<- c(messages, sprintf("PINT permutation %d risk failed: %s", b, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(risk_b) && nrow(risk_b) > 0L) {
      risk_b[, permutation := b]
      data.table::setnames(risk_b, "observed_risk", "null_risk")
      null_list[[length(null_list) + 1L]] = risk_b
    }
  }

  null_dt = if (length(null_list) > 0L) data.table::rbindlist(null_list, fill = TRUE) else NULL
  if (!is.null(null_dt) && nrow(null_dt) > 0L) {
    null_sum = null_dt[is.finite(null_risk), .(
      null_quantile = as.numeric(stats::quantile(null_risk, probs = 1 - alpha, na.rm = TRUE, type = 1)),
      null_mean = mean(null_risk, na.rm = TRUE),
      null_sd = stats::sd(null_risk, na.rm = TRUE),
      null_n = .N
    ), by = feature]
    observed = merge(observed, null_sum, by = "feature", all.x = TRUE, suffixes = c("", ".from_null"))
    if ("null_quantile.from_null" %in% names(observed)) {
      observed[, null_quantile := null_quantile.from_null]
      observed[, null_quantile.from_null := NULL]
    }
    observed[, p_value := vapply(seq_len(.N), function(i) {
      vals = null_dt[feature == observed$feature[i] & is.finite(null_risk), null_risk]
      if (length(vals) == 0L || !is.finite(observed$observed_risk[i])) {
        return(NA_real_)
      }
      (1 + sum(vals >= observed$observed_risk[i], na.rm = TRUE)) / (length(vals) + 1)
    }, numeric(1L))]
    observed[, pint_interaction := is.finite(observed_risk) & is.finite(null_quantile) & observed_risk > null_quantile]
  }

  observed[, `:=`(
    alpha = as.numeric(alpha),
    permutations = as.integer(B),
    grid_type = as.character(grid_type)
  )]

  list(screen = observed, null = null_dt, messages = unique(messages))
}
