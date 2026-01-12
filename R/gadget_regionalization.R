# FILE: R/gadget_regionalization.R

#' @title GADGET-style Regionalization for Gate 2
#'
#' @description
#' Internal lightweight GADGET-style regionalization used by Gate 2 to identify
#' regions of heterogeneous feature effects. This implementation avoids heavy
#' dependencies and uses centered ICE matrices.
#'
#' The regionalization recursively partitions the feature space to minimize
#' within-region variance of effect curves, producing interpretable subgroup-specific
#' effect summaries.
#'
#' @name gadget_regionalization
#' @keywords internal
NULL

.autoiml_gadget_loss = function(pred_mat) {
  if (is.null(pred_mat) || nrow(pred_mat) == 0L) {
    return(NA_real_)
  }
  mu = colMeans(pred_mat, na.rm = TRUE)
  res = pred_mat - matrix(mu, nrow = nrow(pred_mat), ncol = length(mu), byrow = TRUE)
  sum(res^2, na.rm = TRUE)
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
        right = setdiff(idx, left)

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
        right = setdiff(idx, left)

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
      rule_left = sprintf("%s \u2264 %.4g", sp$feature, sp$threshold)
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
