# FILE: R/iml_effects.R

#' @title Model-Agnostic Effect and Attribution Computations
#'
#' @description
#' Internal, model-agnostic effect and attribution computations for Gate 2.
#'
#' These helpers are intentionally lightweight and rely on:
#' \itemize{
#'   \item mlr3 learners for prediction
#'   \item data.table for efficient data manipulation
#' }
#'
#' Where feasible, the numerical definitions follow the reference implementation in
#' the
#' 'iml' package (Christoph Molnar et al.), notably:
#' \itemize{
#'   \item PDP/ICE via intervention-style predictions on a feature grid
#'   \item ALE via accumulated local differences over quantile-based intervals and
#'         centering by the data-weighted average (Apley & Zhu style)
#' }
#'
#' @name iml_effects
#' @keywords internal
NULL

# ---- type casting helpers -------------------------------------------------

.autoiml_cast_like_feature = function(value, ftype, levels = NULL) {
  # Cast a scalar or vector `value` to a type compatible with an mlr3 Task column.
  if (is.null(ftype) || is.na(ftype)) {
    return(value)
  }

  if (ftype == "integer") {
    out = suppressWarnings(as.integer(round(as.numeric(value))))
    return(out)
  }

  if (ftype %in% c("numeric", "double")) {
    return(as.numeric(value))
  }

  if (ftype == "logical") {
    return(as.logical(value))
  }

  if (ftype %in% c("factor", "ordered")) {
    if (is.null(levels)) {
      return(factor(value))
    }
    return(factor(value, levels = levels, ordered = identical(ftype, "ordered")))
  }

  # fallback: do not coerce
  value
}

.autoiml_set_feature_value = function(dt, task, feature, value) {
  # Set feature values in `dt` with correct type.
  ftype = .autoiml_feature_type(task, feature)

  levs = NULL
  if (ftype %in% c("factor", "ordered")) {
    # infer levels from task
    col = task$data(cols = feature)[[feature]]
    levs = levels(col)
  }

  dt[[feature]] = .autoiml_cast_like_feature(value, ftype, levels = levs)
  dt
}

# ---- grids (mirrors iml::get.grid.1D semantics) ---------------------------

.autoiml_grid_1d_iml = function(
  x,
  grid_n = 20L,
  grid_type = c("equidist", "quantile"),
  trim = NULL,
  anchor_value = NULL
) {
  # Close to iml:::get.grid.1D():
  # - equidist: seq(min, max, length.out = grid_n)
  # - quantile: quantile(x, probs = seq(0,1,length.out=grid_n), type = 1)
  # Optional trim:
  # - quantile: probs = seq(trim[1], trim[2], ...)
  # - equidist: range is taken from quantiles at trim bounds
  grid_type = match.arg(grid_type)
  grid_n = as.integer(grid_n)
  if (grid_n < 2L) grid_n = 2L

  x0 = x
  x = x[is.finite(as.numeric(x))]
  if (length(x) < 2L) {
    return(numeric())
  }

  use_trim = !is.null(trim) && length(trim) == 2L &&
    is.finite(trim[1]) && is.finite(trim[2]) &&
    trim[1] >= 0 && trim[2] <= 1 && trim[1] < trim[2]

  if (grid_type == "equidist") {
    if (use_trim) {
      qs = as.numeric(stats::quantile(x, probs = trim, na.rm = TRUE, type = 1))
      rng = suppressWarnings(range(qs))
    } else {
      rng = suppressWarnings(range(x, na.rm = TRUE))
    }
    if (!all(is.finite(rng))) {
      return(numeric())
    }
    g = seq(rng[1], rng[2], length.out = grid_n)
  } else {
    probs = seq(0, 1, length.out = grid_n)
    if (use_trim) probs = seq(trim[1], trim[2], length.out = grid_n)
    g = as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, type = 1))
  }

  g = sort(unique(g))

  # integer feature handling (mirror iml: round grid)
  if (is.integer(x0)) {
    g = sort(unique(as.integer(round(g))))
  }

  if (!is.null(anchor_value) && length(anchor_value) == 1L && is.finite(anchor_value)) {
    av = as.numeric(anchor_value)
    if (is.integer(x0)) av = as.integer(round(av))
    g = sort(unique(c(g, av)))
  }

  g = g[is.finite(as.numeric(g))]
  g
}

# ---- PDP / ICE ------------------------------------------------------------

.autoiml_pdp_ice_1d = function(
  task,
  model,
  X,
  feature,
  grid_n = 20L,
  grid_type = c("equidist", "quantile"),
  class_labels = NULL,
  ice_keep_n = 30L,
  ice_center = c("anchor", "mean", "none"),
  seed = 1L
) {
  grid_type = match.arg(grid_type)
  ice_center = match.arg(ice_center)

  X = data.table::as.data.table(X)
  n = nrow(X)
  if (n < 1L) {
    return(NULL)
  }

  set.seed(as.integer(seed))

  # Build grid in the same spirit as iml (1D intervention grid)
  x = X[[feature]]
  grid = .autoiml_grid_1d_iml(x, grid_n = as.integer(grid_n), grid_type = grid_type)
  if (length(grid) < 2L) {
    return(NULL)
  }

  ftype = .autoiml_feature_type(task, feature)

  # Cast grid to feature type (critical for integer features in mlr3 tasks)
  grid_cast = .autoiml_cast_like_feature(grid, ftype, levels = NULL)
  grid_cast = unique(grid_cast)
  if (length(grid_cast) < 2L) {
    return(NULL)
  }

  # Ensure numeric grid used for x-axis
  grid_x = as.numeric(grid_cast)
  G = length(grid_cast)

  # Replicate X once (n*G rows) and set feature column blockwise (batch intervention)
  X_rep = X[rep.int(seq_len(n), times = G)]
  data.table::set(X_rep, j = feature, value = rep(grid_cast, each = n))

  # Predict ONCE for the whole (n*G) batch
  pred_all = .autoiml_predict_matrix(task, model, X_rep)

  # Determine output columns / classes
  if (inherits(task, "TaskClassif")) {
    if (is.null(class_labels)) {
      class_labels = colnames(pred_all)
    }
    keep = intersect(as.character(class_labels), colnames(pred_all))
    if (length(keep) == 0L) {
      stop("No matching class labels in prediction output.", call. = FALSE)
    }
  } else {
    keep = "response"
  }

  # ICE row subset for storage
  keep_n = min(as.integer(ice_keep_n), n)
  keep_idx = if (keep_n < n) sample.int(n, keep_n) else seq_len(n)

  pd_list = list()
  ice_list = list()
  spread_list = list()
  pred_mats_raw = list()
  pred_mats_centered = list()

  for (k in keep) {
    vec = pred_all[, k]
    M_raw = matrix(vec, nrow = n, ncol = G)

    # PD (raw)
    pd_dt = data.table::data.table(
      class_label = if (inherits(task, "TaskClassif")) as.character(k) else NA_character_,
      feature = feature,
      x = grid_x,
      pd = as.numeric(colMeans(M_raw, na.rm = TRUE))
    )
    pd_list[[k]] = pd_dt

    # Centered ICE matrix for heterogeneity + regionalization
    M_center = M_raw
    if (ice_center == "mean") {
      M_center = M_raw - rowMeans(M_raw, na.rm = TRUE)
    } else if (ice_center == "anchor") {
      M_center = M_raw - M_raw[, 1L]
    }

    pred_mats_raw[[k]] = M_raw
    pred_mats_centered[[k]] = M_center

    sd_grid = apply(M_center, 2, stats::sd, na.rm = TRUE)
    spread_dt = data.table::data.table(
      class_label = if (inherits(task, "TaskClassif")) as.character(k) else NA_character_,
      feature = feature,
      ice_sd_mean = mean(sd_grid, na.rm = TRUE),
      grid_n = G,
      sample_n = n
    )
    spread_list[[k]] = spread_dt

    # ICE curves (RAW; centering only used for metrics/regionalization)
    ice_dt = data.table::data.table(
      class_label = if (inherits(task, "TaskClassif")) as.character(k) else NA_character_,
      feature = feature,
      row_index = rep(keep_idx, each = G),
      x = rep(grid_x, times = length(keep_idx)),
      yhat = as.numeric(t(M_raw[keep_idx, , drop = FALSE]))
    )
    ice_list[[k]] = ice_dt
  }

  list(
    grid = grid_x,
    pd = data.table::rbindlist(pd_list, fill = TRUE),
    ice = data.table::rbindlist(ice_list, fill = TRUE),
    ice_spread = data.table::rbindlist(spread_list, fill = TRUE),
    pred_mats_raw = pred_mats_raw,
    pred_mats_centered = pred_mats_centered
  )
}

# ---- ALE (1D; mirrors iml::calculate.ale.num) -----------------------------

.autoiml_ale_1d_iml = function(
  task,
  model,
  X,
  feature,
  bins = 10L,
  trim = c(0.05, 0.95),
  class_labels = NULL,
  seed = 1L
) {
  X = data.table::as.data.table(X)
  n0 = nrow(X)
  if (n0 < 1L) {
    return(NULL)
  }

  set.seed(as.integer(seed))

  x = X[[feature]]
  ok = is.finite(as.numeric(x))
  if (!any(ok)) {
    return(NULL)
  }

  X_ok = X[ok]
  x_ok = as.numeric(X_ok[[feature]])

  # Quantile-based grid (iml-style ALE binning)
  g = .autoiml_grid_1d_iml(x_ok, grid_n = as.integer(bins) + 1L, grid_type = "quantile", trim = trim)
  if (length(g) < 3L) {
    return(NULL)
  }

  nb = length(g) - 1L
  interval = findInterval(x_ok, g, all.inside = TRUE)

  lower_val = g[interval]
  upper_val = g[interval + 1L]

  ftype = .autoiml_feature_type(task, feature)
  lower_cast = .autoiml_cast_like_feature(lower_val, ftype, levels = NULL)
  upper_cast = .autoiml_cast_like_feature(upper_val, ftype, levels = NULL)

  X_low = data.table::copy(X_ok)
  X_high = data.table::copy(X_ok)

  data.table::set(X_low, j = feature, value = lower_cast)
  data.table::set(X_high, j = feature, value = upper_cast)

  newdata = data.table::rbindlist(list(X_low, X_high), use.names = TRUE)
  pred = .autoiml_predict_matrix(task, model, newdata)

  n = nrow(X_ok)
  pred_low = pred[seq_len(n), , drop = FALSE]
  pred_high = pred[seq_len(n) + n, , drop = FALSE]
  diff = pred_high - pred_low

  if (inherits(task, "TaskClassif")) {
    if (is.null(class_labels)) {
      class_labels = colnames(diff)
    }
    keep = intersect(as.character(class_labels), colnames(diff))
    if (length(keep) == 0L) stop("No matching class labels for ALE output.", call. = FALSE)
  } else {
    keep = "response"
  }

  x_left = g[seq_len(nb)]
  x_right = g[seq_len(nb) + 1L]
  x_mid = (x_left + x_right) / 2

  out_list = list()

  for (k in keep) {
    d = diff[, k]
    dt = data.table::data.table(interval = interval, d = as.numeric(d))

    agg = dt[, .(delta = mean(d, na.rm = TRUE), n_interval = .N), by = interval]
    delta = rep(0, nb)
    n_bin = rep(0L, nb)

    delta[agg$interval] = agg$delta
    n_bin[agg$interval] = agg$n_interval

    ale_raw = cumsum(delta)

    if (sum(n_bin) > 0L) {
      center = sum(ale_raw * n_bin) / sum(n_bin)
    } else {
      center = 0
    }

    ale = ale_raw - center

    out_list[[k]] = data.table::data.table(
      class_label = if (inherits(task, "TaskClassif")) as.character(k) else NA_character_,
      feature = feature,
      x_left = x_left,
      x_right = x_right,
      x = x_mid,
      ale = as.numeric(ale),
      n_interval = n_bin
    )
  }

  data.table::rbindlist(out_list, fill = TRUE)
}

# ---- 2D ALE (interaction surface) -----------------------------------------

.autoiml_ale_2d = function(
  task,
  model,
  X,
  feature1,
  feature2,
  bins        = 8L,
  class_label = NULL
) {
  X = data.table::as.data.table(X)

  x1 = suppressWarnings(as.numeric(X[[feature1]]))
  x2 = suppressWarnings(as.numeric(X[[feature2]]))
  ok = is.finite(x1) & is.finite(x2)
  if (sum(ok) < 10L) return(NULL)

  X_ok  = X[ok]
  x1_ok = x1[ok]
  x2_ok = x2[ok]
  n     = nrow(X_ok)
  bins  = as.integer(bins)

  g1 = unique(stats::quantile(x1_ok, probs = seq(0, 1, length.out = bins + 1L),
    na.rm = TRUE, type = 7))
  g2 = unique(stats::quantile(x2_ok, probs = seq(0, 1, length.out = bins + 1L),
    na.rm = TRUE, type = 7))
  if (length(g1) < 3L || length(g2) < 3L) return(NULL)

  B1   = length(g1) - 1L
  B2   = length(g2) - 1L
  idx1 = findInterval(x1_ok, g1, all.inside = TRUE)
  idx2 = findInterval(x2_ok, g2, all.inside = TRUE)

  ftype1 = .autoiml_feature_type(task, feature1)
  ftype2 = .autoiml_feature_type(task, feature2)

  X_ll = data.table::copy(X_ok)
  X_hl = data.table::copy(X_ok)
  X_lh = data.table::copy(X_ok)
  X_hh = data.table::copy(X_ok)

  data.table::set(X_ll, j = feature1, value = .autoiml_cast_like_feature(g1[idx1],      ftype1, NULL))
  data.table::set(X_ll, j = feature2, value = .autoiml_cast_like_feature(g2[idx2],      ftype2, NULL))
  data.table::set(X_hl, j = feature1, value = .autoiml_cast_like_feature(g1[idx1 + 1L], ftype1, NULL))
  data.table::set(X_hl, j = feature2, value = .autoiml_cast_like_feature(g2[idx2],      ftype2, NULL))
  data.table::set(X_lh, j = feature1, value = .autoiml_cast_like_feature(g1[idx1],      ftype1, NULL))
  data.table::set(X_lh, j = feature2, value = .autoiml_cast_like_feature(g2[idx2 + 1L], ftype2, NULL))
  data.table::set(X_hh, j = feature1, value = .autoiml_cast_like_feature(g1[idx1 + 1L], ftype1, NULL))
  data.table::set(X_hh, j = feature2, value = .autoiml_cast_like_feature(g2[idx2 + 1L], ftype2, NULL))

  batch = data.table::rbindlist(list(X_ll, X_hl, X_lh, X_hh), use.names = TRUE)
  preds = .autoiml_predict_score(model, batch, task, class_of_interest = class_label)

  p_ll = preds[seq_len(n)]
  p_hl = preds[seq_len(n) + n]
  p_lh = preds[seq_len(n) + 2L * n]
  p_hh = preds[seq_len(n) + 3L * n]

  # Second-order difference: interaction effect per observation
  delta = (p_hh - p_lh) - (p_hl - p_ll)

  agg_dt = data.table::data.table(i = idx1, j = idx2, delta = delta)
  agg    = agg_dt[, .(mean_delta = mean(delta, na.rm = TRUE), n_obs = .N), by = c("i", "j")]

  delta_mat = matrix(NA_real_, nrow = B1, ncol = B2)
  n_mat     = matrix(0L,       nrow = B1, ncol = B2)
  for (k in seq_len(nrow(agg))) {
    delta_mat[agg$i[k], agg$j[k]] = agg$mean_delta[k]
    n_mat[agg$i[k],     agg$j[k]] = agg$n_obs[k]
  }

  # Nearest-neighbor imputation for empty cells (iml-style):
  # empty cells inherit the delta of their closest non-empty cell in
  # normalised feature space, ensuring a gap-free surface.
  empty = is.na(delta_mat)
  if (any(empty)) {
    x1_mid_all = (g1[seq_len(B1)] + g1[seq_len(B1) + 1L]) / 2
    x2_mid_all = (g2[seq_len(B2)] + g2[seq_len(B2) + 1L]) / 2
    r1 = diff(range(x1_mid_all)); if (r1 == 0) r1 = 1
    r2 = diff(range(x2_mid_all)); if (r2 == 0) r2 = 1
    x1n = (x1_mid_all - min(x1_mid_all)) / r1
    x2n = (x2_mid_all - min(x2_mid_all)) / r2

    # Flat index arrays: vary row (i) fastest, matching matrix column-major layout
    all_i = rep(seq_len(B1), B2)
    all_j = rep(seq_len(B2), each = B1)
    ax1   = x1n[all_i]
    ax2   = x2n[all_j]
    ev    = as.vector(empty)

    non_empty_k = which(!ev)
    for (ek in which(ev)) {
      d2  = (ax1[non_empty_k] - ax1[ek])^2 + (ax2[non_empty_k] - ax2[ek])^2
      nn  = non_empty_k[which.min(d2)]
      delta_mat[all_i[ek], all_j[ek]] = delta_mat[all_i[nn], all_j[nn]]
    }
  }

  # Replace any residual NAs (all-empty grid — degenerate case) with 0
  delta_mat[is.na(delta_mat)] = 0

  # 2D cumulative sum: first along feature2 (cols), then along feature1 (rows)
  # ale_mat[I,J] = sum_{i<=I, j<=J} delta[i,j]
  ale_mat = t(apply(delta_mat, 1, cumsum))  # cumsum across cols (feature2 dir)
  ale_mat = apply(ale_mat,     2, cumsum)   # cumsum down rows   (feature1 dir)

  # Centre by weighted mean (weights = observed cell counts; empty cells weight 0)
  total_n = sum(n_mat)
  if (total_n > 0L) {
    center  = sum(ale_mat * (n_mat / total_n), na.rm = TRUE)
    ale_mat = ale_mat - center
  }

  x1_mid = (g1[seq_len(B1)] + g1[seq_len(B1) + 1L]) / 2
  x2_mid = (g2[seq_len(B2)] + g2[seq_len(B2) + 1L]) / 2
  cell_i = rep(seq_len(B1), B2)
  cell_j = rep(seq_len(B2), each = B1)

  data.table::data.table(
    feature1    = feature1,
    feature2    = feature2,
    x1_left     = g1[cell_i],
    x1_right    = g1[cell_i + 1L],
    x2_bottom   = g2[cell_j],
    x2_top      = g2[cell_j + 1L],
    x1          = x1_mid[cell_i],
    x2          = x2_mid[cell_j],
    ale2d       = as.vector(ale_mat),
    n_cell      = as.integer(as.vector(n_mat)),
    class_label = if (is.null(class_label)) NA_character_ else as.character(class_label)
  )
}
