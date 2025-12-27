# FILE: R/plot_shap_global.R
#
# Global SHAP plots:
# - Beeswarm (per-feature distribution across sampled rows)
# - Mean(|SHAP|) (global importance)
#
# Internal helpers used by AutoIML$plot(type = ...)

NULL

.autoiml_shap_sample_dt = function(auto,
  rows = NULL,
  n_rows = 200L,
  class_label = NULL,
  sample_size = 50L,
  background_n = 200L,
  seed = NULL
) {
  if (!inherits(auto, "AutoIML")) {
    stop("Expected an AutoIML object.", call. = FALSE)
  }
  if (is.null(auto$result)) stop("No result yet; call $run() first.", call. = FALSE)

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.", call. = FALSE)
  }

  ctx = auto$ctx
  task = ctx$task
  model = ctx$final_model
  if (is.null(model)) stop("No trained final model found. Run $run() first.", call. = FALSE)

  feats = task$feature_names

  seed_use = seed
  if (is.null(seed_use)) seed_use = ctx$seed
  if (is.null(seed_use)) seed_use = auto$seed
  if (is.null(seed_use)) seed_use = 1L
  seed_use = as.integer(seed_use)

  # Select class label (single-class SHAP for efficiency)
  cls = NULL
  if (inherits(task, "TaskClassif")) {
    if (is.null(class_label)) {
      cls = task$positive
      if (is.null(cls)) cls = task$class_names[[1L]]
    } else {
      cls = as.character(class_label)
    }
  }

  # Caching only when rows are deterministically sampled from seed
  cacheable = is.null(rows)
  if (cacheable) {
    key = paste0(
      "shap_global|task=", task$id,
      "|learner=", auto$learner$id,
      "|cls=", if (is.null(cls)) "__regr__" else cls,
      "|n_rows=", as.integer(n_rows),
      "|B=", as.integer(sample_size),
      "|bg=", as.integer(background_n),
      "|seed=", seed_use
    )
    if (!is.null(ctx$shap_cache_key) && identical(ctx$shap_cache_key, key) &&
      !is.null(ctx$shap_cache_dt) && data.table::is.data.table(ctx$shap_cache_dt)) {
      return(ctx$shap_cache_dt)
    }
  }

  set.seed(seed_use)

  if (is.null(rows)) {
    n_rows = as.integer(n_rows)
    n_rows = min(n_rows, task$nrow)
    rows = sample(task$row_ids, size = n_rows)
  } else {
    rows = as.integer(rows)
  }

  background_n = as.integer(background_n)
  background_n = min(background_n, task$nrow)
  bg_rows = sample(task$row_ids, size = background_n)

  X_bg = task$data(rows = bg_rows, cols = feats)
  X_bg = data.table::as.data.table(X_bg)

  X_rows = task$data(rows = rows, cols = feats)
  X_rows = data.table::as.data.table(X_rows)

  out_list = vector("list", length(rows))

  # Use distinct seeds per row for MC stability without coupling
  base_seed = seed_use

  for (i in seq_along(rows)) {
    x_i = X_rows[i, , drop = FALSE]

    sh = .autoiml_shapley_iml(
      task = task,
      model = model,
      x_interest = x_i,
      background = X_bg,
      sample_size = as.integer(sample_size),
      seed = as.integer(base_seed + i),
      class_labels = cls
    )

    sh = data.table::as.data.table(sh)
    sh[, row_id := rows[[i]]]
    out_list[[i]] = sh
  }

  dt = data.table::rbindlist(out_list, fill = TRUE)

  if (cacheable) {
    ctx$shap_cache_key = key
    ctx$shap_cache_dt = dt
  }

  dt[]
}

.autoiml_plot_shap_importance = function(auto,
  class_label = NULL,
  rows = NULL,
  n_rows = 200L,
  sample_size = 50L,
  background_n = 200L,
  top_n = 20L,
  seed = NULL
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Plotting requires package 'ggplot2'. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.", call. = FALSE)
  }

  dt = .autoiml_shap_sample_dt(
    auto = auto,
    rows = rows,
    n_rows = n_rows,
    class_label = class_label,
    sample_size = sample_size,
    background_n = background_n,
    seed = seed
  )

  dt = data.table::as.data.table(dt)

  # If class_label is present in dt, restrict to it (should already be single-class)
  if (!all(is.na(dt$class_label))) {
    if (!is.null(class_label)) {
      dt = dt[class_label == as.character(class_label)]
    } else {
      # pick first class label
      u = unique(dt$class_label)
      u = u[!is.na(u)]
      if (length(u) > 0L) dt = dt[class_label == u[[1L]]]
    }
  }

  imp = dt[, .(
    mean_abs_phi = mean(abs(phi), na.rm = TRUE),
    mean_phi = mean(phi, na.rm = TRUE)
  ), by = feature][order(-mean_abs_phi)]

  top_n = as.integer(top_n)
  if (nrow(imp) > top_n) imp = imp[seq_len(top_n)]

  imp[, feature := factor(feature, levels = rev(feature))]

  title = "Global SHAP importance (mean |phi|)"
  if (!all(is.na(dt$class_label))) {
    cl = unique(dt$class_label)
    cl = cl[!is.na(cl)]
    if (length(cl) > 0L) title = paste0(title, " — class: ", cl[[1L]])
  }

  ggplot2::ggplot(imp, ggplot2::aes(x = feature, y = mean_abs_phi)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = NULL, y = "mean(|SHAP|)") +
    ggplot2::theme_minimal()
}

.autoiml_plot_shap_beeswarm = function(auto,
  class_label = NULL,
  rows = NULL,
  n_rows = 200L,
  sample_size = 50L,
  background_n = 200L,
  top_n = 20L,
  seed = NULL,
  jitter_height = 0.25,
  alpha = 0.6,
  point_size = 1.2
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Plotting requires package 'ggplot2'. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.", call. = FALSE)
  }

  dt = .autoiml_shap_sample_dt(
    auto = auto,
    rows = rows,
    n_rows = n_rows,
    class_label = class_label,
    sample_size = sample_size,
    background_n = background_n,
    seed = seed
  )

  dt = data.table::as.data.table(dt)

  # Restrict to one class label if present
  if (!all(is.na(dt$class_label))) {
    if (!is.null(class_label)) {
      dt = dt[class_label == as.character(class_label)]
    } else {
      u = unique(dt$class_label)
      u = u[!is.na(u)]
      if (length(u) > 0L) dt = dt[class_label == u[[1L]]]
    }
  }

  imp = dt[, .(mean_abs_phi = mean(abs(phi), na.rm = TRUE)), by = feature][order(-mean_abs_phi)]
  top_n = as.integer(top_n)
  if (nrow(imp) > top_n) imp = imp[seq_len(top_n)]

  keep_feats = imp$feature
  dt = dt[feature %in% keep_feats]

  # order features by importance (top at top)
  lev = rev(keep_feats)
  dt[, feature_f := factor(feature, levels = lev)]

  # numeric-ish feature value scaling per feature for coloring
  dt[, value_num := suppressWarnings(as.numeric(feature_value))]
  dt[, value_scaled := NA_real_]
  dt[is.finite(value_num), value_scaled := {
    r = range(value_num, finite = TRUE)
    den = r[2] - r[1]
    if (!is.finite(den) || den < 1e-12) rep(0.5, .N) else (value_num - r[1]) / den
  }, by = feature]

  title = "SHAP beeswarm"
  if (!all(is.na(dt$class_label))) {
    cl = unique(dt$class_label)
    cl = cl[!is.na(cl)]
    if (length(cl) > 0L) title = paste0(title, " — class: ", cl[[1L]])
  }

  use_color = any(is.finite(dt$value_scaled))

  if (use_color) {
    ggplot2::ggplot(dt, ggplot2::aes(x = phi, y = feature_f, color = value_scaled)) +
      ggplot2::geom_point(
        alpha = alpha,
        size = point_size,
        position = ggplot2::position_jitter(height = jitter_height, width = 0)
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = 2) +
      ggplot2::labs(title = title, x = "SHAP value (phi)", y = NULL, color = "value (scaled)") +
      ggplot2::theme_minimal()
  } else {
    ggplot2::ggplot(dt, ggplot2::aes(x = phi, y = feature_f)) +
      ggplot2::geom_point(
        alpha = alpha,
        size = point_size,
        position = ggplot2::position_jitter(height = jitter_height, width = 0)
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = 2) +
      ggplot2::labs(title = title, x = "SHAP value (phi)", y = NULL) +
      ggplot2::theme_minimal()
  }
}
