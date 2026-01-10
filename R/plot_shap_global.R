# FILE: R/plot_shap_global.R
#
# Global SHAP plots:
# - Beeswarm (per-feature distribution across sampled rows)
# - Mean(|SHAP|) (global importance)
#
# Internal helpers used by AutoIML$plot(type = ...)
NULL

#' @keywords internal
.autoiml_scale_feature_values = function(feature_value, trim = c(0.05, 0.95)) {
  v = feature_value
  if (is.factor(v)) v = as.character(v)
  v = as.character(v)

  # Try numeric scaling first (robust min-max with trimming)
  v_num = suppressWarnings(as.numeric(v))
  ok_num = is.finite(v_num)

  n_non_missing = sum(!is.na(v))
  frac_num = if (n_non_missing > 0L) sum(ok_num) / n_non_missing else 0

  # Treat as numeric if a clear majority parses to numeric
  if (sum(ok_num) >= 3L && frac_num >= 0.6) {
    x_ok = v_num[ok_num]
    if (length(x_ok) < 2L) {
      return(rep(0.5, length(v)))
    }

    trim = as.numeric(trim)
    if (length(trim) != 2L) trim = c(0.05, 0.95)
    trim = pmin(pmax(trim, 0), 1)

    lohi = stats::quantile(x_ok, probs = trim, na.rm = TRUE, type = 7)
    lo = as.numeric(lohi[[1L]])
    hi = as.numeric(lohi[[2L]])

    # fallback if trimming collapses range
    if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
      lo = min(x_ok, na.rm = TRUE)
      hi = max(x_ok, na.rm = TRUE)
    }
    if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
      return(rep(0.5, length(v)))
    }

    x = pmin(pmax(v_num, lo), hi)
    out = (x - lo) / (hi - lo)
    out[!is.finite(out)] = NA_real_
    return(out)
  }

  # Categorical fallback: deterministic mapping to [0, 1]
  lev = sort(unique(v[!is.na(v)]))
  if (length(lev) < 2L) {
    return(rep(0.5, length(v)))
  }

  code = match(v, lev)
  out = (code - 1) / (length(lev) - 1)
  out[is.na(code)] = NA_real_
  out
}

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

.autoiml_shap_global_cached = function(result,
  class_label,
  sample_rows = 200L,
  sample_size = 100L,
  background_n = 200L,
  seed = 1L) {

  # Accept both AutoIML and AutoIMLResult
  ctx = NULL
  if (inherits(result, "AutoIML")) ctx <- result$ctx
  if (inherits(result, "AutoIMLResult")) ctx <- result$extras$ctx

  if (!is.environment(ctx) || is.null(ctx$task) || is.null(ctx$final_model)) {
    stop("SHAP requires a trained model. Run `auto$run()` first.", call. = FALSE)
  }

  # Cache environment lives inside ctx (ctx is an environment, so this is safe & fast)
  if (is.null(ctx$.cache_shap_global) || !is.environment(ctx$.cache_shap_global)) {
    ctx$.cache_shap_global = new.env(parent = emptyenv())
  }
  cache = ctx$.cache_shap_global

  cl = as.character(class_label)[1L]
  key = paste(cl, as.integer(sample_rows), as.integer(sample_size), as.integer(background_n), as.integer(seed), sep = "|")

  if (exists(key, envir = cache, inherits = FALSE)) {
    return(get(key, envir = cache, inherits = FALSE))
  }

  # .autoiml_shap_sample_dt() only needs `$ctx`
  auto_like = if (inherits(result, "AutoIML")) result else list(ctx = ctx)

  dt = .autoiml_shap_sample_dt(
    auto = auto_like,
    class_label = cl,
    n_rows = as.integer(sample_rows),
    sample_size = as.integer(sample_size),
    background_n = as.integer(background_n),
    seed = as.integer(seed)
  )

  assign(key, dt, envir = cache)
  dt
}

.autoiml_plot_shap_beeswarm = function(
  result,
  class_label = NULL,
  sample_rows = 200L,
  background_n = 200L,
  sample_size = 80L,
  top_n = 20L,
  seed = 1L,
  bee_width = 0.60,
  bee_adjust = 1,
  alpha = 0.60,
  point_size = 0.90
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }
  if (!requireNamespace("ggforce", quietly = TRUE)) {
    stop(
      "Package 'ggforce' is required for a true violin-like SHAP beeswarm plot. ",
      "Install it with install.packages('ggforce').",
      call. = FALSE
    )
  }

  shap_dt = .autoiml_shap_global_cached(
    result = result,
    class_label = class_label,
    sample_rows = sample_rows,
    sample_size = sample_size,
    background_n = background_n,
    seed = seed
  )
  if (is.null(shap_dt) || nrow(shap_dt) < 1L) {
    return(NULL)
  }

  shap_dt = data.table::as.data.table(shap_dt)

  if (!is.null(class_label) && "class_label" %in% names(shap_dt)) {
    cl = as.character(class_label)[1L]
    shap_dt = shap_dt[class_label == cl]
  }
  if (nrow(shap_dt) < 1L) {
    return(NULL)
  }

  # top features by mean(|phi|)
  top_n = as.integer(top_n)
  rank_dt = shap_dt[, .(mean_abs_phi = mean(abs(phi), na.rm = TRUE)), by = feature][order(-mean_abs_phi)]
  top_feats = rank_dt$feature[seq_len(min(top_n, nrow(rank_dt)))]

  dt = shap_dt[feature %in% top_feats]
  dt[, feature_f := factor(feature, levels = rev(top_feats))]

  # scale feature values for colouring (iml/shapviz-style)
  dt[, value_scaled := .autoiml_scale_feature_values(feature_value), by = feature]
  dt[!is.finite(value_scaled), value_scaled := NA_real_]

  ttl = if (!is.null(class_label)) sprintf("SHAP beeswarm – %s", as.character(class_label)[1L]) else "SHAP beeswarm"

  p = ggplot2::ggplot(dt, ggplot2::aes(x = phi, y = feature_f, colour = value_scaled)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5, linewidth = 0.3) +
    ggforce::geom_sina(
      orientation = "y",
      scale = "width",
      maxwidth = as.numeric(bee_width),
      adjust = as.numeric(bee_adjust),
      alpha = as.numeric(alpha),
      size = as.numeric(point_size)
    ) +
    ggplot2::scale_colour_gradient(low = "steelblue", high = "firebrick", na.value = "grey70") +
    ggplot2::labs(
      title = ttl,
      x = "SHAP value (phi)",
      y = NULL,
      colour = "Feature value"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )

  # multiclass -> facet if not filtered
  if ("class_label" %in% names(dt) && is.null(class_label)) {
    p = p + ggplot2::facet_wrap(~class_label, scales = "free_x")
  }

  p
}
