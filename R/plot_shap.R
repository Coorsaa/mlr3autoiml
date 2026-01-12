# FILE: R/plot_shap.R

#' @title Local SHAP Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helpers for local (case-level) SHAP visualizations.
#'
#' These functions create waterfall-style plots showing Shapley value contributions
#' for individual predictions, similar to the SHAP library's Python implementation.
#'
#' @name plot_shap
#' @keywords internal
NULL

#' @describeIn plot_shap Create a waterfall plot for local SHAP values
#' @keywords internal
.autoiml_plot_shap_local = function(
  auto,
  row_id,
  class_label = NULL,
  sample_size = 100L,
  background_n = 200L,
  top_n = 12L,
  sort_by_abs = TRUE,
  base_size = 11
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }
  if (!inherits(auto, "AutoIML")) {
    stop("Expected an AutoIML object.", call. = FALSE)
  }
  if (is.null(auto$result)) {
    stop("No result found. Call $run() first.", call. = FALSE)
  }

  task = auto$ctx$task
  model = auto$ctx$final_model
  if (is.null(model)) {
    stop("No trained model found in ctx$final_model. Call $run() first.", call. = FALSE)
  }

  # Compute Shapley contributions (iml-style intervention design)
  shap_dt = auto$shap(
    row_id = row_id,
    class_label = class_label,
    sample_size = sample_size,
    background_n = background_n
  )

  if (is.null(shap_dt) || nrow(shap_dt) < 1L) {
    return(NULL)
  }

  dt = data.table::as.data.table(shap_dt)
  dt = dt[is.finite(phi)]
  if (nrow(dt) < 1L) {
    return(NULL)
  }

  # Pick a single class for plotting (waterfall is inherently 1-output)
  cl = NULL
  if ("class_label" %in% names(dt)) {
    if (!is.null(class_label)) {
      cl = as.character(class_label)[1L]
      dt = dt[class_label == cl]
    } else {
      cl = unique(dt$class_label)[1L]
      dt = dt[class_label == cl]
    }
  }

  if (nrow(dt) < 1L) {
    return(NULL)
  }

  # Order features
  dt[, abs_phi := abs(phi)]
  if (isTRUE(sort_by_abs)) {
    data.table::setorder(dt, -abs_phi)
  } else {
    data.table::setorder(dt, -phi)
  }

  # Keep top_n and aggregate the remainder into "Other"
  top_n = as.integer(top_n)
  top_n = max(1L, min(top_n, nrow(dt)))

  if (top_n < nrow(dt)) {
    other = dt[(top_n + 1L):.N]
    other_phi = sum(other$phi, na.rm = TRUE)

    dt = dt[seq_len(top_n)]
    dt = data.table::rbindlist(
      list(
        dt,
        data.table::data.table(
          class_label = if ("class_label" %in% names(dt)) dt$class_label[1L] else NA_character_,
          feature = "Other",
          feature_value = "",
          phi = other_phi,
          phi_var = NA_real_,
          sample_size = if ("sample_size" %in% names(dt)) dt$sample_size[1L] else as.integer(sample_size)
        )
      ),
      use.names = TRUE,
      fill = TRUE
    )
  }

  # Compute f(x) for this row (same prediction scale as SHAP values)
  X1 = task$data(rows = row_id, cols = task$feature_names)
  pred_mat = .autoiml_predict_matrix(task, model, X1)

  if (inherits(task, "TaskClassif")) {
    if (is.null(cl)) {
      cl = as.character(class_label %||% task$positive %||% task$class_names[1L])[1L]
    }
    if (!cl %in% colnames(pred_mat)) {
      cl = colnames(pred_mat)[1L]
    }
    fx = as.numeric(pred_mat[1L, cl])
  } else {
    fx = as.numeric(pred_mat[1L, 1L])
  }

  # Baseline: E[f(X)] ≈ f(x) - Σ phi  (sampling noise possible)
  phi_total = sum(dt$phi, na.rm = TRUE)
  base_value = fx - phi_total

  # Waterfall geometry (cumulative starts/ends)
  dt[, start := base_value + c(0, cumsum(phi[-.N]))]
  dt[, end := start + phi]
  dt[, sign := ifelse(phi >= 0, "positive", "negative")]

  dt[, feature_value := ifelse(is.na(feature_value), "", as.character(feature_value))]
  dt[, feature_label := ifelse(
    feature == "Other",
    "Other features",
    paste0(feature, " = ", feature_value)
  )]

  # Put largest contributions at the top (SHAP-like)
  dt[, feature_label := factor(feature_label, levels = rev(feature_label))]

  dt[, x_lab := sprintf("%+.3f", phi)]
  dt[, hjust := ifelse(phi >= 0, -0.1, 1.1)]

  ttl = if (!is.null(cl)) sprintf("SHAP waterfall – %s", cl) else "SHAP waterfall"
  subt = sprintf(
    "E[f(X)] ≈ %.4f  |  f(x) = %.4f  |  ΣSHAP = %.4f  |  row_id = %s",
    base_value, fx, phi_total, as.character(row_id)
  )

  ggplot2::ggplot(dt, ggplot2::aes(y = feature_label)) +
    ggplot2::geom_vline(xintercept = base_value, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = fx, linetype = "dotted", alpha = 0.5) +
    ggplot2::geom_segment(
      ggplot2::aes(x = start, xend = end, yend = feature_label, color = sign),
      linewidth = 6,
      lineend = "butt"
    ) +
    ggplot2::geom_point(ggplot2::aes(x = end), size = 1.5, alpha = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(x = end, label = x_lab, hjust = hjust),
      vjust = 0.35,
      size = 3.2
    ) +
    ggplot2::scale_color_manual(
      values = c("negative" = "#1f77b4", "positive" = "#d62728"),
      guide = "none"
    ) +
    ggplot2::labs(
      title = ttl,
      subtitle = subt,
      x = "Model output (prediction scale)",
      y = NULL
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::coord_cartesian(clip = "off") +
    .autoiml_theme_iml(base_size = base_size)
}
