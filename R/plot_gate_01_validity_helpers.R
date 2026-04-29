# FILE: R/plot_gate_01_validity_helpers.R

#' @title Gate 1 Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helpers for Gate 1 (Modeling and Data Validity).
#'
#' Visualizes CV fold-level performance as a boxplot with individual fold points,
#' a featureless baseline reference line, and significance annotations.
#'
#' @name plot_gate_01_validity_helpers
#' @keywords internal
NULL

#' @describeIn plot_gate_01_validity_helpers Plot Gate 1 CV scores
#' @keywords internal
.autoiml_plot_g1_scores = function(result, measure = NULL, include_baseline = TRUE) {
  if (!.autoiml_require_pkg("ggplot2")) {
    stop("Plotting requires package 'ggplot2'. Please install it.", call. = FALSE)
  }

  g1 = .autoiml_get_gate_result(result, "G1")
  if (is.null(g1) || is.null(g1$artifacts$scores)) {
    stop("Gate 1 scores not available.", call. = FALSE)
  }

  scores = data.table::as.data.table(g1$artifacts$scores)
  if (!"iteration" %in% names(scores)) {
    stop("Gate 1 scores table does not contain per-iteration rows.", call. = FALSE)
  }

  measure_cols = setdiff(
    names(scores),
    c("task_id", "learner_id", "resampling_id", "iteration", "task", "learner", "resampling", "prediction_test")
  )
  if (length(measure_cols) == 0L) {
    stop("No measure columns found in Gate 1 scores.", call. = FALSE)
  }
  if (is.null(measure)) measure = measure_cols[1L]
  measure = as.character(measure)
  if (!measure %in% names(scores)) {
    stop(sprintf("Unknown measure '%s'. Available: %s", measure, paste(measure_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  pal = .autoiml_plot_palette()
  model_vals = as.numeric(scores[[measure]])
  model_vals = model_vals[is.finite(model_vals)]

  # Significance label vs featureless (one-sided paired t-test when fold counts match)
  sig_label = NA_character_
  fl_ref = NA_real_
  baseline_scores = g1$artifacts$baseline_scores
  if (isTRUE(include_baseline) && !is.null(baseline_scores) && measure %in% names(baseline_scores)) {
    base_vals = as.numeric(data.table::as.data.table(baseline_scores)[[measure]])
    base_vals = base_vals[is.finite(base_vals)]
    fl_ref = median(base_vals, na.rm = TRUE)
    if (length(model_vals) >= 2L && length(base_vals) >= 2L) {
      minimize = tryCatch(mlr3::msr(measure)$minimize, error = function(e) FALSE)
      alt = if (isTRUE(minimize)) "less" else "greater"
      paired = length(model_vals) == length(base_vals)
      p = tryCatch(
        stats::t.test(model_vals, base_vals, paired = paired, alternative = alt)$p.value,
        error = function(e) NA_real_
      )
      sig_label = if (is.na(p)) "?" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.10) "." else "ns"
    }
  }

  dt = data.table::data.table(x = "model", value = model_vals)
  y_sig = max(model_vals, na.rm = TRUE)

  p = ggplot2::ggplot(dt, ggplot2::aes(x = x, y = value)) +
    ggplot2::geom_boxplot(fill = NA, color = "black", width = 0.4, outlier.shape = NA) +
    ggplot2::geom_jitter(color = pal$metric[["primary"]], width = 0.08, size = 2.5) +
    ggplot2::labs(
      title = sprintf("G1: CV performance (%s)", measure),
      x = NULL,
      y = measure,
      caption = "** p<0.01, * p<0.05, . p<0.10, ns (one-sided paired t-test vs featureless)"
    ) +
    .autoiml_theme_iml() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

  if (is.finite(fl_ref)) {
    p = p + ggplot2::geom_hline(
      yintercept = fl_ref, linetype = "dashed", color = pal$reference[["neutral"]]
    ) +
      ggplot2::annotate("text",
        x = 1.3, y = fl_ref, label = "featureless",
        hjust = 0, vjust = -0.4, size = 3, color = pal$reference[["neutral"]]
      )
  }

  if (!is.na(sig_label)) {
    p = p + ggplot2::annotate("text",
      x = 1, y = y_sig, label = sig_label,
      vjust = -0.5, size = 4.5
    )
  }

  p
}
