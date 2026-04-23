# FILE: R/plot_gate_05_stability.R

#' @title Gate 5 Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helpers for Gate 5 (Stability and Robustness).
#'
#' Visualizes permutation importance confidence intervals across bootstrap
#' resamples, showing the top-\code{top_n} features ordered by mean importance.
#'
#' @name plot_gate_05_stability
#' @keywords internal
NULL

.autoiml_plot_g5_stability = function(result, top_n = 15L) {
  if (!.autoiml_require_pkg("ggplot2")) {
    cli_abort("Plotting requires package {.pkg ggplot2}. Install it with {.code install.packages('ggplot2')}.")
  }

  gr = .autoiml_get_gate_result(result, "G5")
  if (is.null(gr)) {
    return(NULL)
  }

  ci = gr$artifacts$perf_ci
  if (is.null(ci) || nrow(ci) == 0L) {
    return(NULL)
  }

  ci = data.table::as.data.table(ci)
  if (!all(c("feature", "mean", "ci_low", "ci_high") %in% names(ci))) {
    return(NULL)
  }

  top_n = as.integer(top_n)
  dt = ci[order(-mean)][seq_len(min(top_n, .N))]
  dt[, feature := factor(feature, levels = rev(feature))]

  pal = .autoiml_plot_palette()

  ggplot2::ggplot(dt, ggplot2::aes(x = mean, y = feature)) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed", alpha = 0.4,
      color = pal$reference[["neutral"]]
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_low, xmax = ci_high),
      orientation = "y", width = 0.25,
      color = pal$metric[["primary"]], alpha = 0.7
    ) +
    ggplot2::geom_point(
      color = pal$metric[["primary"]], size = 2
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dotted",
      color = pal$reference[["neutral"]], alpha = 0.6
    ) +
    ggplot2::labs(
      title = "G5: Permutation importance (mean \u00b1 95% CI, bootstrap)",
      x     = "Importance (change in primary metric)",
      y     = NULL
    ) +
    .autoiml_theme_iml()
}
