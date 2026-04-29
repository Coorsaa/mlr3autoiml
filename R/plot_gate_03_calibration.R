# FILE: R/plot_gate_03_calibration.R

#' @title Gate 3 Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helpers for Gate 3 (Calibration and Decision Utility).
#'
#' These functions visualize:
#' \itemize{
#'   \item Reliability (calibration) curve for the primary model
#'   \item Decision Curve Analysis (DCA) with net benefit and treat-all baseline
#' }
#'
#' @name plot_gate_03_calibration
#' @keywords internal
NULL

.autoiml_plot_g3_calibration = function(result) {
  if (!.autoiml_require_pkg("ggplot2")) {
    cli_abort("Plotting requires package {.pkg ggplot2}. Install it with {.code install.packages('ggplot2')}.")
  }

  gr = .autoiml_get_gate_result(result, "G3")
  if (is.null(gr)) {
    return(NULL)
  }

  rel = gr$artifacts$reliability
  if (is.null(rel) || nrow(rel) == 0L) {
    return(NULL)
  }

  rel = data.table::as.data.table(rel)

  # Reliability curve uses .autoiml_bin_mean output:
  # columns: bin, n, x_mid (mean predicted), y_mean (observed fraction)
  if (!all(c("x_mid", "y_mean") %in% names(rel))) {
    return(NULL)
  }

  pal = .autoiml_plot_palette()

  xy_max = max(max(rel$x_mid, na.rm = TRUE), max(rel$y_mean, na.rm = TRUE), 0.5, na.rm = TRUE)
  xy_max = min(xy_max * 1.05, 1.0)

  ggplot2::ggplot(rel, ggplot2::aes(x = x_mid, y = y_mean)) +
    ggplot2::geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", alpha = 0.5, color = pal$reference[["neutral"]]
    ) +
    ggplot2::geom_line(color = pal$metric[["primary"]], linewidth = 1) +
    ggplot2::geom_point(
      ggplot2::aes(size = n),
      color = pal$metric[["primary"]], alpha = 0.8
    ) +
    ggplot2::scale_size_continuous(range = c(1.5, 4), guide = "none") +
    ggplot2::coord_equal(xlim = c(0, xy_max), ylim = c(0, xy_max), expand = FALSE) +
    ggplot2::labs(
      title = "G3: Reliability (calibration) curve",
      x     = "Mean predicted probability",
      y     = "Observed event fraction"
    ) +
    .autoiml_theme_iml()
}

.autoiml_plot_g3_dca = function(result) {
  if (!.autoiml_require_pkg("ggplot2")) {
    cli_abort("Plotting requires package {.pkg ggplot2}. Install it with {.code install.packages('ggplot2')}.")
  }

  gr = .autoiml_get_gate_result(result, "G3")
  if (is.null(gr)) {
    return(NULL)
  }

  dca = gr$artifacts$dca
  if (is.null(dca) || nrow(dca) == 0L) {
    return(NULL)
  }

  dca = data.table::as.data.table(dca)
  if (!all(c("threshold", "net_benefit") %in% names(dca))) {
    return(NULL)
  }

  pal = .autoiml_plot_palette()

  # Decision range for shading
  dr = gr$artifacts$decision_range
  thr_min = if (!is.null(dr) && is.finite(dr$thr_min %??% NA_real_)) dr$thr_min else NULL
  thr_max = if (!is.null(dr) && is.finite(dr$thr_max %??% NA_real_)) dr$thr_max else NULL

  dca[, threshold_pct := threshold * 100]

  # Dynamic y limits: pad around observed range
  all_nb = c(dca$net_benefit, if ("nb_treat_all" %in% names(dca)) dca$nb_treat_all, 0)
  all_nb = all_nb[is.finite(all_nb)]
  y_range = range(all_nb, na.rm = TRUE)
  y_pad = max(0.005, diff(y_range) * 0.08)
  y_lims = c(y_range[1L] - y_pad, y_range[2L] + y_pad)

  x_range = range(dca$threshold_pct, na.rm = TRUE)

  p = ggplot2::ggplot(dca, ggplot2::aes(x = threshold_pct, y = net_benefit)) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dotted", color = pal$reference[["neutral"]]
    ) +
    ggplot2::geom_line(
      color = pal$metric[["primary"]], linewidth = 1,
      ggplot2::aes(linetype = "Model")
    )

  if ("nb_treat_all" %in% names(dca)) {
    p = p + ggplot2::geom_line(
      ggplot2::aes(y = nb_treat_all, linetype = "Treat all"),
      color = pal$reference[["neutral"]]
    )
  }

  if (!is.null(thr_min) && !is.null(thr_max)) {
    p = p + ggplot2::annotate(
      "rect",
      xmin = thr_min * 100, xmax = thr_max * 100,
      ymin = -Inf, ymax = Inf,
      alpha = 0.08, fill = pal$surface[["threshold"]]
    )
  }

  p +
    ggplot2::scale_linetype_manual(
      values = c(Model = "solid", "Treat all" = "dashed")
    ) +
    ggplot2::coord_cartesian(xlim = x_range, ylim = y_lims, expand = FALSE) +
    ggplot2::labs(
      title    = "G3: Decision curve analysis",
      x        = "Threshold probability (%)",
      y        = "Net benefit",
      linetype = NULL
    ) +
    .autoiml_theme_iml() +
    ggplot2::theme(legend.position = "bottom")
}
