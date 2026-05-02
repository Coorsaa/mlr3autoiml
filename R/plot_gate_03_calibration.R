# FILE: R/plot_gate_03_calibration.R

#' @title Gate 3 Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helpers for Gate 3 (Calibration and Decision Utility).
#'
#' These functions visualize:
#' \itemize{
#'   \item Reliability (calibration) curve with bootstrap 95\% CI bands
#'   \item Decision Curve Analysis (DCA) with net benefit, bootstrap 95\% CI,
#'     and treat-all / treat-none baselines
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

  if (!all(c("x_mid", "y_mean") %in% names(rel))) {
    return(NULL)
  }

  pal = .autoiml_plot_palette()
  has_ci = all(c("y_ci_low", "y_ci_high") %in% names(rel))

  xy_max = max(max(rel$x_mid, na.rm = TRUE), max(rel$y_mean, na.rm = TRUE), 0.5, na.rm = TRUE)
  xy_max = min(xy_max * 1.05, 1.0)

  p = ggplot2::ggplot(rel, ggplot2::aes(x = x_mid, y = y_mean)) +
    ggplot2::geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", alpha = 0.5, color = pal$reference[["neutral"]]
    )

  # Add bootstrap CI ribbon if available
  if (has_ci) {
    p = p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_ci_low, ymax = y_ci_high),
      fill = pal$metric[["primary"]], alpha = 0.18
    )
  }

  p = p +
    ggplot2::geom_line(color = pal$metric[["primary"]], linewidth = 1) +
    ggplot2::geom_point(
      ggplot2::aes(size = n),
      color = pal$metric[["primary"]], alpha = 0.8
    ) +
    ggplot2::scale_size_continuous(range = c(1.5, 4), guide = "none") +
    ggplot2::coord_equal(xlim = c(0, xy_max), ylim = c(0, xy_max), expand = FALSE) +
    ggplot2::labs(
      title = if (has_ci) {
        "G3: Reliability (calibration) curve (\u00b1 95% bootstrap CI)"
      } else {
        "G3: Reliability (calibration) curve"
      },
      x     = "Mean predicted probability",
      y     = "Observed event fraction"
    ) +
    .autoiml_theme_iml()

  p
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
  has_ci = all(c("nb_ci_low", "nb_ci_high") %in% names(dca))

  # Decision range for shading
  dr = gr$artifacts$decision_range
  thr_min = if (!is.null(dr) && is.finite(dr$thr_min %??% NA_real_)) dr$thr_min else NULL
  thr_max = if (!is.null(dr) && is.finite(dr$thr_max %??% NA_real_)) dr$thr_max else NULL

  dca[, threshold_pct := threshold * 100]

  # y limits based on the model curve (+ CI) only — treat-all may be clipped
  model_nb = dca$net_benefit
  if (has_ci) model_nb = c(model_nb, dca$nb_ci_low, dca$nb_ci_high)
  model_nb = model_nb[is.finite(model_nb)]
  y_range = range(c(model_nb, 0), na.rm = TRUE)
  y_pad = max(0.005, diff(y_range) * 0.10)
  y_lims = c(y_range[1L] - y_pad, y_range[2L] + y_pad)

  x_range = c(0, 100)

  p = ggplot2::ggplot(dca, ggplot2::aes(x = threshold_pct, y = net_benefit))

  # Decision-range shading (under everything)
  if (!is.null(thr_min) && !is.null(thr_max)) {
    p = p + ggplot2::annotate(
      "rect",
      xmin = thr_min * 100, xmax = thr_max * 100,
      ymin = -Inf, ymax = Inf,
      alpha = 0.08, fill = pal$surface[["threshold"]]
    )
  }

  # Treat-none baseline (NB = 0) -- shown as a labeled line in the legend
  p = p + ggplot2::geom_line(
    data = data.table::data.table(
      threshold_pct = c(0, 100),
      net_benefit = c(0, 0)
    ),
    mapping = ggplot2::aes(x = threshold_pct, y = net_benefit, linetype = "Treat none"),
    color = pal$reference[["neutral"]], inherit.aes = FALSE
  )

  # Bootstrap CI ribbon if available
  if (has_ci) {
    p = p + ggplot2::geom_ribbon(
      data = dca,
      mapping = ggplot2::aes(x = threshold_pct, ymin = nb_ci_low, ymax = nb_ci_high),
      inherit.aes = FALSE,
      fill = pal$metric[["primary"]], alpha = 0.15
    )
  }

  # Model net benefit line
  p = p + ggplot2::geom_line(
    color = pal$metric[["primary"]], linewidth = 1,
    ggplot2::aes(linetype = "Model")
  )

  # Treat-all baseline
  if ("nb_treat_all" %in% names(dca)) {
    p = p + ggplot2::geom_line(
      ggplot2::aes(y = nb_treat_all, linetype = "Treat all"),
      color = pal$reference[["neutral"]]
    )
  }

  p = p +
    ggplot2::scale_linetype_manual(
      values = c(Model = "solid", "Treat all" = "dashed", "Treat none" = "dotted")
    ) +
    ggplot2::coord_cartesian(xlim = x_range, ylim = y_lims, expand = FALSE) +
    ggplot2::labs(
      title    = if (has_ci) {
        "G3: Decision curve analysis (\u00b1 95% bootstrap CI)"
      } else {
        "G3: Decision curve analysis"
      },
      x        = "Threshold probability (%)",
      y        = "Net benefit",
      linetype = NULL
    ) +
    .autoiml_theme_iml() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(t = 5.5, r = 12, b = 5.5, l = 5.5, unit = "pt")
    )

  p
}
