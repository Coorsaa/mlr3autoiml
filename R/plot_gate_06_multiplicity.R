# FILE: R/plot_gate_06_multiplicity.R

#' @title Gate 6 Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helpers for Gate 6 (Multiplicity and Transport).
#'
#' These functions visualize:
#' \itemize{
#'   \item Alternative learner performance comparisons with confidence intervals
#'   \item Rashomon set membership
#'   \item Explanation dispersion across Rashomon models (permutation importance)
#'   \item Group performance heterogeneity
#' }
#'
#' @name plot_gate_06_multiplicity
#' @keywords internal
NULL

.autoiml_plot_g6_performance = function(result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli_abort("Plotting requires package {.pkg ggplot2}. Install it with {.code install.packages('ggplot2')}.")
  }

  gr = .autoiml_get_gate_result(result, "G6")
  if (is.null(gr)) {
    return(NULL)
  }

  perf_dt = gr$artifacts$alt_learner_performance
  if (is.null(perf_dt) || nrow(perf_dt) == 0L) {
    return(NULL)
  }

  measure_id = unique(perf_dt$measure_id)[1L]
  minimize = tryCatch(mlr3::msr(measure_id)$minimize, error = function(e) TRUE)

  rash = gr$artifacts$rashomon_set
  if (!is.null(rash) && nrow(rash) > 0L) {
    perf_dt[, in_rashomon := learner_id %in% rash$learner_id]
  } else {
    perf_dt[, in_rashomon := FALSE]
  }

  ord = if (minimize) order(perf_dt$mean, decreasing = FALSE) else order(perf_dt$mean, decreasing = TRUE)
  perf_dt[, learner_id := factor(learner_id, levels = perf_dt$learner_id[ord])]
  pal = .autoiml_plot_palette()

  ggplot2::ggplot(perf_dt, ggplot2::aes(x = mean, y = learner_id)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_low, xmax = ci_high),
      orientation = "y", color = pal$metric[["primary"]]
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = in_rashomon),
      color = pal$metric[["primary"]], size = 2
    ) +
    ggplot2::labs(
      title = "G6: Alternative learner performance",
      x = sprintf("%s (mean +/- ~95%% CI)", measure_id),
      y = NULL
    ) +
    .autoiml_theme_iml()
}

.autoiml_plot_g6_group_performance = function(result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli_abort("Plotting requires package {.pkg ggplot2}. Install it with {.code install.packages('ggplot2')}.")
  }

  gr = .autoiml_get_gate_result(result, "G6")
  if (is.null(gr)) {
    return(NULL)
  }

  dt = gr$artifacts$group_performance
  if (is.null(dt) || nrow(dt) == 0L) {
    return(NULL)
  }

  dt = data.table::as.data.table(dt)

  val_col = setdiff(names(dt), c("group", "n"))[1L]
  if (is.na(val_col)) {
    return(NULL)
  }
  pal = .autoiml_plot_palette()

  ggplot2::ggplot(dt, ggplot2::aes(x = .data[[val_col]], y = group)) +
    ggplot2::geom_col(fill = pal$metric[["primary"]], alpha = 0.85) +
    ggplot2::labs(
      title = "G6: Group performance heterogeneity",
      x = val_col,
      y = NULL
    ) +
    .autoiml_theme_iml()
}

.autoiml_plot_g6_rashomon_importance = function(result, top_n = 15L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli_abort("Plotting requires package {.pkg ggplot2}. Install it with {.code install.packages('ggplot2')}.")
  }

  gr = .autoiml_get_gate_result(result, "G6")
  if (is.null(gr)) {
    return(NULL)
  }

  imp = gr$artifacts$rashomon_importance
  if (is.null(imp) || nrow(imp) == 0L) {
    return(NULL)
  }

  imp = data.table::as.data.table(imp)
  top_n = as.integer(top_n)

  top_feats = imp[, .(m = mean(importance, na.rm = TRUE)), by = feature][order(-m)][seq_len(min(top_n, .N))]$feature
  imp = imp[feature %in% top_feats]
  imp[, feature := factor(feature, levels = rev(top_feats))]
  pal = .autoiml_plot_palette()

  ggplot2::ggplot(imp, ggplot2::aes(x = importance, y = feature)) +
    ggplot2::geom_point(color = pal$metric[["primary"]], size = 1.8) +
    ggplot2::facet_wrap(~learner_id, scales = "free_x") +
    ggplot2::labs(
      title = "G6: Rashomon explanation dispersion (permutation importance)",
      x = "Permutation importance (change in primary measure)",
      y = NULL
    ) +
    .autoiml_theme_iml()
}

.autoiml_plot_g6_pred_multiplicity = function(result, bins = 35L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli_abort("Plotting requires package {.pkg ggplot2}. Install it with {.code install.packages('ggplot2')}.")
  }

  gr = .autoiml_get_gate_result(result, "G6")
  if (is.null(gr)) return(NULL)

  dist = gr$artifacts$pred_range_dist
  if (is.null(dist) || length(dist) < 2L) return(NULL)

  pal = .autoiml_plot_palette()
  med = stats::median(dist, na.rm = TRUE)
  p95 = stats::quantile(dist, probs = 0.95, na.rm = TRUE, names = FALSE)

  ggplot2::ggplot(data.table::data.table(pred_range = dist), ggplot2::aes(x = pred_range)) +
    ggplot2::geom_histogram(fill = pal$metric[["quaternary"]], bins = as.integer(bins), alpha = 0.9) +
    ggplot2::geom_vline(xintercept = med, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = p95, color = pal$reference[["alert"]], linetype = "dashed") +
    ggplot2::labs(
      title = "G6: Predictive multiplicity",
      x = "Per-row prediction range across Rashomon members",
      y = "Count"
    ) +
    .autoiml_theme_iml()
}

.autoiml_plot_g6_loco = function(result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli_abort("Plotting requires package {.pkg ggplot2}. Install it with {.code install.packages('ggplot2')}.")
  }

  gr = .autoiml_get_gate_result(result, "G6")
  if (is.null(gr)) return(NULL)

  sa = gr$artifacts$shift_assessment
  if (is.null(sa) || !identical(sa$mode, "loco")) return(NULL)

  transport_dt = data.table::as.data.table(sa$transport)
  if (is.null(transport_dt) || nrow(transport_dt) == 0L) return(NULL)

  score_col = intersect(c("score", "logloss", "rmse", "auc"), names(transport_dt))[1L]
  if (is.na(score_col)) return(NULL)

  transport_dt = transport_dt[order(get(score_col))]
  measure_id = as.character(sa$measure_id %??% score_col)
  med_score = stats::median(transport_dt[[score_col]], na.rm = TRUE)
  pal = .autoiml_plot_palette()

  ggplot2::ggplot(transport_dt, ggplot2::aes(
    x = reorder(group, .data[[score_col]]), y = .data[[score_col]]
  )) +
    ggplot2::geom_col(fill = pal$metric[["tertiary"]], alpha = 0.9) +
    ggplot2::geom_hline(yintercept = med_score, linetype = "dashed") +
    ggplot2::labs(
      title = "G6: Leave-one-out transport (LOCO)",
      x = NULL,
      y = measure_id
    ) +
    .autoiml_theme_iml() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 70, hjust = 1))
}

.autoiml_plot_g6_summary = function(result, top_n = 12L) {
  p1 = .autoiml_plot_g6_performance(result)
  p2 = .autoiml_plot_g6_rashomon_importance(result, top_n = top_n)

  if (!is.null(p1) && !is.null(p2) && requireNamespace("patchwork", quietly = TRUE)) {
    return(p1 + p2)
  }
  if (!is.null(p1)) {
    return(p1)
  }
  p2
}
