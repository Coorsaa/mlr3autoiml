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
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  gr = .autoiml_get_gate_result(result, "G6")
  if (is.null(gr)) {
    return(NULL)
  }

  dt = gr$artifacts$alt_learner_performance
  if (is.null(dt) || nrow(dt) == 0L) {
    return(NULL)
  }

  dt = data.table::as.data.table(dt)
  measure_id = unique(dt$measure_id)[1L]
  minimize = tryCatch(mlr3::msr(measure_id)$minimize, error = function(e) TRUE)

  rash = gr$artifacts$rashomon_set
  if (!is.null(rash) && nrow(rash) > 0L) {
    dt[, in_rashomon := learner_id %in% rash$learner_id]
  } else {
    dt[, in_rashomon := FALSE]
  }

  ord = if (minimize) order(dt$mean, decreasing = FALSE) else order(dt$mean, decreasing = TRUE)
  dt[, learner_id := factor(learner_id, levels = dt$learner_id[ord])]

  ggplot2::ggplot(dt, ggplot2::aes(x = mean, y = learner_id)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_low, xmax = ci_high)) +
    ggplot2::geom_point(ggplot2::aes(shape = in_rashomon), size = 2) +
    ggplot2::labs(
      title = "Gate 6: Alternative learner performance",
      x = sprintf("%s (mean ± ~95%% CI)", measure_id),
      y = "Learner"
    )
}

.autoiml_plot_g6_group_performance = function(result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
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

  ggplot2::ggplot(dt, ggplot2::aes(x = group, y = .data[[val_col]])) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Gate 6: Group performance heterogeneity",
      x = "Group",
      y = val_col
    ) +
    ggplot2::coord_flip()
}

.autoiml_plot_g6_rashomon_importance = function(result, top_n = 15L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
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

  ggplot2::ggplot(imp, ggplot2::aes(x = importance, y = feature)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~learner_id, scales = "free_x") +
    ggplot2::labs(
      title = "Gate 6: Rashomon explanation dispersion (permutation importance)",
      x = "Permutation importance (Δ in primary measure; positive = worse after permutation)",
      y = "Feature"
    )
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
