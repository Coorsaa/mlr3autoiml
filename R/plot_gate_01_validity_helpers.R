# FILE: R/plot_gate_01_validity_helpers.R

#' @title Gate 1 Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helpers for Gate 1 (Modeling and Data Validity).
#'
#' These functions visualize cross-validation performance scores and comparisons
#' against baseline models.
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
    # aggregate-only case
    stop("Gate 1 scores table does not contain per-iteration rows.", call. = FALSE)
  }

  # choose a default measure
  measure_cols = setdiff(names(scores), c("task_id", "learner_id", "resampling_id", "iteration", "task", "learner", "resampling", "prediction_test"))
  if (length(measure_cols) == 0L) {
    stop("No measure columns found in Gate 1 scores.", call. = FALSE)
  }

  if (is.null(measure)) {
    measure = measure_cols[1L]
  }
  measure = as.character(measure)
  if (!measure %in% names(scores)) {
    stop(sprintf("Unknown measure '%s'. Available: %s", measure, paste(measure_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  dt = scores[, .(iteration, value = get(measure))]

  p = ggplot2::ggplot(dt, ggplot2::aes(x = iteration, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = sprintf("Gate 1: CV performance (%s)", measure),
      x = "CV iteration",
      y = measure
    )

  if (isTRUE(include_baseline) && !is.null(g1$artifacts$baseline_rr)) {
    base_scores = tryCatch(g1$artifacts$baseline_rr$score(mlr3measures::msr(measure)), error = function(e) NULL)
    if (!is.null(base_scores) && "iteration" %in% names(base_scores)) {
      bdt = data.table::as.data.table(base_scores)[, .(iteration, value = get(measure))]
      p = p + ggplot2::geom_line(data = bdt, ggplot2::aes(x = iteration, y = value), linetype = 2)
    }
  }

  p
}
