# FILE: R/gate6_viz.R

#' @title Gate 6 tables
#'
#' @description
#' Extract key Gate 6 (multiplicity and transport) tables from an AutoIML run.
#'
#' Gate 6 assesses:
#' * **Multiplicity (Rashomon effect)**: whether multiple alternative model classes
#'   achieve near-tie performance under the chosen resampling and primary measure.
#' * **Transportability** (lightweight): subgroup performance heterogeneity if the
#'   task has a `group` role set.
#'
#' The returned object is intended for programmatic reporting (tables); plotting
#' is performed via [AutoIML] `$plot(type = ...)`.
#'
#' @param x ([AutoIML], [AutoIMLResult], [GateResult])\cr
#'   Object containing a Gate 6 result. If a [GateResult] is passed, it must be
#'   the Gate 6 result (`gate_id == "G6"`).
#' @param top_n (`integer(1)`)
#'   Number of rows to return for the top feature lists (importance).
#'
#' @return A `list()` containing (when available):
#' * `metrics`: Gate 6 metrics table (multiplicity_flag, transport_flag, measure_id).
#' * `performance`: alternative learner performance summary (mean ± CI).
#' * `rashomon_set`: subset of learners in the Rashomon set.
#' * `importance_top`: top features by mean permutation importance across Rashomon models.
#' * `rankcorr`: pairwise Kendall rank correlations of importance vectors.
#' * `group_performance`: subgroup performance table (if a `group` role is set).
#'
#' @export
gate6_tables = function(x, top_n = 15L) {
  top_n = as.integer(top_n)

  gr = .autoiml_get_gate_result(x, "G6")
  if (is.null(gr)) {
    stop("Gate 6 results not found. Did you run AutoIML$run()?", call. = FALSE)
  }

  arts = gr$artifacts %||% list()

  perf = arts$alt_learner_performance
  rash = arts$rashomon_set
  imp = arts$rashomon_importance
  rankc = arts$rashomon_rankcorr
  grp = arts$group_performance

  imp_top = NULL
  if (!is.null(imp) && nrow(imp) > 0L) {
    imp_top = imp[, .(
      mean_importance = mean(importance, na.rm = TRUE),
      sd_importance = stats::sd(importance, na.rm = TRUE)
    ), by = feature][order(-mean_importance)][seq_len(min(top_n, .N))]
  }

  list(
    metrics = gr$metrics,
    performance = perf,
    rashomon_set = rash,
    importance_top = imp_top,
    rankcorr = rankc,
    group_performance = grp
  )
}

# @keywords internal
.autoiml_gate6_tables = gate6_tables
