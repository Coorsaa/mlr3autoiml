# FILE: R/gate2_viz.R

#' @title Gate 2 tables
#'
#' @description
#' Extract key Gate 2 (structure) tables and recommendations from an AutoIML run.
#'
#' Gate 2 assesses whether global effect summaries are likely to be misleading
#' under feature dependence and/or heterogeneity. It computes global effect
#' curves (PDP + ICE and ALE) and lightweight interaction diagnostics.
#'
#' The returned object is intended for programmatic reporting (tables); plotting
#' is performed via [AutoIML] `$plot(type = ...)`.
#'
#' @param x ([AutoIML], [AutoIMLResult], [GateResult])\cr
#'   Object containing a Gate 2 result. If a [GateResult] is passed, it must be
#'   the Gate 2 result (`gate_id == "G2"`).
#'
#' @param top_n (`integer(1)`)
#'   Number of rows to return for the top lists (ICE spread, H-statistics).
#'
#' @return A `list()` containing (when available):
#' * `metrics`: Gate 2 metrics table (including recommended method).
#' * `recommendation`: Structured recommendation list.
#' * `max_cor_pair`: Most correlated numeric feature pair (proxy for dependence).
#' * `ice_spread_top`: Top ICE spread features (heterogeneity proxy).
#' * `hstats_top`: Top interaction diagnostics (Friedman--Popescu H).
#'
#' @export

gate2_tables = function(x, top_n = 10L) {
  top_n = as.integer(top_n)

  gr = .autoiml_get_gate_result(x, "G2")
  if (is.null(gr)) {
    stop("Gate 2 results not found. Did you run AutoIML$run()?", call. = FALSE)
  }

  a = gr$artifacts %||% list()
  m = gr$metrics

  rec = a$recommendation
  rec_dt = NULL
  if (is.list(rec) && length(rec) > 0L) {
    rec_dt = data.table::as.data.table(rec)
  }

  ice_spread_top = NULL
  if (!is.null(a$ice_spread) && nrow(a$ice_spread) > 0L) {
    ice_spread_top = a$ice_spread[order(-ice_sd_mean)][seq_len(min(top_n, .N))]
  }

  hstats_top = NULL
  if (!is.null(a$hstats) && nrow(a$hstats) > 0L) {
    hstats_top = a$hstats[is.finite(hstat)][order(-hstat)][seq_len(min(top_n, .N))]
  }

  list(
    metrics = m,
    recommendation = rec_dt,
    max_cor_pair = a$max_cor_pair,
    ice_spread_top = ice_spread_top,
    hstats_top = hstats_top
  )
}
