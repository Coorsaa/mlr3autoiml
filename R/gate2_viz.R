# FILE: R/gate2_viz.R

#' @title Gate 2 tables
#'
#' @description
#' Extract key Gate 2 (structure) tables and recommendations from an AutoIML run.
#'
#' Gate 2 assesses whether global effect summaries are likely to be misleading
#' under feature dependence and/or heterogeneity. It computes global effect
#' curves (PDP + ICE and ALE), lightweight interaction diagnostics, optional GADGET-style
#' regionalization, optional GADGET-PINT screening, and off-manifold support screening.
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
#' * `pint`: Optional GADGET-PINT permutation interaction screen.
#' * `gadget_regions`: Joint GADGET region table.
#' * `gadget_splits`: GADGET split rules and split gains.
#' * `gadget_feature_metrics`: Feature-wise GADGET heterogeneity reduction.
#' * `support_check_flagged`: Off-manifold support flags for PDP grids (if computed).
#'
#' @export

gate2_tables = function(x, top_n = 10L) {
  top_n = as.integer(top_n)

  gr = .autoiml_get_gate_result(x, "G2")
  if (is.null(gr)) {
    stop("Gate 2 results not found. Did you run AutoIML$run()?", call. = FALSE)
  }

  a = gr$artifacts %??% list()
  m = gr$metrics

  rec = a$recommendation
  rec_dt = NULL
  if (is.list(rec) && length(rec) > 0L) {
    rec_flat = lapply(rec, function(z) {
      if (is.null(z) || length(z) == 0L) {
        return(NA_character_)
      }
      if (length(z) > 1L) {
        return(paste(as.character(z), collapse = ", "))
      }
      z
    })
    rec_dt = data.table::as.data.table(rec_flat)
  }

  ice_spread_top = NULL
  if (!is.null(a$ice_spread) && nrow(a$ice_spread) > 0L) {
    dt = a$ice_spread[order(-ice_sd_mean)]
    ice_spread_top = dt[seq_len(min(top_n, nrow(dt)))]
  }

  hstats_top = NULL
  if (!is.null(a$hstats) && nrow(a$hstats) > 0L) {
    dt = a$hstats[is.finite(hstat)][order(-hstat)]
    hstats_top = dt[seq_len(min(top_n, nrow(dt)))]
  }

  pint_top = NULL
  if (!is.null(a$pint) && nrow(a$pint) > 0L) {
    dt = a$pint[order(-observed_risk)]
    pint_top = dt[seq_len(min(top_n, nrow(dt)))]
  }

  gadget_feature_metrics = NULL
  if (!is.null(a$gadget_feature_metrics) && nrow(a$gadget_feature_metrics) > 0L) {
    dt = a$gadget_feature_metrics[order(-heterogeneity_reduction)]
    gadget_feature_metrics = dt[seq_len(min(top_n, nrow(dt)))]
  }

  support_check_flagged = NULL
  if (!is.null(a$support_check) && nrow(a$support_check) > 0L) {
    dt = a$support_check[flag_off_support %in% TRUE & is.finite(ratio_to_baseline)]
    if (nrow(dt) > 0L) {
      dt = dt[order(-ratio_to_baseline)]
      support_check_flagged = dt[seq_len(min(top_n, nrow(dt)))]
    } else {
      dt = a$support_check[order(-ratio_to_baseline)]
      support_check_flagged = dt[seq_len(min(top_n, nrow(dt)))]
    }
  }

  list(
    metrics = m,
    recommendation = rec_dt,
    max_cor_pair = a$max_cor_pair,
    ice_spread_top = ice_spread_top,
    hstats_top = hstats_top,
    pint = pint_top,
    gadget_regions = a$gadget_regions,
    gadget_splits = a$gadget_splits,
    gadget_feature_metrics = gadget_feature_metrics,
    support_check_flagged = support_check_flagged
  )
}
