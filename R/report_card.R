# FILE: R/report_card.R

#' @title AutoIML report card
#'
#' @description
#' Create a compact, audit-trail style report card (one row per gate) from an
#' [AutoIMLResult].
#'
#' The report card includes the gate status, a short summary, and the achieved
#' **claim-scoped Interpretation Readiness Levels (IRL)**.
#'
#' @param x ([AutoIMLResult] | [AutoIML])\cr
#'   A completed result or an AutoIML runner.
#'
#' @return A [data.table::data.table] with one row per gate.
#' @export
report_card = function(x) {
  res = if (inherits(x, "AutoIML")) x$result else x
  if (!inherits(res, "AutoIMLResult")) {
    stop("report_card() expects an AutoIML or AutoIMLResult.", call. = FALSE)
  }

  gates = res$gate_results %||% list()
  if (length(gates) == 0L) {
    return(data.table::data.table())
  }

  dt = data.table::rbindlist(
    lapply(gates, function(gr) {
      data.table::data.table(
        gate_id = gr$gate_id,
        gate_name = gr$gate_name,
        pdr = gr$pdr %||% NA_character_,
        status = gr$status %||% NA_character_,
        summary = gr$summary %||% NA_character_
      )
    }),
    fill = TRUE
  )

  # Attach IRL fields (repeat across rows for convenience)
  irl = res$irl
  if (is.list(irl)) {
    dt[, irl_overall := irl$overall %||% NA_character_]
    dt[, irl_global := irl$global %||% NA_character_]
    dt[, irl_local := irl$local %||% NA_character_]
    dt[, irl_decision := irl$decision %||% NA_character_]
  } else {
    dt[, irl_overall := as.character(irl)]
    dt[, irl_global := NA_character_]
    dt[, irl_local := NA_character_]
    dt[, irl_decision := NA_character_]
  }

  dt[, purpose := res$purpose %||% NA_character_]
  dt[, quick_start := isTRUE(res$quick_start)]

  # If Gate 0 exists, attach claim semantics / stakes as convenience columns.
  g0a = gates[["G0A"]]
  g0b = gates[["G0B"]]
  if (!is.null(g0a) && inherits(g0a$metrics, "data.table") && nrow(g0a$metrics) >= 1L) {
    m0 = g0a$metrics[1L]
    dt[, semantics := m0$semantics %||% NA_character_]
    dt[, stakes := m0$stakes %||% NA_character_]
    dt[, claim_global := m0$claim_global %||% NA]
    dt[, claim_local := m0$claim_local %||% NA]
    dt[, claim_decision := m0$claim_decision %||% NA]
  }

  dt[]
}
