# FILE: R/gate0_viz.R

#' @title Extract Gate 0 (claim + measurement readiness) tables
#'
#' @description
#' Helper to extract the normalized claim specification produced by Gate 0A
#' and the measurement readiness summary produced by Gate 0B.
#'
#' @param x ([AutoIML] | [AutoIMLResult])
#'   An AutoIML runner or a completed result.
#'
#' @return `list()` with:
#' * `claim_card`: one-row `data.table` summary (Gate 0A).
#' * `claim`: full normalized claim specification list (Gate 0A).
#' * `measurement_card`: one-row `data.table` summary (Gate 0B; may be NULL).
#' * `missingness`: per-feature missingness table (Gate 0B; may be NULL).
#'
#' @export
gate0_tables = function(x) {
  res = if (inherits(x, "AutoIML")) x$result else x
  if (!inherits(res, "AutoIMLResult")) {
    stop("gate0_tables() expects an AutoIML or AutoIMLResult.", call. = FALSE)
  }

  g0a = res$gate_results[["G0A"]]
  g0b = res$gate_results[["G0B"]]

  claim_card = NULL
  claim = NULL
  if (!is.null(g0a)) {
    a = g0a$artifacts %||% list()
    claim_card = g0a$metrics
    claim = a$claim %||% NULL
  }

  measurement_card = NULL
  missingness = NULL
  if (!is.null(g0b)) {
    a = g0b$artifacts %||% list()
    measurement_card = g0b$metrics
    missingness = a$missingness %||% NULL
  }

  list(
    claim_card = claim_card,
    claim = claim,
    measurement_card = measurement_card,
    missingness = missingness
  )
}
