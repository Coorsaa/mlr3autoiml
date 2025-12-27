# FILE: R/IRL.R

#' @title Compute Interpretation Readiness Level (IRL) from gate outcomes
#'
#' @description
#' Map a set of gate outcomes to an Interpretation Readiness Level (IRL).
#'
#' IRL is computed **only from achieved evidence** (gate outcomes), consistent with the
#' paper's definition that levels are cumulative and claims must not exceed the highest level
#' whose evidence requirements are met.
#'
#' The `purpose` argument is accepted for convenience (e.g., downstream claim-scope text),
#' but it does **not** cap the achieved IRL.
#'
#' @param gates (`list()`)\cr
#'   List of [GateResult] objects (typically G1..G7).
#'
#' @return `character(1)` with one of `"IRL-0"`, `"IRL-1"`, `"IRL-2"`, `"IRL-3"`.
#' @export
irl_from_gates = function(gates) {
  # Defensive: allow partial lists, treat missing as "skip"
  g = setNames(gates, vapply(gates, function(x) x$gate_id, character(1)))

  status_of = function(id) {
    x = g[[id]]
    if (is.null(x)) {
      return("skip")
    }
    x$status %||% "skip"
  }

  ok = function(id) status_of(id) %in% c("pass", "warn")
  bad = function(id) status_of(id) %in% c("fail", "error")

  # IRL-0: no predictive adequacy / preflight not passed
  if (!ok("G1") || bad("G1")) {
    return("IRL-0")
  }

  # IRL-1: require Gates 1–3 + basic stability (Gate 5)
  irl1 = ok("G1") && ok("G2") && ok("G3") && ok("G5") &&
    !bad("G2") && !bad("G3") && !bad("G5")

  if (!irl1) {
    return("IRL-0")
  }

  # IRL-2: additionally require faithfulness (Gate 4)
  irl2 = irl1 && ok("G4") && !bad("G4")
  if (!irl2) {
    return("IRL-1")
  }

  # IRL-3: additionally require multiplicity/transport (Gate 6) + human/fairness (Gate 7)
  irl3 = irl2 && ok("G6") && ok("G7") && !bad("G6") && !bad("G7")
  if (irl3) {
    return("IRL-3")
  }

  "IRL-2"
}
