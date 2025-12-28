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


#' @title Create a conservative claim-scope statement from IRL and purpose
#'
#' @description
#' Returns a short, human-readable statement describing what kinds of interpretation
#' claims are defensible, given the achieved IRL and the declared purpose.
#'
#' @param irl (`character(1)`) One of `"IRL-0"`, `"IRL-1"`, `"IRL-2"`, `"IRL-3"`.
#' @param purpose (`character(1)`) One of `"exploratory"`, `"global_insight"`,
#'   `"decision_support"`, `"deployment"`.
#'
#' @return `character(1)`.
#' @export
claim_scope_from_irl = function(irl, purpose = "exploratory") {
  purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))
  irl = match.arg(irl, c("IRL-0", "IRL-1", "IRL-2", "IRL-3"))

  # Map IRL to the *maximum* defensible purpose level.
  max_level = switch(
    irl,
    "IRL-0" = "exploratory",
    "IRL-1" = "global_insight",
    "IRL-2" = "decision_support",
    "IRL-3" = "deployment"
  )

  # Cap declared purpose if it exceeds achieved IRL.
  purpose_eff = purpose
  order = c("exploratory", "global_insight", "decision_support", "deployment")
  if (match(purpose, order) > match(max_level, order)) {
    purpose_eff = max_level
  }

  if (purpose_eff == "exploratory") {
    return("Exploratory only: use for pipeline auditing and hypothesis generation; avoid substantive global/local claims and causal language.")
  }
  if (purpose_eff == "global_insight") {
    return("Global insight: report global effects/importance with diagnostics and uncertainty; avoid causal language and avoid pointwise decision claims.")
  }
  if (purpose_eff == "decision_support") {
    return("Decision support: allow limited case-level guidance with explicit faithfulness + stability diagnostics; report calibration/utility and guard against automation bias.")
  }

  "Deployment: suitable for operational use only with transport checks, subgroup audits, monitoring, and governance; communicate limitations and update mechanisms."
}
