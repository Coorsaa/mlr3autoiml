# FILE: R/IRL.R

#' @title Compute Interpretation Readiness Levels (IRL) from gate outcomes
#'
#' @description
#' Map gate outcomes to **Interpretation Readiness Levels (IRL)**.
#'
#' In the revised framework, IRL is **claim-scoped**:
#' * `global`: readiness for global/descriptive summaries (effects, importance).
#' * `local`: readiness for case-level explanations.
#' * `decision`: readiness for threshold/utility-aware decision support.
#'
#' The function returns a named list with IRLs per claim and an `overall` IRL
#' computed conservatively as the minimum across the **requested** claim scopes
#' (derived from Gate 0A).
#'
#' @param gates (`list()`)
#'   List of [GateResult] objects (typically G0A/G0B, G1–G6, G7A/G7B).
#'
#' @return `list()` with elements `overall`, `global`, `local`, `decision`, and `requested`.
#' @export
irl_from_gates = function(gates) {
  gate_map = setNames(gates, vapply(gates, function(g) g$gate_id, character(1L)))

  ok = function(id) !is.null(gate_map[[id]]) && gate_map[[id]]$status %in% c("pass", "warn")
  bad = function(id) !is.null(gate_map[[id]]) && gate_map[[id]]$status %in% c("fail", "error")

  # Gate 0A is the source of claim scope requests.
  claim0 = NULL
  if (!is.null(gate_map[["G0A"]])) {
    claim0 = gate_map[["G0A"]]$artifacts$claim %||% NULL
  }

  # Requested claim scopes (default: global only)
  scopes_req = list(global = TRUE, local = FALSE, decision = FALSE)
  if (!is.null(claim0$claims)) {
    scopes_req$global = isTRUE(claim0$claims$global %||% TRUE)
    scopes_req$local = isTRUE(claim0$claims$local %||% FALSE)
    scopes_req$decision = isTRUE(claim0$claims$decision %||% FALSE)
  }

  requested = character()
  if (isTRUE(scopes_req$global)) requested <- c(requested, "global")
  if (isTRUE(scopes_req$local)) requested <- c(requested, "local")
  if (isTRUE(scopes_req$decision)) requested <- c(requested, "decision")
  if (length(requested) < 1L) requested <- "global"

  # Stakes signal: used to require human-factors evaluation for IRL-3 decision use.
  stakes = tolower(as.character((claim0$stakes %||% "medium")[1L]))
  purpose = as.character((claim0$purpose %||% "exploratory")[1L])
  high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))

  # Base prerequisites (Gate 0 + measurement + predictive preflight)
  base_ok = ok("G0A") && !bad("G0A") && ok("G0B") && !bad("G0B") && ok("G1") && !bad("G1")

  # Subgroup / human-factors evidence
  subgroup_ok = ok("G7A") && !bad("G7A")
  human_ok = ok("G7B") && !bad("G7B")

  req_global = "global" %in% requested
  req_local = "local" %in% requested
  req_decision = "decision" %in% requested

  # --- Global IRL --------------------------------------------------------
  global_irl = "IRL-0"
  if (isTRUE(req_global) && base_ok && ok("G2") && ok("G5") && subgroup_ok) {
    global_irl = "IRL-1"
  }
  if (isTRUE(req_global) && base_ok && ok("G2") && ok("G5") && ok("G6") && subgroup_ok) {
    global_irl = "IRL-2"
  }
  if (isTRUE(req_global) && base_ok && ok("G2") && ok("G5") && ok("G6") && subgroup_ok) {
    # IRL-3 is reserved for deployment-grade usage. We proxy this with an
    # explicit human-factors / use evaluation requirement in high-stakes contexts.
    if (isTRUE(high_stakes)) {
      if (human_ok) global_irl = "IRL-3"
    } else {
      global_irl = "IRL-3"
    }
  }

  # --- Local IRL ---------------------------------------------------------
  local_irl = "IRL-0"
  if (isTRUE(req_local) && base_ok && ok("G2") && ok("G4") && ok("G5") && subgroup_ok) {
    local_irl = "IRL-1"
  }
  if (isTRUE(req_local) && base_ok && ok("G2") && ok("G4") && ok("G5") && ok("G6") && subgroup_ok) {
    local_irl = "IRL-2"
  }
  if (isTRUE(req_local) && base_ok && ok("G2") && ok("G4") && ok("G5") && ok("G6") && subgroup_ok) {
    if (isTRUE(high_stakes)) {
      if (human_ok) local_irl = "IRL-3"
    } else {
      local_irl = "IRL-3"
    }
  }

  # --- Decision-support IRL ---------------------------------------------
  decision_irl = "IRL-0"
  if (isTRUE(req_decision) && base_ok && ok("G3") && subgroup_ok) {
    decision_irl = "IRL-1"
  }
  if (isTRUE(req_decision) && base_ok && ok("G3") && ok("G4") && ok("G5") && subgroup_ok) {
    decision_irl = "IRL-2"
  }
  if (isTRUE(req_decision) && base_ok && ok("G3") && ok("G4") && ok("G5") && ok("G6") && subgroup_ok) {
    if (isTRUE(high_stakes)) {
      if (human_ok) decision_irl = "IRL-3"
    } else {
      decision_irl = "IRL-3"
    }
  }

  irls_req = unname(c(global = global_irl, local = local_irl, decision = decision_irl)[requested])

  list(
    overall = .autoiml_min_irl(irls_req),
    global = global_irl,
    local = local_irl,
    decision = decision_irl,
    requested = requested,
    requested_flags = scopes_req
  )
}

#' @title Create conservative claim-scope statements from claim-scoped IRL
#'
#' @description
#' Returns short, human-readable statements describing what kinds of interpretation
#' claims are defensible, given achieved IRLs and the declared purpose.
#'
#' @param irl (`list()`)
#'   Claim-scoped IRL list produced by [irl_from_gates()].
#' @param purpose (`character(1)`)
#'   One of `"exploratory"`, `"global_insight"`, `"decision_support"`, `"deployment"`.
#' @return `list()`.
#' @export
claim_scope_from_irl = function(irl, purpose = "exploratory") {
  purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))

  if (!is.list(irl)) {
    stop("claim_scope_from_irl(): `irl` must be a list returned by irl_from_gates().", call. = FALSE)
  }

  irl_global = irl$global %||% "IRL-0"
  irl_local = irl$local %||% "IRL-0"
  irl_decision = irl$decision %||% "IRL-0"
  requested = irl$requested %||% c("global")

  scope_global = switch(
    irl_global,
    "IRL-0" = "Exploratory only: do not report global effects beyond sanity checks; avoid causal language.",
    "IRL-1" = "Cautious global insight: describe global structure on observed support; report stability and subgroup diagnostics; avoid causal language.",
    "IRL-2" = "Global insight (publication-grade): add multiplicity/transport diagnostics where relevant; avoid causal language and overgeneralization.",
    "IRL-3" = "Deployment-grade global reporting: include transport/multiplicity checks, subgroup audits, monitoring, and governance."
  )

  scope_local = switch(
    irl_local,
    "IRL-0" = "Avoid case-level explanations: local attributions may be misleading without faithfulness and stability diagnostics.",
    "IRL-1" = "Limited local explanations with faithfulness + stability diagnostics; prefer regionalization when interactions/heterogeneity are present.",
    "IRL-2" = "Local explanations with multiplicity/transport checks and subgroup diagnostics; document limitations and uncertainty.",
    "IRL-3" = "Deployment-grade local explanations: include transport/multiplicity checks, subgroup audits, monitoring, and governance."
  )

  scope_decision = switch(
    irl_decision,
    "IRL-0" = "Avoid decision recommendations: treat outputs as exploratory and do not use for thresholded actions.",
    "IRL-1" = "Preliminary decision analysis: assess calibration and specify utilities/costs; do not overclaim without faithfulness/stability evidence.",
    "IRL-2" = "Decision support: add faithfulness + stability checks; document thresholds/utilities and guard against automation bias.",
    "IRL-3" = "Deployment decision support: include transport/multiplicity checks, subgroup audits, human-factors evaluation, monitoring, and governance."
  )

  overall = irl$overall %||% .autoiml_min_irl(c(irl_global, irl_local, irl_decision))
  scope_overall = paste0(
    "Overall readiness for requested claims (", paste(requested, collapse = ", "), ") is ", overall, "."
  )

  list(
    overall = scope_overall,
    global = scope_global,
    local = scope_local,
    decision = scope_decision,
    requested = requested
  )
}

# ---- Internal helpers -----------------------------------------------------

.autoiml_irl_levels = c("IRL-0", "IRL-1", "IRL-2", "IRL-3")

.autoiml_irl_to_int = function(x) {
  x = as.character(x)[1L]
  m = match(x, .autoiml_irl_levels)
  if (is.na(m)) {
    return(0L)
  }
  as.integer(m - 1L)
}

.autoiml_min_irl = function(x) {
  x = as.character(x)
  x = x[x %in% .autoiml_irl_levels]
  if (length(x) == 0L) {
    return("IRL-0")
  }
  x[which.min(.autoiml_irl_to_int(x))]
}

.autoiml_format_irl = function(irl) {
  if (is.character(irl) && length(irl) == 1L) {
    return(irl)
  }

  if (is.list(irl)) {
    parts = character()
    if (!is.null(irl$overall)) parts <- c(parts, paste0("overall=", irl$overall))
    if (!is.null(irl$global)) parts <- c(parts, paste0("global=", irl$global))
    if (!is.null(irl$local)) parts <- c(parts, paste0("local=", irl$local))
    if (!is.null(irl$decision)) parts <- c(parts, paste0("decision=", irl$decision))
    return(paste(parts, collapse = " | "))
  }

  as.character(irl)
}

.autoiml_format_claim_scope = function(scope) {
  if (is.character(scope) && length(scope) == 1L) {
    return(scope)
  }

  if (is.list(scope)) {
    req = scope$requested %||% c("global")
    lines = c()
    if (!is.null(scope$overall)) lines <- c(lines, scope$overall)
    if ("global" %in% req && !is.null(scope$global)) lines <- c(lines, paste0("Global: ", scope$global))
    if ("local" %in% req && !is.null(scope$local)) lines <- c(lines, paste0("Local: ", scope$local))
    if ("decision" %in% req && !is.null(scope$decision)) lines <- c(lines, paste0("Decision: ", scope$decision))
    return(paste(lines, collapse = "\n"))
  }

  as.character(scope)
}
