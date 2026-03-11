# FILE: R/IEL.R

#' @title Interpretation Evidence Level (IEL) Computation
#'
#' @description
#' Functions for computing claim-scoped Interpretation Evidence Levels (IEL)
#' from gate outcomes and deriving conservative claim-scope statements.
#'
#' @details
#' IEL provides a structured way to summarize the evidentiary basis for different
#' types of interpretation claims:
#' \itemize{
#'   \item \strong{IEL-0}: Exploratory only; insufficient evidence for reporting claims.
#'   \item \strong{IEL-1}: Cautious insight with basic diagnostics.
#'   \item \strong{IEL-2}: Robust reporting level with multiplicity/transport checks.
#'   \item \strong{IEL-3}: High-stakes / deployment-grade with a fuller audit trail.
#' }
#'
#' IEL-3 is intentionally strict: it is only assigned when a high-stakes context is
#' declared (e.g., stakes = "high" or purpose in decision/deployment settings) and
#' human-factors evidence (Gate 7B) is available in addition to scope-specific
#' technical evidence.
#'
#' IEL is intentionally \emph{claim-scoped}: evidence for global summaries may differ
#' from evidence for case-level explanations or decision-support use.
#'
#' @name IEL
NULL

#' @rdname IEL
#' @title Compute Interpretation Evidence Levels (IEL) from Gate Outcomes
#'
#' @description
#' Map gate outcomes to \strong{Interpretation Evidence Levels (IEL)}.
#'
#' IEL is \strong{claim-scoped}:
#' \itemize{
#'   \item `global`: evidence for global/descriptive summaries (effects, importance).
#'   \item `local`: evidence for case-level explanations.
#'   \item `decision`: evidence for threshold/utility-aware decision support.
#' }
#'
#' The function returns a named list with IELs per claim scope and an `overall` IEL
#' computed conservatively as the minimum across the \strong{requested} claim scopes
#' (derived from Gate 0A).
#'
#' For each requested scope, IEL-3 requires both high-stakes context and successful
#' Gate 7B (human-factors evidence); otherwise the maximum attainable level is IEL-2.
#'
#' @param gates (`list()`)
#'   List of [GateResult] objects (typically G0A/G0B, G1--G6, G7A/G7B).
#'
#' @return `list()` with elements `overall`, `global`, `local`, `decision`, and `requested`.
#' @export
#' @seealso [claim_scope_from_iel()] for human-readable scope statements.
iel_from_gates = function(gates) {
  gate_map = setNames(gates, vapply(gates, function(g) g$gate_id, character(1L)))

  claim0 = NULL
  if (!is.null(gate_map[["G0A"]])) {
    claim0 = gate_map[["G0A"]]$artifacts$claim %||% NULL
  }

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

  stakes = tolower(as.character((claim0$stakes %||% "medium")[1L]))
  purpose = as.character((claim0$purpose %||% "exploratory")[1L])
  high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))

  iel_rules = .autoiml_iel_rules()
  vr = .autoiml_validate_iel_rules(iel_rules)
  if (!isTRUE(vr$ok)) {
    stop("Invalid IEL rules: ", paste(vr$errors, collapse = " | "), call. = FALSE)
  }

  scopes_all = c("global", "local", "decision")
  scope_iels = setNames(rep("IEL-0", length(scopes_all)), scopes_all)
  justification = list()

  for (scope in scopes_all) {
    if (!scope %in% requested) {
      justification[[scope]] = data.table::data.table(
        rule_id = NA_character_,
        level = "IEL-0",
        scope = scope,
        matched = FALSE,
        reason = "scope_not_requested"
      )
      next
    }

    rules_scope = iel_rules$rules[vapply(iel_rules$rules, function(r) identical(as.character(r$scope), scope), logical(1L))]
    rules_scope = rules_scope[order(vapply(rules_scope, function(r) .autoiml_iel_to_int(r$level), integer(1L)))]

    rule_rows = list()
    matched_levels = character(0)

    for (i in seq_along(rules_scope)) {
      r = rules_scope[[i]]
      chk = .autoiml_eval_iel_rule(r, gate_map = gate_map, high_stakes = high_stakes)
      if (isTRUE(chk$matched)) matched_levels = c(matched_levels, as.character(r$level))

      rule_rows[[i]] = data.table::data.table(
        rule_id = as.character(r$id),
        level = as.character(r$level),
        scope = scope,
        matched = isTRUE(chk$matched),
        reason = as.character(chk$reason)
      )
    }

    if (length(matched_levels) > 0L) {
      scope_iels[[scope]] = matched_levels[which.max(vapply(matched_levels, .autoiml_iel_to_int, integer(1L)))]
    }

    justification[[scope]] = data.table::rbindlist(rule_rows, fill = TRUE)
  }

  iels_req = unname(scope_iels[requested])

  list(
    overall = .autoiml_min_iel(iels_req),
    global = scope_iels[["global"]],
    local = scope_iels[["local"]],
    decision = scope_iels[["decision"]],
    requested = requested,
    requested_flags = scopes_req,
    iel_justification = justification,
    iel_rule_source = iel_rules$`_path` %||% NA_character_
  )
}

#' @rdname IEL
#' @title Create Conservative Claim-Scope Statements from Claim-Scoped IEL
#'
#' @description
#' Returns short, human-readable statements describing what kinds of interpretation
#' claims are defensible, given achieved IELs and the declared purpose.
#'
#' @param iel (`list()`)
#'   Claim-scoped IEL list produced by [iel_from_gates()].
#' @param purpose (`character(1)`)
#'   One of `"exploratory"`, `"global_insight"`, `"decision_support"`, `"deployment"`.
#'
#' @return `list()` with elements `overall`, `global`, `local`, `decision`, and `requested`.
#' @export
#' @seealso [iel_from_gates()] for computing IEL from gate results.
claim_scope_from_iel = function(iel, purpose = "exploratory") {
  purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))

  if (!is.list(iel)) {
    stop("claim_scope_from_iel(): `iel` must be a list returned by iel_from_gates().", call. = FALSE)
  }

  iel_global = iel$global %||% "IEL-0"
  iel_local = iel$local %||% "IEL-0"
  iel_decision = iel$decision %||% "IEL-0"
  requested = iel$requested %||% c("global")

  scope_global = switch(
    iel_global,
    "IEL-0" = "Exploratory only: do not report global effects beyond sanity checks; avoid causal language.",
    "IEL-1" = "Cautious global insight: describe global structure on observed support; report stability and subgroup diagnostics; avoid causal language.",
    "IEL-2" = "Global insight (publication-grade): add multiplicity/transport diagnostics where relevant; avoid causal language and overgeneralization.",
    "IEL-3" = "High-stakes / deployment-grade global reporting: include transport/multiplicity checks, subgroup audits, human-factors evaluation (as applicable), monitoring, and governance."
  )

  scope_local = switch(
    iel_local,
    "IEL-0" = "Avoid case-level explanations: local attributions may be misleading without faithfulness and stability diagnostics.",
    "IEL-1" = "Limited local explanations with faithfulness + stability diagnostics; prefer regionalization when interactions/heterogeneity are present.",
    "IEL-2" = "Local explanations with multiplicity/transport checks and subgroup diagnostics; document limitations and uncertainty.",
    "IEL-3" = "High-stakes / deployment-grade local explanations: include transport/multiplicity checks, subgroup audits, human-factors evaluation (as applicable), monitoring, and governance."
  )

  scope_decision = switch(
    iel_decision,
    "IEL-0" = "Avoid decision recommendations: treat outputs as exploratory and do not use for thresholded actions.",
    "IEL-1" = "Preliminary decision analysis: assess calibration and specify utilities/costs; do not overclaim without faithfulness/stability evidence.",
    "IEL-2" = "Decision support: add faithfulness + stability checks; document thresholds/utilities and guard against automation bias.",
    "IEL-3" = "High-stakes / deployment decision support: include transport/multiplicity checks, subgroup audits, human-factors evaluation, monitoring, and governance."
  )

  overall = iel$overall %||% .autoiml_min_iel(c(iel_global, iel_local, iel_decision))
  scope_overall = paste0(
    "Overall evidence for requested claims (", paste(requested, collapse = ", "), ") is ", overall, "."
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

.autoiml_iel_levels = c("IEL-0", "IEL-1", "IEL-2", "IEL-3")

.autoiml_iel_to_int = function(x) {
  x = as.character(x)[1L]
  m = match(x, .autoiml_iel_levels)
  if (is.na(m)) {
    return(0L)
  }
  as.integer(m - 1L)
}

.autoiml_min_iel = function(x) {
  x = as.character(x)
  x = x[x %in% .autoiml_iel_levels]
  if (length(x) == 0L) {
    return("IEL-0")
  }
  x[which.min(.autoiml_iel_to_int(x))]
}

.autoiml_format_iel = function(iel) {
  if (is.character(iel) && length(iel) == 1L) {
    return(iel)
  }

  if (is.list(iel)) {
    parts = character()
    if (!is.null(iel$overall)) parts <- c(parts, paste0("overall=", iel$overall))
    if (!is.null(iel$global)) parts <- c(parts, paste0("global=", iel$global))
    if (!is.null(iel$local)) parts <- c(parts, paste0("local=", iel$local))
    if (!is.null(iel$decision)) parts <- c(parts, paste0("decision=", iel$decision))
    return(paste(parts, collapse = " | "))
  }

  as.character(iel)
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


# Declarative IEL rule evaluation (Milestone E)
# @keywords internal
.autoiml_eval_iel_rule = function(rule, gate_map, high_stakes = FALSE) {
  rule = .autoiml_as_list(rule)
  req_gates = as.character(unlist(rule$required_gates %||% character(), use.names = FALSE))
  req_status = as.character(unlist(rule$requires_any_status_in %||% c("pass", "warn"), use.names = FALSE))

  cond = .autoiml_as_list(rule$conditions)
  if (length(cond) > 0L && !is.null(cond$high_stakes)) {
    need_hs = isTRUE(cond$high_stakes)
    if (need_hs && !isTRUE(high_stakes)) {
      return(list(matched = FALSE, reason = "high_stakes_condition_not_met"))
    }
  }

  missing_gates = req_gates[!vapply(req_gates, function(g) !is.null(gate_map[[g]]), logical(1L))]
  if (length(missing_gates) > 0L) {
    return(list(matched = FALSE, reason = paste0("missing_gates:", paste(missing_gates, collapse = ","))))
  }

  bad_status = req_gates[!vapply(req_gates, function(g) {
    st = as.character(gate_map[[g]]$status %||% "")
    st %in% req_status
  }, logical(1L))]

  if (length(bad_status) > 0L) {
    return(list(matched = FALSE, reason = paste0("status_mismatch:", paste(bad_status, collapse = ","))))
  }

  list(matched = TRUE, reason = "matched")
}
