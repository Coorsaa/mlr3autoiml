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
#'   \item \strong{IEL-2}: Controlled reporting level with claim-matched evidence.
#'   \item \strong{IEL-3}: Robust support for stronger transport / high-stakes claims.
#' }
#'
#' IEL-3 is intentionally strict. Depending on the claim scope, it typically
#' requires stronger robustness evidence (e.g., Gate 6) and, for user-facing
#' high-stakes claims, human-factors evidence (Gate 7B) in addition to the
#' scope-specific technical evidence.
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
#' IEL assignment is claim-scoped and rule-based. Higher levels may require
#' Gate 6 (multiplicity / transport) and, for user-facing high-stakes claims,
#' Gate 7B (human-factors evidence).
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
    claim0 = gate_map[["G0A"]]$artifacts$claim %??% NULL
  }

  scopes_req = list(global = TRUE, local = FALSE, decision = FALSE)
  if (!is.null(claim0$claims)) {
    scopes_req$global = isTRUE(claim0$claims$global %??% TRUE)
    scopes_req$local = isTRUE(claim0$claims$local %??% FALSE)
    scopes_req$decision = isTRUE(claim0$claims$decision %??% FALSE)
  }

  requested = character()
  if (isTRUE(scopes_req$global)) requested <- c(requested, "global")
  if (isTRUE(scopes_req$local)) requested <- c(requested, "local")
  if (isTRUE(scopes_req$decision)) requested <- c(requested, "decision")
  if (length(requested) < 1L) requested <- "global"

  stakes = tolower(as.character((claim0$stakes %??% "medium")[1L]))
  purpose = as.character((claim0$purpose %??% "exploratory")[1L])
  high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))
  audience = as.character((claim0$audience %??% "technical")[1L])
  user_facing = .autoiml_is_user_facing_audience(audience)

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
      chk = .autoiml_eval_iel_rule(r, gate_map = gate_map, high_stakes = high_stakes, user_facing = user_facing)
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
    iel_rule_source = iel_rules$`_path` %??% NA_character_
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

  iel_global = iel$global %??% "IEL-0"
  iel_local = iel$local %??% "IEL-0"
  iel_decision = iel$decision %??% "IEL-0"
  requested = iel$requested %??% c("global")

  scope_global = switch(
    iel_global,
    "IEL-0" = "Exploratory only: do not report global effects beyond debugging or tentative hypothesis generation; avoid causal language.",
    "IEL-1" = "Cautious global insight under stated semantics: report within-support global structure conservatively and add subgroup/measurement audit only when cross-group generalization is implied.",
    "IEL-2" = "Controlled global/regional reporting: report uncertainty, regionalize when interactions are material, and add multiplicity/transport diagnostics when the claim extends across settings.",
    "IEL-3" = "Robust global insight across near-tie models and plausible shifts, with monitoring for explanation drift."
  )

  scope_local = switch(
    iel_local,
    "IEL-0" = "Avoid case-level explanations: local attributions are not currently warranted as part of the claim.",
    "IEL-1" = "Cautious local explanation for representative cases only: document semantics and basic faithfulness/stability, but avoid person-specific guidance.",
    "IEL-2" = "Controlled local/regional support: use high-fidelity regional or local explanations with explicit semantics, interaction checks, and stability evidence.",
    "IEL-3" = "Robust local/regional explanations across near-tie models and plausible shifts; if shown to end users, pair them with human-factors evidence and monitoring."
  )

  scope_decision = switch(
    iel_decision,
    "IEL-0" = "Avoid decision recommendations: treat outputs as exploratory or descriptive only and do not use them for thresholded action.",
    "IEL-1" = "Preliminary decision analysis only: calibration and decision value are assessed in an explicitly stated threshold range, but deployment readiness is not claimed.",
    "IEL-2" = "Controlled decision support (pilot/advisory): decision value is supported in the intended range, subgroup consequences are audited, and actionability limits are explicit.",
    "IEL-3" = "Applied decision support with monitoring: add transport checks, governance artifacts, and user-facing human-factors evidence when explanations are shown to end users."
  )

  overall = iel$overall %??% .autoiml_min_iel(c(iel_global, iel_local, iel_decision))
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
    req = scope$requested %??% c("global")
    lines = c()
    if (!is.null(scope$overall)) lines <- c(lines, scope$overall)
    if ("global" %in% req && !is.null(scope$global)) lines <- c(lines, paste0("Global: ", scope$global))
    if ("local" %in% req && !is.null(scope$local)) lines <- c(lines, paste0("Local: ", scope$local))
    if ("decision" %in% req && !is.null(scope$decision)) lines <- c(lines, paste0("Decision: ", scope$decision))
    return(paste(lines, collapse = "\n"))
  }

  as.character(scope)
}


# Declarative IEL rule evaluation (Milestone E + artifact-key extension, v0.0.5)
#
# Supports two per-rule checks:
#   1. Status check: gate_map[gid]$status must be in `requires_any_status_in`
#      (defaults to c("pass", "warn")).
#   2. Artifact-key check (NEW in 0.0.5): when `requires_artifact_keys` is
#      present in a rule (schema v0.3.0+), the listed keys must exist and be
#      non-empty in gate_map[gid]$artifacts. Empty data.frames, NULL,
#      zero-length atomic vectors, and zero-length lists are rejected.
#
# This ensures the declarative IEL ceiling reflects substantive evidence
# (faithfulness curves, rashomon set, recalibration evaluation, ...) rather
# than merely the gate's pass/warn status.
#
# @keywords internal
.autoiml_eval_iel_rule = function(rule, gate_map, high_stakes = FALSE, user_facing = FALSE) {
  rule = .autoiml_as_list(rule)
  req_gates = as.character(unlist(rule$required_gates %??% character(), use.names = FALSE))
  req_status = as.character(unlist(rule$requires_any_status_in %??% c("pass", "warn"), use.names = FALSE))
  gate_status_req = .autoiml_as_list(rule$gate_status_requirements)
  req_keys = .autoiml_as_list(rule$requires_artifact_keys) # named list: gate_id -> required keys

  # condition: high_stakes / user_facing
  cond = .autoiml_as_list(rule$conditions)
  if (length(cond) > 0L && !is.null(cond$high_stakes)) {
    if (isTRUE(cond$high_stakes) && !isTRUE(high_stakes)) {
      return(list(matched = FALSE, reason = "high_stakes_condition_not_met"))
    }
  }
  if (length(cond) > 0L && !is.null(cond$user_facing)) {
    if (isTRUE(cond$user_facing) && !isTRUE(user_facing)) {
      return(list(matched = FALSE, reason = "user_facing_condition_not_met"))
    }
    if (identical(cond$user_facing, FALSE) && isTRUE(user_facing)) {
      return(list(matched = FALSE, reason = "user_facing_condition_not_met"))
    }
  }

  # presence of required gates
  missing_gates = req_gates[!vapply(req_gates, function(g) !is.null(gate_map[[g]]), logical(1L))]
  if (length(missing_gates) > 0L) {
    return(list(matched = FALSE, reason = paste0("missing_gates:", paste(missing_gates, collapse = ","))))
  }

  # status check
  bad_status = req_gates[!vapply(req_gates, function(g) {
    st = as.character(gate_map[[g]]$status %??% "")
    allowed = as.character(unlist(gate_status_req[[g]] %??% req_status, use.names = FALSE))
    st %in% allowed
  }, logical(1L))]
  if (length(bad_status) > 0L) {
    return(list(matched = FALSE, reason = paste0("status_mismatch:", paste(bad_status, collapse = ","))))
  }

  # Artifact-key check - per-gate, required keys must exist and be non-empty
  if (length(req_keys) > 0L) {
    bad_artifacts = character()
    for (gid in names(req_keys)) {
      gobj = gate_map[[gid]]
      if (is.null(gobj)) {
        bad_artifacts = c(bad_artifacts, paste0(gid, ":<gate_missing>"))
        next
      }
      arts = .autoiml_as_list(gobj$artifacts)
      keys_needed = as.character(unlist(req_keys[[gid]], use.names = FALSE))
      missing_keys = keys_needed[!keys_needed %in% names(arts)]
      empty_keys = vapply(keys_needed, function(k) {
        if (!k %in% names(arts)) {
          return(FALSE)
        }
        !isTRUE(.autoiml_has_evidence_value(arts[[k]]))
      }, logical(1L))
      empty_now = keys_needed[empty_keys]
      bad = unique(c(missing_keys, empty_now))
      if (length(bad) > 0L) {
        bad_artifacts = c(bad_artifacts, paste0(gid, ":", paste(bad, collapse = "+")))
      }
    }
    if (length(bad_artifacts) > 0L) {
      return(list(matched = FALSE,
        reason = paste0("missing_artifact_keys:", paste(bad_artifacts, collapse = ","))))
    }
  }

  list(matched = TRUE, reason = "matched")
}
