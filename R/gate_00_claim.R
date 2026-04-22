# FILE: R/gate_00_claim.R

#' @title Gate 0A: Scope Claim & Use (Claim-First)
#'
#' @description
#' Establishes and validates the \strong{intended interpretive claim(s)} (global / local / decision)
#' and the \strong{semantics} under which explanations are interpreted.
#'
#' This gate makes the "meaning" of explanation artifacts explicit \emph{before} producing them.
#' The framework distinguishes three semantics families:
#'
#' @section Semantics:
#' \describe{
#'   \item{Within-support associational (on-manifold / descriptive)}{Summarize patterns along the empirical
#'     joint distribution (do not interpret as interventions).}
#'   \item{Marginal model query (model-based, non-causal)}{"What would the model predict if we set X_j = x?",
#'     which may break empirical dependence and requires explicit support / off-manifold diagnostics.}
#'   \item{Causal / recourse (intervention / action)}{Requires explicit causal assumptions and an action-set
#'     (mutable features, constraints, costs). This package does \strong{not} implement causal identification;
#'     requesting causal/recourse semantics triggers a hard stop.}
#' }
#'
#' Users can provide a claim specification via `ctx$claim` (or `config = list(claim = ...)`).
#' Conservative defaults are filled based on `purpose`.
#'
#' @name Gate0AClaim
#' @keywords internal
NULL

Gate0AClaim = R6::R6Class(
  "Gate0AClaim",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G0A",
        name = "Scope claim & use",
        pdr = "P"
      )
    },

    run = function(ctx) {
      purpose = ctx$purpose %||% "exploratory"
      purpose = .autoiml_validate_purpose(purpose)

      claim = .autoiml_as_list(ctx$claim)
      .autoiml_assert_known_names(
        claim,
        c(
          "purpose", "claims", "semantics", "stakes", "audience",
          "decision_spec", "actionability", "causal_assumptions", "human_factors_evidence",
          "target_population", "setting", "time_horizon", "transport_boundary",
          "intended_use", "intended_non_use", "prohibited_interpretations",
          "decision_policy_rationale"
        ),
        "ctx$claim"
      )

      msgs = character()
      status = "pass"
      missing_codes = character()

      has_text = function(x) {
        if (is.null(x)) {
          return(FALSE)
        }
        vals = as.character(unlist(x, use.names = FALSE))
        vals = vals[!is.na(vals)]
        any(nzchar(trimws(vals)))
      }

      # ---- defaults implied by purpose ---------------------------------
      default_claims = switch(
        purpose,
        "exploratory" = list(global = TRUE, local = FALSE, decision = FALSE),
        "global_insight" = list(global = TRUE, local = FALSE, decision = FALSE),
        "decision_support" = list(global = TRUE, local = FALSE, decision = TRUE),
        "deployment" = list(global = TRUE, local = FALSE, decision = TRUE)
      )

      claims_in = .autoiml_as_list(claim$claims)

      claims = utils::modifyList(default_claims, claims_in)
      claims$global = isTRUE(claims$global)
      claims$local = isTRUE(claims$local)
      claims$decision = isTRUE(claims$decision)

      # ---- semantics (3-way taxonomy) -----------------------------------
      sem_raw = claim$semantics %||% NULL
      if (is.null(sem_raw) || !nzchar(as.character(sem_raw)[1L])) {
        sem_norm = "within_support"
        msgs = c(msgs, "Claim semantics were not provided; defaulting to 'within_support' (associational / on-manifold / descriptive).")
        status = "warn"
      } else {
        sem_norm = .autoiml_normalize_semantics(sem_raw, default = NA_character_)
        if (is.na(sem_norm)) {
          sem_norm = "within_support"
          status = "warn"
          msgs = c(msgs, sprintf("Unknown ctx$claim$semantics='%s'; defaulting to 'within_support'.", as.character(sem_raw)[1L]))
        }
      }

      # ---- stakes / audience -------------------------------------------
      stakes = claim$stakes %||% NULL
      if (is.null(stakes) || !nzchar(as.character(stakes)[1L])) {
        stakes = switch(
          purpose,
          "exploratory" = "low",
          "global_insight" = "medium",
          "decision_support" = "high",
          "deployment" = "high"
        )
      }
      stakes = tolower(as.character(stakes)[1L])
      if (!stakes %in% c("low", "medium", "high")) {
        msgs = c(msgs, sprintf("Unknown ctx$claim$stakes='%s'; using 'medium'.", stakes))
        stakes = "medium"
        status = "warn"
      }

      audience = claim$audience %||% "technical"
      audience = as.character(audience)[1L]

      # ---- transport + use boundaries ---------------------------------
      target_population = claim$target_population %||% NULL
      setting = claim$setting %||% NULL
      time_horizon = claim$time_horizon %||% NULL
      transport_boundary = claim$transport_boundary %||% NULL

      intended_use = claim$intended_use %||% NULL
      intended_non_use = claim$intended_non_use %||% NULL
      prohibited_interpretations = claim$prohibited_interpretations %||% NULL

      has_transport_scope = has_text(target_population) && has_text(setting) && has_text(time_horizon) && has_text(transport_boundary)
      has_use_boundaries = has_text(intended_use) && has_text(intended_non_use) && has_text(prohibited_interpretations)

      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))

      if (!has_transport_scope) {
        if (isTRUE(high_stakes)) {
          status = "fail"
          missing_codes = c(missing_codes, "transport_scope")
          msgs = c(msgs, "Missing transport scope metadata in ctx$claim (target_population, setting, time_horizon, transport_boundary), required for high-stakes claims.")
        } else if (purpose != "exploratory") {
          status = "warn"
          msgs = c(msgs, "Transport scope metadata is incomplete (target_population, setting, time_horizon, transport_boundary).")
        }
      }

      if (!has_use_boundaries) {
        if (isTRUE(high_stakes)) {
          status = "fail"
          missing_codes = c(missing_codes, "use_boundaries")
          msgs = c(msgs, "Missing use-boundary metadata in ctx$claim (intended_use, intended_non_use, prohibited_interpretations), required for high-stakes claims.")
        } else if (purpose != "exploratory") {
          status = "warn"
          msgs = c(msgs, "Use-boundary metadata is incomplete (intended_use, intended_non_use, prohibited_interpretations).")
        }
      }

      # ---- decision specification (optional) ----------------------------
      decision_spec = .autoiml_as_list(claim$decision_spec)
      .autoiml_assert_known_names(decision_spec, c("thresholds", "costs", "utility", "positive_class"), "ctx$claim$decision_spec")

      decision_policy_rationale = claim$decision_policy_rationale %||% NULL
      has_decision_policy_rationale = has_text(decision_policy_rationale)

      # If a decision claim is requested but thresholds are missing, populate them.
      if (isTRUE(claims$decision) && is.null(decision_spec$thresholds)) {
        decision_spec$thresholds = (ctx$calibration$thresholds %||% seq(0.01, 0.99, by = 0.01))
        msgs = c(msgs, "Decision claim enabled but no decision_spec$thresholds provided; using a default threshold grid. For application-specific decision support, provide justified thresholds and/or utilities/costs.")
        status = "warn"
      }

      # Sanitize thresholds if present.
      if (!is.null(decision_spec$thresholds)) {
        thr = suppressWarnings(as.numeric(decision_spec$thresholds))
        thr = thr[is.finite(thr) & thr > 0 & thr < 1]
        thr = sort(unique(thr))
        if (isTRUE(claims$decision) && length(thr) < 2L) {
          thr = seq(0.01, 0.99, by = 0.01)
          msgs = c(msgs, "Decision thresholds were invalid/degenerate; using default grid 0.01..0.99.")
          status = "warn"
        }
        decision_spec$thresholds = thr
      }

      # Utility / cost specification (optional but expected for decision support)
      has_utility = !is.null(decision_spec$utility) || !is.null(decision_spec$costs)

      if (isTRUE(claims$decision) && !isTRUE(has_utility)) {
        msgs = c(msgs, "Decision claim enabled but no utility/cost specification provided (decision_spec$utility or decision_spec$costs). Gate 3 will report calibration/DCA, but cost-sensitive recommendations require explicit utilities/costs.")
        status = "warn"
      }

      if (isTRUE(claims$decision) && !isTRUE(has_decision_policy_rationale)) {
        if (isTRUE(high_stakes)) {
          status = "fail"
          missing_codes = c(missing_codes, "decision_policy_rationale")
          msgs = c(msgs, "Decision claims in high-stakes contexts require ctx$claim$decision_policy_rationale.")
        } else {
          status = "warn"
          msgs = c(msgs, "Decision claim enabled without decision_policy_rationale; provide policy justification.")
        }
      }

      # ---- actionability constraints (optional; required for recourse) ----
      actionability = .autoiml_as_list(claim$actionability)
      .autoiml_assert_known_names(actionability, c("mutable_features", "immutable_features", "constraints", "costs"), "ctx$claim$actionability")

      has_actionability = !is.null(actionability$mutable_features) || !is.null(actionability$immutable_features) ||
        !is.null(actionability$constraints) || !is.null(actionability$costs)

      # ---- causal assumptions (not implemented; hard stop) ---------------
      causal_assumptions = claim$causal_assumptions %||% NULL
      has_causal = !is.null(causal_assumptions)

      if (identical(sem_norm, "causal_recourse")) {
        # This package does not implement causal identification; we stop here to avoid misuse.
        ctx$hard_stop = TRUE
        ctx$hard_stop_reason = "Causal/recourse semantics requested: provide causal assumptions + an action-set, and use a causal estimation workflow (not implemented in AutoIML)."

        if (!isTRUE(has_causal) || !isTRUE(has_actionability)) {
          status = "fail"
          msgs = c(msgs, "Hard stop: causal/recourse semantics require (i) explicit causal assumptions/specification and (ii) explicit actionability constraints (mutable features, constraints, costs).")
        } else {
          status = "warn"
          msgs = c(msgs, "Causal/recourse semantics declared with assumptions + actionability, but AutoIML does not perform causal identification. Stopping before explanation recommendations.")
        }
      }

      # ---- persist normalized spec -------------------------------------
      claim_norm = utils::modifyList(claim, list(
        purpose = purpose,
        claims = claims,
        semantics = sem_norm,
        stakes = stakes,
        audience = audience,
        target_population = target_population,
        setting = setting,
        time_horizon = time_horizon,
        transport_boundary = transport_boundary,
        intended_use = intended_use,
        intended_non_use = intended_non_use,
        prohibited_interpretations = prohibited_interpretations,
        decision_policy_rationale = decision_policy_rationale,
        decision_spec = decision_spec,
        actionability = actionability
      ))

      ctx$claim = claim_norm

      thr = decision_spec$thresholds %||% numeric()
      thr = suppressWarnings(as.numeric(thr))
      thr = thr[is.finite(thr) & thr > 0 & thr < 1]

      claim_card = data.table::data.table(
        purpose = purpose,
        claim_global = claims$global,
        claim_local = claims$local,
        claim_decision = claims$decision,
        semantics = sem_norm,
        stakes = stakes,
        audience = audience,
        n_thresholds = length(thr),
        thr_min = if (length(thr) > 0L) min(thr) else NA_real_,
        thr_max = if (length(thr) > 0L) max(thr) else NA_real_,
        has_actionability = isTRUE(has_actionability),
        has_utility = isTRUE(has_utility),
        has_transport_scope = isTRUE(has_transport_scope),
        has_use_boundaries = isTRUE(has_use_boundaries),
        has_decision_policy_rationale = isTRUE(has_decision_policy_rationale),
        hard_stop = isTRUE(ctx$hard_stop %||% FALSE)
      )

      summary = sprintf(
        "Claim scope set (global=%s, local=%s, decision=%s) with semantics=%s.",
        claims$global, claims$local, claims$decision, sem_norm
      )

      if (isTRUE(ctx$hard_stop %||% FALSE)) {
        summary = paste0(summary, " Hard stop triggered.")
      }

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = claim_card,
        artifacts = list(
          claim = claim_norm,
          claim_card = claim_card,
          missing_codes = unique(missing_codes)
        ),
        messages = msgs
      )
    }
  )
)
