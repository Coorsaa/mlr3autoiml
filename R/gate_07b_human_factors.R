# FILE: R/gate_07b_human_factors.R

#' @title Gate 7B: Human-Factors Evaluation (Decision Support)
#'
#' @description
#' Explanations can change users' beliefs and decisions in unintended ways.
#' This gate does not "run" a user study; it validates whether explicit
#' human-factors evidence is provided for high-stakes decision/deployment claims
#' and provides a checklist for expected evidence elements.
#'
#' @section When Triggered:
#' This gate is triggered when explanations are intended for non-technical end users
#' or other user-facing high-stakes stakeholders. Technical audits that remain within
#' the analyst/researcher workflow usually do not require Gate 7B by default.
#'
#' @section Recommended Evaluation Tasks:
#' \itemize{
#'   \item Error detection (identify wrong model outputs)
#'   \item Appropriate reliance / deferral
#'   \item Consistency across cases
#'   \item Robustness to misleading explanations (spurious patterns)
#' }
#'
#' @name Gate7bHumanFactors
#' @keywords internal
NULL

Gate7bHumanFactors = R6::R6Class(
  "Gate7bHumanFactors",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G7B",
        name = "Human-factors evaluation",
        pdr = "R"
      )
    },

    run = function(ctx) {
      claim = ctx$claim %??% list()
      .autoiml_assert_known_names(
        .autoiml_as_list(claim),
        c(
          "purpose", "claims", "semantics", "stakes", "audience",
          "decision_spec", "actionability", "causal_assumptions", "human_factors_evidence",
          "target_population", "setting", "time_horizon", "transport_boundary",
          "intended_use", "intended_non_use", "prohibited_interpretations",
          "decision_policy_rationale"
        ),
        "ctx$claim"
      )
      purpose = claim$purpose %??% ctx$purpose %??% "exploratory"
      purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))

      claims = (claim$claims %??% list())
      decision_claim = isTRUE(claims$decision %??% FALSE)
      local_claim = isTRUE(claims$local %??% FALSE)

      stakes = tolower(as.character(claim$stakes %??% "medium")[1L])
      if (!stakes %in% c("low", "medium", "high")) stakes = "medium"

      audience = as.character(claim$audience %??% "technical")[1L]
      user_facing = .autoiml_is_user_facing_audience(audience)
      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment") || decision_claim)

      evidence = .autoiml_as_list(claim$human_factors_evidence %??% ctx$human_factors_evidence)
      has_evidence_object = length(evidence) > 0L
      required = isTRUE(user_facing && (high_stakes || local_claim || decision_claim))

      if (!isTRUE(required) && !isTRUE(has_evidence_object)) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = "skip",
          summary = "Human-factors evaluation is not in scope for the current technical/non-user-facing claim.",
          metrics = data.table::data.table(required = FALSE, provided = FALSE, stakes = stakes, purpose = purpose, audience = audience, user_facing = user_facing),
          messages = character()
        ))
      }
      .autoiml_assert_known_names(
        evidence,
        c("participants", "task_design", "baseline", "outcomes", "analysis_summary", "artifacts"),
        "ctx$claim$human_factors_evidence"
      )

      has_text = function(x) {
        if (is.null(x)) {
          return(FALSE)
        }
        vals = as.character(unlist(x, use.names = FALSE))
        vals = vals[!is.na(vals)]
        any(nzchar(trimws(vals)))
      }

      req_schema = c("participants", "task_design", "baseline", "outcomes", "analysis_summary")
      missing_schema = req_schema[!vapply(req_schema, function(k) has_text(evidence[[k]]), logical(1L))]
      has_evidence = length(missing_schema) == 0L

      if (!isTRUE(has_evidence)) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = if (isTRUE(required)) "fail" else "warn",
          summary = if (isTRUE(required)) {
            "User-facing high-stakes claims require schema-complete human-factors evidence (participants, task_design, baseline, outcomes, analysis_summary)."
          } else {
            "Human-factors evidence was provided, but the schema is incomplete."
          },
          metrics = data.table::data.table(
            required = required,
            provided = FALSE,
            stakes = stakes,
            purpose = purpose,
            audience = audience,
            user_facing = user_facing,
            missing_schema_n = length(missing_schema)
          ),
          artifacts = list(missing_schema = missing_schema),
          messages = c(
            "Provide a schema-complete human-factors evidence object with participants, task design, baseline, outcomes, and analysis summary."
          )
        ))
      }

      # Evidence provided, return pass and checklist for expected content.
      msgs = c(
        "If explanations will be shown to people (clinicians, participants, decision-makers), evaluate their effects on decision quality and trust calibration.",
        "Recommended evaluation tasks: (i) error detection (identify wrong model outputs), (ii) appropriate reliance / deferral, (iii) consistency across cases, (iv) robustness to misleading explanations (spurious patterns).",
        "Report: participants, tasks, decision policy, ground-truth availability, and whether explanations improved outcomes vs. a no-explanation baseline."
      )

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = "pass",
        summary = if (isTRUE(required)) "Human-factors evidence provided for a user-facing high-stakes claim." else "Human-factors evidence recorded for an optional user-evaluation audit.",
        metrics = data.table::data.table(
          required = required,
          provided = TRUE,
          stakes = stakes,
          purpose = purpose,
          audience = audience,
          user_facing = user_facing,
          missing_schema_n = 0L
        ),
        artifacts = list(checklist = msgs, human_factors_evidence = evidence),
        messages = msgs
      )
    }
  )
)
