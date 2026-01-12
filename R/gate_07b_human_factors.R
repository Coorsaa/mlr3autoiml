# FILE: R/gate_07b_human_factors.R

#' @title Gate 7B: Human-Factors Evaluation (Decision Support)
#'
#' @description
#' Explanations can change users' beliefs and decisions in unintended ways.
#' This gate does not "run" a user study; it flags when human-factors evaluation
#' is warranted and provides a lightweight checklist.
#'
#' @section When Triggered:
#' The gate triggers a warning for high-stakes or decision-support purposes,
#' recommending formal human-factors evaluation.
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
      claim = ctx$claim %||% list()
      purpose = claim$purpose %||% ctx$purpose %||% "exploratory"
      purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))

      claims = (claim$claims %||% list())
      decision_claim = isTRUE(claims$decision %||% FALSE)

      stakes = tolower(as.character(claim$stakes %||% "medium")[1L])
      if (!stakes %in% c("low", "medium", "high")) stakes = "medium"

      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment") || decision_claim)

      if (!isTRUE(high_stakes)) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = "skip",
          summary = "Human-factors evaluation is primarily relevant for decision support / high-stakes use; skipped.",
          metrics = data.table::data.table(required = FALSE, stakes = stakes, purpose = purpose),
          messages = character()
        ))
      }

      # For high-stakes or decision support: warn and provide a checklist.
      msgs = c(
        "If explanations will be shown to people (clinicians, participants, decision-makers), evaluate their effects on decision quality and trust calibration.",
        "Recommended evaluation tasks: (i) error detection (identify wrong model outputs), (ii) appropriate reliance / deferral, (iii) consistency across cases, (iv) robustness to misleading explanations (spurious patterns).",
        "Report: participants, tasks, decision policy, ground-truth availability, and whether explanations improved outcomes vs. a no-explanation baseline."
      )

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = "warn",
        summary = "Human-factors evaluation is warranted for the declared use (decision support / high stakes).",
        metrics = data.table::data.table(required = TRUE, stakes = stakes, purpose = purpose),
        artifacts = list(checklist = msgs),
        messages = msgs
      )
    }
  )
)
