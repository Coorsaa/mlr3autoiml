# FILE: R/AutoIMLResult.R

#' @title AutoIMLResult Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Lightweight, print-friendly container returned by [AutoIML] and [autoiml()].
#'
#' An [AutoIMLResult] stores the executed gate outcomes ([GateResult] objects),
#' the computed Interpretation Evidence Level (IEL), and any gate artifacts
#' (tables, intermediate objects, model surrogates, etc.) required for
#' reproducibility and reporting.
#'
#' @examples
#' \dontrun{
#' # Typically returned by AutoIML$run():
#' result$report_card()
#' }
#'
#' @export
AutoIMLResult = R6::R6Class(
  "AutoIMLResult",
  public = list(
    #' @field task_id (`character(1)`)
    #' Task identifier (typically `task$id`).
    task_id = NULL,

    #' @field learner_id (`character(1)`)
    #' Learner identifier (typically `learner$id`).
    learner_id = NULL,

    #' @field purpose (`character(1)`)
    #' Declared purpose used for gate selection and claim-scope text.
    purpose = NULL,

    #' @field quick_start (`logical(1)`)
    #' Whether the reduced gate set was used.
    quick_start = NULL,

    #' @field iel (`list()`)
    #' Claim-scoped Interpretation Evidence Levels returned by [iel_from_gates()].
    iel = NULL,

    #' @field claim_scope (`list()`)
    #' Human-readable claim-scope guidance derived from IEL and purpose.
    claim_scope = NULL,

    #' @field gate_results (`list()`)
    #' List of [GateResult] objects in gate order (G0A/G0B, G1..G6, G7A/G7B).
    gate_results = NULL,

    #' @field report ([data.table::data.table])
    #' Compact report card table (one row per gate).
    report = NULL,

    #' @field timings (`list()` | `NULL`)
    #' Optional named list of timing information in seconds.
    timings = NULL,

    #' @field extras (`list()` | `NULL`)
    #' Optional free-form additional outputs.
    extras = NULL,

    #' @description
    #' Create a new `AutoIMLResult` object.
    #'
    #' @param task_id (`character(1)`) Task identifier.
    #' @param learner_id (`character(1)`) Learner identifier.
    #' @param purpose (`character(1)`) Declared purpose.
    #' @param quick_start (`logical(1)`) Whether quick_start mode was used.
    #' @param iel (`list()`) Interpretation Evidence Levels.
    #' @param claim_scope (`list()`) Claim-scope guidance.
    #' @param gate_results (`list()`) List of [GateResult] objects.
    #' @param report ([data.table::data.table]) Report card table.
    #' @param timings (`list()` | `NULL`) Optional timing information.
    #' @param extras (`list()`) Optional additional outputs.
    #' @return A new `AutoIMLResult` object.
    initialize = function(
      task_id,
      learner_id,
      purpose,
      quick_start,
      iel,
      claim_scope,
      gate_results,
      report,
      timings,
      extras = list()
    ) {
      self$task_id = task_id
      self$learner_id = learner_id
      self$purpose = purpose
      self$quick_start = quick_start
      self$iel = iel
      self$claim_scope = claim_scope
      self$gate_results = gate_results
      self$report = report
      self$timings = timings
      self$extras = extras
    },

    #' @description
    #' Returns the report card table.
    #' @return A [data.table::data.table] with one row per gate.
    report_card = function() {
      self$report
    },

    #' @description
    #' Returns the extended requirement-level report card.
    #' @return A [data.table::data.table] with one row per framework requirement.
    report_card_extended = function() {
      report_card_extended(self)
    },

    #' @description
    #' Export an audit bundle for reproducibility and reporting.
    #'
    #' @param dir (`character(1)`) Output directory.
    #' @param prefix (`character(1)`) Filename prefix.
    #' @return Named list of exported file paths.
    export_audit_bundle = function(dir = "autoiml_audit_bundle", prefix = "autoiml") {
      export_audit_bundle(self, dir = dir, prefix = prefix)
    },

    #' @description
    #' Return guided next-step recommendations.
    #'
    #' @param max_actions (`integer(1)`) Maximum number of actions.
    #' @return Named list with summary, actions, and recommended plots.
    guide = function(max_actions = 6L) {
      guide_workflow(self, max_actions = max_actions)
    },

    #' @description
    #' Print a summary of the result object.
    #' @param ... Ignored.
    #' @return Invisibly returns `self`.
    print = function(...) {
      cat(sprintf("<AutoIMLResult task=%s learner=%s>\n", self$task_id, self$learner_id))
      cat(sprintf("  Purpose: %s (quick_start=%s)\n", self$purpose, self$quick_start))
      cat(sprintf("  IEL: %s\n", .autoiml_format_iel(self$iel)))
      if (!is.null(self$report) && inherits(self$report, "data.table")) {
        cat("  Gates:\n")
        for (i in seq_len(nrow(self$report))) {
          cat(sprintf("   - %s: %s\n", self$report$gate_id[i], self$report$status[i]))
        }
      }
      cat(sprintf("  Claim scope: %s\n", .autoiml_format_claim_scope(self$claim_scope)))
      invisible(self)
    }
  )
)
