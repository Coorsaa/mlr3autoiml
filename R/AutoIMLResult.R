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
#' the computed Interpretation Readiness Level (IRL), and any gate artifacts
#' (tables, intermediate objects, model surrogates, etc.) required for
#' reproducibility and reporting.
#'
#' @section Fields:
#' * `task_id` :: `character(1)`\cr
#'   Task identifier (typically `task$id`).
#' * `learner_id` :: `character(1)`\cr
#'   Learner identifier (typically `learner$id`).
#' * `purpose` :: `character(1)`\cr
#'   Declared purpose used for gate selection and claim-scope text.
#' * `quick_start` :: `logical(1)`\cr
#'   Whether the reduced gate set was used.
#' * `irl` :: `list()`\cr
#'   Claim-scoped Interpretation Readiness Levels returned by [irl_from_gates()].
#' * `claim_scope` :: `list()`\cr
#'   Human-readable claim-scope guidance derived from IRL and purpose.
#' * `gate_results` :: `list()`\cr
#'   List of [GateResult] objects in gate order (G0A/G0B, G1..G6, G7A/G7B).
#' * `report` :: [data.table::data.table]\cr
#'   Compact report card table (one row per gate).
#' * `timings` :: `list()` | `NULL`\cr
#'   Optional named list of timing information in seconds.
#' * `extras` :: `list()` | `NULL`\cr
#'   Optional free-form additional outputs.
#'
#' @section Methods:
#' * `report_card()`\cr
#'   `()` -> [data.table::data.table]\cr
#'   Returns the report card table.
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
    task_id = NULL,
    learner_id = NULL,
    purpose = NULL,
    quick_start = NULL,
    irl = NULL,
    claim_scope = NULL,
    gate_results = NULL,
    report = NULL,
    timings = NULL,
    extras = NULL,

    initialize = function(
      task_id,
      learner_id,
      purpose,
      quick_start,
      irl,
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
      self$irl = irl
      self$claim_scope = claim_scope
      self$gate_results = gate_results
      self$report = report
      self$timings = timings
      self$extras = extras
    },

    report_card = function() {
      self$report
    },

    print = function(...) {
      cat(sprintf("<AutoIMLResult task=%s learner=%s>\n", self$task_id, self$learner_id))
      cat(sprintf("  Purpose: %s (quick_start=%s)\n", self$purpose, self$quick_start))
      cat(sprintf("  IRL: %s\n", .autoiml_format_irl(self$irl)))
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
