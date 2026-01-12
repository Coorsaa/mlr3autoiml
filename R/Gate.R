#' @title Gate Base Class
#'
#' @description
#' Abstract base class for diagnostic gates in the AutoIML workflow.
#'
#' Gates operate on a context environment that includes:
#' \itemize{
#'   \item `task`: [mlr3::Task]
#'   \item `learner`: [mlr3::Learner] (cloned for safety)
#'   \item `resampling`: [mlr3::Resampling]
#'   \item `rr`: [mlr3::ResampleResult] (if already computed)
#'   \item `final_model`: trained learner (optional)
#'   \item `pred`: merged [mlr3::Prediction] object (optional)
#'   \item `purpose`: scalar purpose string
#'   \item `quick_start`: logical flag
#'   \item `seed`: integer seed (optional)
#' }
#'
#' Concrete gate implementations inherit from this class and override the `run()` method.
#'
#' @name Gate
#' @keywords internal
NULL

Gate = R6::R6Class(
  "Gate",
  public = list(
    id = NULL,
    name = NULL,
    pdr = NULL,

    initialize = function(id, name, pdr) {
      self$id = id
      self$name = name
      self$pdr = pdr
    },

    run = function(ctx) {
      stop("Gate$run() must be implemented by subclasses.", call. = FALSE)
    }
  )
)
