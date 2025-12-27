#' Gate
#'
#' Abstract base class for diagnostic gates.
#'
#' Gates operate on a context list that includes:
#' \itemize{
#'   \item \code{task}: \code{mlr3::Task}
#'   \item \code{learner}: \code{mlr3::Learner} (cloned for safety)
#'   \item \code{resampling}: \code{mlr3::Resampling}
#'   \item \code{rr}: \code{mlr3::ResampleResult} (if already computed)
#'   \item \code{final_model}: trained learner (optional)
#'   \item \code{pred}: merged \code{mlr3::Prediction} object (optional)
#'   \item \code{purpose}: scalar purpose
#'   \item \code{quick_start}: logical
#'   \item \code{seed}: integer seed (optional)
#' }
#'
#' @keywords internal
#' @noRd
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
