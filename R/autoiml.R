# FILE: R/autoiml.R

#' @title Run an AutoIML audit (convenience wrapper)
#'
#' @description
#' Convenience wrapper that constructs an [AutoIML] object and immediately runs
#' it.
#'
#' Use this function if you want a single-call interface. If you want to inspect
#' or modify configuration prior to running (e.g. `ctx$structure`), instantiate
#' [AutoIML] directly.
#'
#' @param task ([mlr3::Task])\cr
#'   The task to audit.
#' @param learner ([mlr3::Learner])\cr
#'   The learner to audit. For classification, `predict_type = "prob"` is required.
#' @param resampling ([mlr3::Resampling])\cr
#'   Resampling strategy used for Gate 1. Defaults to 5-fold CV.
#' @param purpose (`character(1)`)\cr
#'   Declared purpose; controls which gates run and how strict the claim scope is.
#'   One of `"exploratory"`, `"global_insight"`, `"decision_support"`, `"deployment"`.
#' @param quick_start (`logical(1)`)\cr
#'   If `TRUE`, runs a reduced set of gates for fast iteration.
#' @param seed (`integer(1)`)\cr
#'   Random seed used for reproducible subsampling in computational gates.
#' @param config (`list()`)\cr
#'   Optional nested configuration list merged into the runtime context `ctx`
#'   (e.g. `list(structure = list(sample_n = 200L))`).
#' @param verbose (`logical(1)`)\cr
#'   If `TRUE`, prints a compact run summary after completion.
#'
#' @return [AutoIMLResult].
#' @export
#'
#' @examples
#' library(mlr3)
#' task = tsk("penguins")
#' learner = lrn("classif.rpart", predict_type = "prob")
#' resampling = rsmp("cv", folds = 5)
#'
#' result = autoiml(task, learner, resampling, purpose = "global_insight", seed = 123)
#' result$report_card()
autoiml = function(task,
  learner,
  resampling = mlr3::rsmp("cv", folds = 5),
  purpose = c("exploratory", "global_insight", "decision_support", "deployment"),
  quick_start = TRUE,
  seed = 1L,
  config = list(),
  verbose = FALSE
) {
  purpose = match.arg(purpose)
  seed = as.integer(seed %||% 1L)
  AutoIML$new(
    task = task,
    learner = learner,
    resampling = resampling,
    purpose = purpose,
    quick_start = quick_start,
    seed = seed,
    config = config
  )$run(verbose = verbose)
}
