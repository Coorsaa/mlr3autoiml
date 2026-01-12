#' @title mlr3autoiml: Automated Gate-Based Auditing for Interpretable Machine Learning in 'mlr3'
#'
#' @description
#' `mlr3autoiml` implements a **gate-based AutoIML workflow (G0A/G0B, G1--G6, G7A/G7B)** for
#' [mlr3][mlr3::mlr3-package] tasks and learners.
#'
#' The package focuses on *interpretability readiness* and *diagnostic evidence*
#' rather than hyperparameter optimization: it automatically computes
#' task- and data-dependent IML artifacts (e.g., PDP/ICE vs. ALE selection under
#' feature dependence), calibration/decision utility checks, faithfulness checks,
#' and stability diagnostics.
#'
#' The main entry point is the [AutoIML] R6 class.
#'
#' @seealso
#' * [AutoIML] for the main orchestrator.
#' * [autoiml()] for a convenience wrapper.
#'
#' @keywords internal
"_PACKAGE"
