#' @title mlr3autoiml: Automated Gate-Based Auditing for Interpretable Machine Learning in 'mlr3'
#'
#' @description
#' `mlr3autoiml` implements a \strong{gate-based AutoIML workflow (G0A/G0B, G1--G6, G7A/G7B)} for
#' [mlr3][mlr3::mlr3-package] tasks and learners.
#'
#' The package focuses on \emph{interpretability readiness} and \emph{diagnostic evidence}
#' rather than hyperparameter optimization: it automatically computes
#' task- and data-dependent IML artifacts (e.g., PDP/ICE vs. ALE selection under
#' feature dependence), calibration/decision utility checks, faithfulness checks,
#' and stability diagnostics.
#'
#' The main entry point is the [AutoIML] R6 class.
#'
#' @seealso
#' \itemize{
#'   \item [AutoIML] for the main orchestrator.
#'   \item [autoiml()] for a convenience wrapper.
#'   \item [irl_from_gates()] for Interpretation Readiness Level computation.
#'   \item [report_card()] for audit trail summary.
#' }
#'
#' @importFrom data.table data.table rbindlist setnames := .N .SD .I
#' @importFrom stats cor quantile sd var coef glm predict lm
#' @importFrom utils modifyList
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check notes for data.table non-standard evaluation
utils::globalVariables(c(
  # Common data.table symbols
  ".N", ".SD", ".I", ".GRP", ".BY", ".EACHI", ":=",
  # Column names used across files
  "phi", "abs_phi", "feature", "feature_value", "feature_label", "feature_f",
  "class_label", "row_id", "mean_abs_phi", "value_scaled",
  "start", "end", "sign", "x_lab", "hjust",
  "importance", "learner_id", "measure_id", "mean", "sd", "se", "ci_low", "ci_high",
  "in_rashomon", "rashomon_threshold",
  "ice_sd_mean", "hstat", "grid_n", "sample_n",
  "type", "id", "group", "n", "logloss",
  "status", "gate_id", "gate_name", "pdr", "summary",
  "irl_overall", "irl_global", "irl_local", "irl_decision",
  "purpose", "quick_start", "semantics", "stakes",
  "claim_global", "claim_local", "claim_decision",
  "missing_rate", "iteration", "value",
  "x_mid", "y_mean", "bin", "threshold", "net_benefit", "nb_treat_all", "nb_treat_none"
))
