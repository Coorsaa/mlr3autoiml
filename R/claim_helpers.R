#' @title Build a validated claim card for `auto$ctx$claim`
#'
#' @description
#' Constructs a named list suitable for assignment to `auto$ctx$claim`. The
#' four operative fields — `purpose`, `semantics`, `stakes`, `claims` — are
#' required and validated. All narrative documentation fields are optional and
#' default to `NULL`; they are stored verbatim in the G0A artifact but never
#' branch gate logic.
#'
#' @param purpose (`character(1)`) Analysis purpose. One of `"exploratory"`,
#'   `"global_insight"`, `"decision_support"`, `"deployment"`.
#' @param semantics (`character(1)`) Interpretation semantics. One of
#'   `"within_support"`, `"marginal_model_query"`, `"causal_recourse"`.
#' @param stakes (`character(1)`) Stakes level. One of `"low"`, `"medium"`,
#'   `"high"`.
#' @param claims (`list`) Named logical list with elements `global`, `local`,
#'   `decision` (e.g. `list(global = TRUE, local = TRUE, decision = FALSE)`).
#' @param decision_spec (`list` | `NULL`) Decision context for G3: named list
#'   with at least `thresholds` (numeric vector). Required when
#'   `claims$decision = TRUE`.
#' @param audience (`character(1)` | `NULL`) Intended audience description.
#' @param target_population (`character(1)` | `NULL`) Target population.
#' @param setting (`character(1)` | `NULL`) Deployment / analysis setting.
#' @param time_horizon (`character(1)` | `NULL`) Decision / prediction
#'   time horizon.
#' @param transport_boundary (`character(1)` | `NULL`) Explicit transport
#'   boundary statement.
#' @param intended_use (`character(1)` | `NULL`) Stated intended use.
#' @param intended_non_use (`character(1)` | `NULL`) Explicit non-uses.
#' @param prohibited_interpretations (`character(1)` | `NULL`) Interpretations
#'   explicitly ruled out (e.g. causal claims).
#' @param decision_policy_rationale (`character(1)` | `NULL`) Rationale for
#'   the decision policy / utility framing.
#' @param human_factors_evidence (`list` | `NULL`) Human-factors evidence
#'   for G7B (user-facing audiences). Named list with fields `participants`,
#'   `task_design`, `baseline`, `outcomes`, `analysis_summary`.
#'
#' @return A named list ready to assign to `auto$ctx$claim`.
#' @export
#' @examples
#' card <- make_claim_card(
#'   purpose  = "decision_support",
#'   semantics = "within_support",
#'   stakes   = "medium",
#'   claims   = list(global = TRUE, local = TRUE, decision = TRUE),
#'   decision_spec = list(
#'     thresholds = seq(0.05, 0.25, by = 0.05),
#'     utility    = list(tp = 1, fp = 0, fn = 0, tn = 0)
#'   ),
#'   target_population = "Adults aged 18+",
#'   intended_use = "Prioritise follow-up contacts.",
#'   prohibited_interpretations = "Causal claims; individual diagnosis."
#' )
make_claim_card = function(
    purpose,
    semantics,
    stakes,
    claims,
    decision_spec                = NULL,
    audience                     = NULL,
    target_population            = NULL,
    setting                      = NULL,
    time_horizon                 = NULL,
    transport_boundary           = NULL,
    intended_use                 = NULL,
    intended_non_use             = NULL,
    prohibited_interpretations   = NULL,
    decision_policy_rationale    = NULL,
    human_factors_evidence       = NULL) {

  checkmate::assert_choice(purpose,
    c("exploratory", "global_insight", "decision_support", "deployment"))
  checkmate::assert_choice(semantics,
    c("within_support", "marginal_model_query", "causal_recourse"))
  checkmate::assert_choice(stakes, c("low", "medium", "high"))
  checkmate::assert_list(claims, names = "named")
  checkmate::assert_subset(names(claims), c("global", "local", "decision"))
  for (nm in names(claims)) checkmate::assert_flag(claims[[nm]])

  if (isTRUE(claims$decision) && is.null(decision_spec)) {
    cli_warn(c(
      "!" = "{.arg decision_spec} is {.code NULL} but {.code claims$decision = TRUE}.",
      "i" = "G3 (calibration + decision utility) requires a {.arg decision_spec} list."
    ))
  }

  Filter(Negate(is.null), list(
    purpose                    = purpose,
    semantics                  = semantics,
    stakes                     = stakes,
    claims                     = claims,
    decision_spec              = decision_spec,
    audience                   = audience,
    target_population          = target_population,
    setting                    = setting,
    time_horizon               = time_horizon,
    transport_boundary         = transport_boundary,
    intended_use               = intended_use,
    intended_non_use           = intended_non_use,
    prohibited_interpretations = prohibited_interpretations,
    decision_policy_rationale  = decision_policy_rationale,
    human_factors_evidence     = human_factors_evidence
  ))
}

#' @title Build a measurement card for `auto$ctx$measurement`
#'
#' @description
#' Constructs a named list suitable for assignment to `auto$ctx$measurement`.
#' `level` is required and operative (affects G0B checks and narration scope).
#' All other fields are narrative documentation stored in the G0B artifact.
#'
#' @param level (`character(1)`) Measurement level. One of `"item"`,
#'   `"scale"`, `"factor_score"`, `"plausible_values"`.
#' @param missingness_plan (`character(1)` | `NULL`) How missing data are
#'   handled in the pipeline.
#' @param reliability (`list` | `character(1)` | `NULL`) Reliability evidence
#'   (e.g. `list(method = "Cronbach alpha", value = 0.87)`).
#' @param invariance (`list` | `character(1)` | `NULL`) Measurement invariance
#'   status across groups / time points.
#' @param scoring_pipeline (`character(1)` | `NULL`) How the outcome and
#'   predictors are scored / encoded.
#'
#' @return A named list ready to assign to `auto$ctx$measurement`.
#' @export
#' @examples
#' card <- make_measurement_card(
#'   level            = "scale",
#'   missingness_plan = "Median impute numerics, mode impute categoricals.",
#'   reliability      = list(method = "Cronbach alpha", note = "alpha = 0.87"),
#'   invariance       = list(status = "partial", note = "Loadings invariant, intercepts not tested.")
#' )
make_measurement_card = function(
    level,
    missingness_plan = NULL,
    reliability      = NULL,
    invariance       = NULL,
    scoring_pipeline = NULL) {

  checkmate::assert_choice(level,
    c("item", "scale", "factor_score", "plausible_values"))

  Filter(Negate(is.null), list(
    level            = level,
    missingness_plan = missingness_plan,
    reliability      = reliability,
    invariance       = invariance,
    scoring_pipeline = scoring_pipeline
  ))
}
