.gate_catalog = function() data.table(
  gate_id = .csdg_gate_ids,
  gate_name = c(
    "Claim and target specification",
    "Measurement and preprocessing review",
    "Predictive adequacy",
    "Dependence, support, and heterogeneity",
    "Calibration",
    "Utility and decision consequences",
    "Local faithfulness and recourse language",
    "Explanation stability",
    "Model multiplicity",
    "Setting transport",
    "Technical subgroup behavior",
    "Audience and workflow evidence"
  )
)

.gate_plan_row = function(required, trigger, components, evidence_role) {
  list(
    required = required,
    applicability = if (required) 1L else 0L,
    trigger = trigger,
    required_components = components,
    evidence_role = evidence_role
  )
}

#' Plan claim-scoped evidence modules
#'
#' Maps a declared claim to applicable evidence modules.
#' A required module is a necessary warrant for that claim as declared; optional diagnostics can still provide
#' graded support, potential defeaters, or descriptive context.
#' The mapping is explicit and contains no aggregate score.
#'
#' @param claim A `CSDGClaim`.
#' @param measurement A `CSDGMeasurement`.
#' @param explanation An optional `CSDGExplanation`.
#'
#' @return A `CSDGGatePlan` data table with applicability, evidence role, trigger, and required components.
#' @export
csdg_gate_plan = function(claim, measurement, explanation = NULL) {
  assert_class(claim, "CSDGClaim", .var.name = "claim")
  assert_class(measurement, "CSDGMeasurement", .var.name = "measurement")
  if (!is.null(explanation)) assert_class(explanation, "CSDGExplanation", .var.name = "explanation")

  claim_type = claim$claim_type
  has_explanation = any(claim_type %in% c("global_explanation", "local_explanation"))
  has_global = "global_explanation" %in% claim_type
  has_local = "local_explanation" %in% claim_type ||
    (!is.null(explanation) && "local" %in% explanation$scope)
  has_decision = "decision" %in% claim_type
  has_calibration = any(claim_type %in% c("calibration", "decision"))
  has_subgroup = "subgroup" %in% claim_type || length(claim$subgroup_variables %||% character()) > 0L
  has_audience_use = has_decision || identical(claim$claim_level, "use")
  has_model_scope = "model_generalization" %in% claim_type ||
    claim$model_scope %in% c("near_equivalent_models", "model_class")
  has_setting_scope = "setting_generalization" %in% claim_type ||
    !claim$setting_scope %in% c("analytic_sample", "unspecified")
  purely_functional = identical(claim$claim_level, "functional") &&
    claim$model_scope %in% c("selected_model", "cross_fitted_pipeline") &&
    !has_decision && !has_subgroup && !has_model_scope && !has_setting_scope
  needs_prediction = !purely_functional && any(claim_type %in% c(
    "predictive_performance", "calibration", "subgroup", "decision", "model_generalization",
    "setting_generalization", "global_explanation", "local_explanation"
  ))

  rows = list(
    G0a = .gate_plan_row(
      TRUE,
      "Every interpretation requires a versioned claim and explicit target, semantics, scope, distribution, and use.",
      "claim_card;target;semantics;model_scope;analytic_distribution;scientific_use;explanation_design",
      "necessary_warrant"
    ),
    G0b = .gate_plan_row(
      TRUE,
      "Every substantive interpretation inherits the measurement, sampling, and preprocessing conditions.",
      "measurement_card;missingness;preprocessing_verification;sampling;weights;clusters",
      "necessary_warrant"
    ),
    G1 = .gate_plan_row(
      needs_prediction,
      if (needs_prediction) {
        "The claim extends beyond a syntactic statement about one fixed function."
      } else {
        "A purely functional statement about one fixed function does not require predictive adequacy."
      },
      "out_of_fold_performance;baseline;leakage_controls",
      if (needs_prediction) "necessary_warrant" else "descriptive_context"
    ),
    G2 = .gate_plan_row(
      has_explanation || has_subgroup || has_decision,
      if (has_explanation || has_subgroup || has_decision) {
        "The claim can be defeated by unsupported or dependence-breaking queries."
      } else {
        "No support-dependent explanation, subgroup, or decision statement is declared."
      },
      "mixed_type_dependence;empirical_support;perturbation_semantics",
      if (has_explanation || has_subgroup || has_decision) "potential_defeater" else "descriptive_context"
    ),
    G3a = .gate_plan_row(
      has_calibration,
      if (has_calibration) {
        "The claim interprets calibrated probabilities, scores, or thresholds."
      } else {
        "Calibration is not part of the declared claim."
      },
      "out_of_fold_calibration;calibration_uncertainty;range_calibration",
      if (has_calibration) "necessary_warrant" else "graded_support"
    ),
    G3b = .gate_plan_row(
      has_decision,
      if (has_decision) {
        "The claim asserts decision, utility, triage, or allocation consequences."
      } else {
        "No decision or utility claim is declared."
      },
      "action;thresholds;utilities;harms;decision_analysis",
      if (has_decision) "necessary_warrant" else "descriptive_context"
    ),
    G4 = .gate_plan_row(
      has_local,
      if (has_local) "The claim is local, regional, counterfactual, or recourse-facing." else
        "No local, regional, counterfactual, or recourse claim is declared.",
      "held_out_local_fidelity;absolute_error;target_case_error;scale;sensitivity;support",
      if (has_local) "necessary_warrant" else "descriptive_context"
    ),
    G5 = .gate_plan_row(
      has_global,
      if (has_global) {
        "The reported global explanation is expected to recur under the declared analysis repetitions."
      } else {
        "No recurring global explanation pattern is claimed; local stability belongs to the local-fidelity design."
      },
      "perturbation_repetitions;practical_ties;top_k;value_sensitive_agreement;monte_carlo_error",
      if (has_global) "graded_support" else "descriptive_context"
    ),
    G6a = .gate_plan_row(
      has_model_scope,
      if (has_model_scope) "The interpretation extends beyond the focal fitted model." else
        "The interpretation is restricted to the focal fitted model.",
      "near_equivalent_models;prediction_dispersion;explanation_agreement",
      if (has_model_scope) "necessary_warrant" else "descriptive_context"
    ),
    G6b = .gate_plan_row(
      has_setting_scope,
      if (has_setting_scope) "The interpretation extends beyond the observed analytic setting." else
        "The interpretation is restricted to the observed analytic setting.",
      "setting_unit;external_validation;matched_setting_exclusion;calibration_by_setting",
      if (has_setting_scope) "necessary_warrant" else "descriptive_context"
    ),
    G7a = .gate_plan_row(
      has_subgroup,
      if (has_subgroup) "The claim compares or interprets technical behavior across subgroups." else
        "No subgroup comparison is declared.",
      "subgroup_composition;performance;calibration;comparability;contrasts",
      if (has_subgroup) "necessary_warrant" else "descriptive_context"
    ),
    G7b = .gate_plan_row(
      has_audience_use,
      if (has_audience_use) "The claim assumes an audience, workflow, implementation, or use consequence." else
        "No audience, workflow, or implementation claim is declared.",
      "intended_users;user_understanding;reliance;workflow;implementation;harms",
      if (has_audience_use) "necessary_warrant" else "descriptive_context"
    )
  )

  plan = rbindlist(lapply(names(rows), function(gate_id) {
    row = as.data.table(rows[[gate_id]])
    row[, gate_id := gate_id]
    setcolorder(row, c("gate_id", setdiff(names(row), "gate_id")))
    row
  }), fill = TRUE)
  plan = merge(.gate_catalog(), plan, by = "gate_id", all.x = TRUE, sort = FALSE)
  plan[, gate_order__ := match(gate_id, .csdg_gate_ids)]
  setorder(plan, gate_order__)
  plan[, gate_order__ := NULL]
  structure(plan, class = c("CSDGGatePlan", class(plan)))
}
