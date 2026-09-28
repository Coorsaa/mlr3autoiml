.gate_registry = function() data.table(
  gate_id = .csdg_gate_ids,
  gate_name = c(
    "Specification",
    "Measurement and data",
    "Predictive performance",
    "Procedure",
    "Calibration",
    "Decisions",
    "Local fidelity",
    "Stability",
    "Models",
    "Settings",
    "Subgroups",
    "Users"
  ),
  area = .csdg_gate_areas[c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)],
  area_order = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
  evidence_question = c(
    "Are the claim and its six scope elements stated?",
    paste(
      "Do the measures, sample, and outcome coding cover what the claim names, and are the predictors available",
      "when the use requires them?"
    ),
    "Does the model predict adequately on held-out data?",
    paste(
      "Does the procedure compute the named quantity, and does the result show what the claim says?",
      "For claims about realistic inputs: do unrealistic combinations leave it unaffected?"
    ),
    "Do predicted values agree with observed outcomes on their scale?",
    "Does acting on the model or explanation improve decisions?",
    "Does a local surrogate reproduce the model near the case, on points not used to fit it?",
    "Does the result persist when the same quantity is estimated again?",
    "Does the result hold for all models the claim covers?",
    "Does the result hold in other settings, such as countries or cohorts?",
    "Does the result hold within named subgroups?",
    "Do the intended users understand or benefit from the explanation?"
  ),
  required_if = c(
    "always",
    "always",
    "is a population claim or uses the predictions (e.g., to select people)",
    "interprets an explanation result",
    "interprets predicted values as probabilities or risks",
    "supports decisions about people",
    "explains individual predictions with a surrogate",
    "states a ranking, size, or pattern",
    "covers several models or learners, or is a population claim",
    "extends beyond the sampled setting",
    "refers to subgroups",
    "asserts understanding or benefit"
  ),
  typical_diagnostic = c(
    "Completed scope",
    "Documentation; validity evidence",
    paste(
      "Cross-validation respecting clustering; comparison with a model without predictors and strong",
      "alternatives"
    ),
    paste(
      "Perturbation and reference data that match the claim; comparison of marginal with grouped, conditional,",
      "or refit-based quantities"
    ),
    "Calibration curve, intercept, and slope",
    "Decision curves; prospective evaluation",
    "Cross-fitted fidelity",
    "Share of seeds, folds, refits, or plausible values in which the result holds",
    "Comparison with named or similarly accurate models",
    "External data",
    "Subgroup analyses",
    "Studies with the intended users"
  )
)

.causal_design_row = function() data.table(
  gate_id = "CD",
  gate_name = "Causal design",
  area = "Causal design (not a gate)",
  area_order = 4L,
  evidence_question = "Does a design with stated identification assumptions support the effect?",
  required_if = "is causal",
  typical_diagnostic = "Randomized, quasi-experimental, or adjustment-based identification"
)

# Kept for code that expects the two-column catalog of earlier versions.
.gate_catalog = function() .gate_registry()[, .(gate_id, gate_name)]

#' The 12 gates of CSDG
#'
#' Returns the gate registry of Table 3 of the article: for each gate, its identifier, name, area, evidence
#' question, the condition under which a claim requires it ("Required if the claim ..."), and a typical diagnostic.
#' The areas are "Foundation of the claim" (G0a, G0b, G1), "Predictions, explanations, and decisions" (G2, G3a, G3b,
#' G4, G5), and "Extensions" (G6a, G6b, G7a, G7b).
#' The causal design is not a gate, because no explanation diagnostic supplies it, but a causal claim requires it;
#' `include_causal_design = TRUE` appends it with the identifier `"CD"`.
#'
#' @param include_causal_design Whether to append the causal design (not a gate) as a final row.
#'
#' @return A `data.table` with columns `gate_id`, `gate_name`, `area`, `area_order`, `evidence_question`,
#'   `required_if`, and `typical_diagnostic`.
#' @examples
#' csdg_gate_registry()[, .(gate_id, gate_name, required_if)]
#' @export
csdg_gate_registry = function(include_causal_design = FALSE) {
  assert_flag(include_causal_design, .var.name = "include_causal_design")
  registry = .gate_registry()
  if (include_causal_design) registry = rbind(registry, .causal_design_row())
  registry[]
}

.gate_plan_row = function(required, trigger, components, plan_role = if (required) "required" else "not_required") {
  .assert_choice(plan_role, c("required", "context", "not_required"), "plan_role")
  list(
    required = required,
    plan_role = plan_role,
    evidence_role = if (required) "required_property" else "context",
    trigger = trigger,
    required_components = components,
    applicable = required,
    applicability = if (required) "applicable" else "not_applicable"
  )
}

.csdg_claim_meaning = function(claim) {
  meaning = claim$meaning
  if (is.null(meaning)) {
    semantics = claim$semantics %||% "fitted_model_description"
    meaning = if (semantics %in% c("causal", "recourse")) "causal_claim" else "model_description"
  }
  meaning
}

.csdg_claim_model = function(claim) {
  .csdg_map_legacy(claim$model_scope %||% "fitted_model", .csdg_legacy_model_scopes, "Model value", warn = FALSE)
}

#' Derive the required properties of a claim (gate plan)
#'
#' Applies the column "Required if the claim ..." of the gate registry ([csdg_gate_registry()], Table 3 of the
#' article) to the claim's scope. G0a and G0b are always required.
#' G1 (predictive performance) is required if the claim is a population claim or uses the predictions (claim types
#' `"predictive_performance"`, `"calibration"`, or `"decision"`); for an explanation claim that does neither, held-out
#' performance is reported as context, because a model description holds whatever the model's accuracy.
#' G2 (procedure) and G5 (stability) are required by every explanation claim (`"global_explanation"`,
#' `"local_explanation"`).
#' G3a (calibration) is required if the claim interprets predicted values as probabilities or risks (claim type
#' `"calibration"`); a decision claim that interprets predicted probabilities declares `"calibration"` as well.
#' G3b (decisions) is required by a `"decision"` claim.
#' G4 (local fidelity) is required by a `"local_explanation"` claim that uses a local surrogate; if `explanation` is
#' missing or declares no `method_ids`, G4 is required conservatively; exact attribution methods such as exact
#' Shapley values do not require it.
#' G6a (models) is required if the claim covers several models or learners (`model = "several_models"` or claim type
#' `"model_generalization"`) or is a population claim.
#' G6b (settings) is required if the claim extends beyond the sampled setting (`"setting_generalization"` or a
#' `setting_scope` other than `"analytic_sample"`).
#' G7a (subgroups) is required by a `"subgroup"` claim; subgroup variables declared only for auditing are context.
#' G7b (users) is required if the claim asserts understanding or benefit (`use_claim = TRUE`) or bases decisions on
#' explanations.
#' A causal claim (`meaning = "causal_claim"`) is a population claim and also requires a causal design, which is not
#' a gate; the plan records this in the attribute `causal_design_required`, and [csdg_adjudicate_claim()] then treats
#' a missing causal design (`gate_id = "CD"`) as an open property.
#' Every gate in the plan that the claim requires is a required property; the plan contains no score.
#'
#' @param claim A `CSDGClaim`.
#' @param measurement A `CSDGMeasurement`.
#' @param explanation An optional `CSDGExplanation`.
#'
#' @return A `CSDGGatePlan` data table with one row per gate and the columns `gate_id`, `gate_name`, `area`,
#'   `evidence_question`, `required_if`, `required`, `plan_role` (`"required"`, `"context"`, or `"not_required"`),
#'   `evidence_role` (`"required_property"` or `"context"`), `trigger` (a claim-specific reason),
#'   `typical_diagnostic`, and `required_components`, plus the legacy columns `applicable` (equal to `required`) and
#'   `applicability`. The attribute `causal_design_required` is `TRUE` for a causal claim.
#' @examples
#' claim = csdg_claim(
#'   id = "both_models",
#'   statement = "Under marginal permutation, both models rely more on item 1 than on item 2.",
#'   claim_type = "global_explanation",
#'   quantity = "marginal PFI with squared error",
#'   model = "several_models",
#'   procedure = "each item permuted independently",
#'   data = "Z standard normal; item 1 = item 2 = outcome = Z",
#'   meaning = "model_description",
#'   use = "scientific description"
#' )
#' plan = csdg_gate_plan(claim, csdg_measurement(), csdg_explanation(method_ids = "pfi"))
#' plan[, .(gate_id, gate_name, required, plan_role)]
#' @export
csdg_gate_plan = function(claim, measurement, explanation = NULL) {
  assert_class(claim, "CSDGClaim", .var.name = "claim")
  assert_class(measurement, "CSDGMeasurement", .var.name = "measurement")
  if (!is.null(explanation)) assert_class(explanation, "CSDGExplanation", .var.name = "explanation")

  claim_type = claim$claim_type
  meaning = .csdg_claim_meaning(claim)
  model = .csdg_claim_model(claim)
  is_explanation = any(claim_type %in% c("global_explanation", "local_explanation"))
  is_population = meaning %in% c("population_claim", "causal_claim")
  is_causal = identical(meaning, "causal_claim")
  uses_predictions = any(claim_type %in% c("predictive_performance", "calibration", "decision"))
  several_models = identical(model, "several_models") || "model_generalization" %in% claim_type
  beyond_setting = "setting_generalization" %in% claim_type ||
    !claim$setting_scope %in% c("analytic_sample", "unspecified")
  methods = if (is.null(explanation)) character() else explanation$method_ids
  surrogate = !length(methods) || "local_surrogate" %in% methods
  local = "local_explanation" %in% claim_type
  decision = "decision" %in% claim_type
  method_label = if (length(methods)) paste0(" (", paste(toupper(methods), collapse = ", "), ")") else ""
  population_label = if (is_causal) "a causal claim, and thus a population claim" else "a population claim"

  g1 = is_population || uses_predictions
  g4 = local && surrogate
  g6a = several_models || is_population
  g7b = isTRUE(claim$use_claim) || (decision && is_explanation)
  rows = list(
    G0a = .gate_plan_row(
      TRUE,
      "Every claim requires a stated claim and six scope elements.",
      "claim_card;quantity;model;procedure;data;meaning;use"
    ),
    G0b = .gate_plan_row(
      TRUE,
      "Every claim requires measures, sample, and outcome coding that cover what it names.",
      "measurement_card;missingness;preprocessing_verification;sampling;weights;clusters"
    ),
    G1 = .gate_plan_row(
      g1,
      if (is_population && uses_predictions) {
        paste0("The claim is ", population_label, " and uses the predictions.")
      } else if (is_population) {
        paste0("The claim is ", population_label, ".")
      } else if (uses_predictions) {
        "The claim uses the predictions."
      } else if (is_explanation) {
        paste(
          "The claim is a model description and uses no predictions; held-out performance is context,",
          "because a model description holds whatever the model's accuracy."
        )
      } else {
        "The claim is not a population claim and uses no predictions."
      },
      "out_of_fold_performance;baseline;leakage_controls",
      plan_role = if (g1) "required" else if (is_explanation) "context" else "not_required"
    ),
    G2 = .gate_plan_row(
      is_explanation,
      if (is_explanation) {
        paste0("The claim interprets an explanation result", method_label, ".")
      } else {
        "The claim interprets no explanation result."
      },
      "quantity_named_by_claim;perturbation_and_reference_data;realistic_inputs"
    ),
    G3a = .gate_plan_row(
      "calibration" %in% claim_type,
      if ("calibration" %in% claim_type) {
        "The claim interprets predicted values as probabilities or risks."
      } else {
        "The claim does not interpret predicted values as probabilities or risks."
      },
      "out_of_fold_calibration;calibration_uncertainty;range_calibration"
    ),
    G3b = .gate_plan_row(
      decision,
      if (decision) "The claim supports decisions about people." else "The claim supports no decision about people.",
      "action;thresholds;utilities;harms;decision_analysis"
    ),
    G4 = .gate_plan_row(
      g4,
      if (g4 && !length(methods)) {
        paste(
          "The claim explains individual predictions and declares no method; declare method_ids; exact",
          "attribution methods such as exact Shapley values do not require G4."
        )
      } else if (g4) {
        "The claim explains individual predictions with a local surrogate."
      } else if (local) {
        "The claim explains individual predictions without a local surrogate."
      } else {
        "The claim does not explain individual predictions."
      },
      "held_out_local_fidelity;absolute_error;target_case_error;scale;sensitivity;support"
    ),
    G5 = .gate_plan_row(
      is_explanation,
      if (is_explanation) {
        "The claim states a ranking, size, or pattern of an explanation result."
      } else {
        "The claim states no ranking, size, or pattern of an explanation result."
      },
      "repetitions;folds;refits;plausible_values;monte_carlo_error"
    ),
    G6a = .gate_plan_row(
      g6a,
      if (several_models && is_population) {
        paste0("The claim covers several models or learners and is ", population_label, ".")
      } else if (several_models) {
        "The claim covers several models or learners."
      } else if (is_population) {
        paste0("The claim is ", population_label, ".")
      } else {
        "The claim covers one fitted model or one learner and is not a population claim."
      },
      "similarly_accurate_models;prediction_dispersion;explanation_agreement"
    ),
    G6b = .gate_plan_row(
      beyond_setting,
      if (beyond_setting) "The claim extends beyond the sampled setting." else
        "The claim is restricted to the sampled setting.",
      "setting_unit;external_validation;matched_setting_exclusion;calibration_by_setting"
    ),
    G7a = .gate_plan_row(
      "subgroup" %in% claim_type,
      if ("subgroup" %in% claim_type) {
        "The claim refers to subgroups."
      } else if (length(claim$subgroup_variables %||% character())) {
        "The claim refers to no subgroup; subgroup variables declared for auditing are context."
      } else {
        "The claim refers to no subgroup."
      },
      "subgroup_composition;performance;calibration;comparability;contrasts"
    ),
    G7b = .gate_plan_row(
      g7b,
      if (isTRUE(claim$use_claim)) {
        "The claim asserts that intended users understand or benefit from the explanation."
      } else if (g7b) {
        "The claim bases decisions about people on explanations."
      } else {
        "The claim asserts no understanding or benefit for users."
      },
      "intended_users;user_understanding;reliance;workflow;implementation;harms"
    )
  )

  plan = rbindlist(lapply(names(rows), function(gate_id) {
    row = as.data.table(rows[[gate_id]])
    row[, gate_id := gate_id]
    row
  }), fill = TRUE)
  plan = merge(.gate_registry(), plan, by = "gate_id", all.x = TRUE, sort = FALSE)
  plan[, gate_order__ := match(gate_id, .csdg_gate_ids)]
  setorder(plan, gate_order__)
  plan[, gate_order__ := NULL]
  plan[, area_order := NULL]
  setcolorder(plan, c(
    "gate_id", "gate_name", "area", "evidence_question", "required_if", "required", "plan_role",
    "evidence_role", "trigger", "typical_diagnostic", "required_components", "applicable", "applicability"
  ))
  setattr(plan, "causal_design_required", is_causal)
  structure(plan, class = c("CSDGGatePlan", class(plan)))
}
