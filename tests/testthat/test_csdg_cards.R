test_that("cards validate and plan claim-specific gates", {
  fx = make_classif_fixture()
  cards = make_cards(fx$task)
  expect_s3_class(cards$claim, "CSDGClaim")
  expect_s3_class(cards$measurement, "CSDGMeasurement")
  expect_s3_class(cards$explanation, "CSDGExplanation")

  plan = csdg_gate_plan(
    cards$claim, cards$measurement, cards$explanation
  )
  expect_s3_class(plan, "CSDGGatePlan")
  expect_equal(plan$gate_id, c("G0a", "G0b", "G1", "G2", "G3a", "G3b", "G4", "G5", "G6a", "G6b", "G7a", "G7b"))
  expect_true(plan[gate_id == "G5", required])
  expect_false(plan[gate_id == "G6a", required])
  expect_identical(plan[gate_id == "G1", evidence_role], "necessary_warrant")
})

test_that("claim revisions retain an explicit version relationship", {
  original = csdg_claim(
    id = "claim_original",
    statement = "An explanation generalizes across models.",
    target = "predictions",
    population = "analytic sample",
    analytic_distribution = "observed rows",
    scientific_use = "Model interpretation",
    explanation_design = "Held-out PFI",
    claim_type = "model_generalization",
    model_scope = "model_class"
  )
  revised = csdg_claim_revision(
    original,
    id = "claim_revised",
    statement = "The explanation describes the selected fitted model.",
    claim_version = "C1",
    model_scope = "selected_model"
  )

  expect_identical(revised$parent_claim_id, original$id)
  expect_identical(revised$claim_version, "C1")
  expect_identical(revised$revision_relation, "narrower")
  expect_identical(revised$model_scope, "selected_model")
})

test_that("decision cards require decision semantics at G0a", {
  fx = make_classif_fixture()
  cards = make_cards(fx$task)
  decision_claim = csdg_claim(
    statement = "Predictions should guide an action.",
    claim_type = "decision",
    target = "y",
    population = "synthetic",
    analytic_distribution = "synthetic",
    model_scope = "selected_model",
    setting_scope = "analytic_sample"
  )
  result = csdg_audit(
    fx$task, fx$learner, decision_claim, cards$measurement,
    config = csdg_config(resampling = list(folds = 3L, repeats = 1L)),
    run_gates = "G0a"
  )
  expect_equal(result$gates$G0a$status, "not_met")
})

test_that("preprocessing review requires an artifact", {
  expect_error(
    csdg_measurement(
      verification = list(status = "author_reviewed")
    ),
    "artifact"
  )
})

test_that("local claims do not trigger global PFI stability", {
  fx = make_classif_fixture()
  measurement = make_cards(fx$task)$measurement
  claim = csdg_claim(
    id = "local_only",
    statement = "A local surrogate approximates the held-out model near a declared case.",
    claim_type = "local_explanation",
    target = "binary probability",
    unit = "row",
    population = "synthetic population",
    analytic_distribution = "synthetic sample",
    model_scope = "cross_fitted_pipeline",
    setting_scope = "analytic_sample",
    scientific_use = "Local fitted-model description",
    explanation_design = "Held-out cross-fitted local surrogate"
  )
  plan = csdg_gate_plan(claim, measurement, csdg_explanation(scope = "local"))

  expect_true(plan[gate_id == "G4", required])
  expect_false(plan[gate_id == "G5", required])
})

test_that("card serialization preserves field names and removes other attributes", {
  card = structure(
    list(first = 1L, second = "two"),
    class = c("SyntheticCard", "list"),
    custom_attribute = "remove me"
  )

  serialized = .card_to_list(card)

  expect_identical(serialized, list(first = 1L, second = "two"))
  expect_identical(names(serialized), c("first", "second"))
  expect_null(attr(serialized, "class"))
  expect_null(attr(serialized, "custom_attribute"))
})

test_that("card serialization rejects unnamed or ambiguously named lists", {
  expect_error(.card_to_list(list(1L)), "uniquely named list")
  expect_error(.card_to_list(structure(list(1L), names = "")), "uniquely named list")
  expect_error(.card_to_list(structure(list(1L, 2L), names = c("field", "field"))), "uniquely named list")
})
