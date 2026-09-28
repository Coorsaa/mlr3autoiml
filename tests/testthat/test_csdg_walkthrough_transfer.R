test_that("independent transfer scenarios preserve claim-specific decision boundaries", {
  supported = function(gate = "G1") csdg_evidence_record(gate, TRUE, "necessary_requirement",
    result_direction = "supports", rationale = "The stated property was directly evaluated and supported.")
  missing = csdg_evidence_record("G4", TRUE, "necessary_requirement", availability = "unavailable",
    result_direction = "not_evaluated", rationale = "Required local evidence is absent.")
  failed = csdg_evidence_record("G4", TRUE, "necessary_requirement", result_direction = "challenges",
    rationale = "Observed error exceeds the scenario's explicitly justified error tolerance.")
  irrelevant = csdg_evidence_record("G4", FALSE, "descriptive_context",
    rationale = "The claim is a global function description and asserts no local-surrogate property.")
  graded = csdg_evidence_record("G5", TRUE, "graded_support", result_direction = "supports",
    rationale = "Some repeated results recur, without specifying a sufficient requirement.")
  mixed = csdg_evidence_record("G5", TRUE, "necessary_requirement", result_direction = "mixed",
    rationale = "The required stability property is not settled by mixed evidence.")
  defeater = csdg_evidence_record("G2", TRUE, "potential_defeater", result_direction = "challenges",
    materiality = "materialized", adjudication_basis = "substantive_adjudication",
    claim_consequence = "exact_claim_not_retained",
    rationale = "A verified column swap invalidates the named feature's reported contrast.")
  unsubstantiated = csdg_evidence_record("G2", TRUE, "potential_defeater", result_direction = "descriptive",
    rationale = "A possible objection has been documented but not established.")
  unresolved_constraint = csdg_evidence_record("G5", TRUE, "graded_support", result_direction = "supports",
    claim_consequence = "unresolved", rationale = "An explicit remaining qualification constrains this sentence.")
  scenarios = list(
    list(supported()), list(supported(), failed), list(supported(), missing),
    list(supported(), irrelevant), list(irrelevant), list(graded),
    list(supported(), mixed), list(supported(), defeater), list(supported(), unsubstantiated),
    list(supported(), unresolved_constraint), list(failed, missing)
  )
  expected = c("met", "not_met", "unresolved", "met", "unresolved", "unresolved", "unresolved",
    "not_met", "met", "unresolved", "not_met")
  actual = vapply(scenarios, function(records) csdg_adjudicate_claim(records)$decision, character(1L))
  expect_identical(actual, expected)
  expect_identical(csdg_adjudicate_claim(list(), claim_applicable = FALSE,
    applicability_rationale = "This scenario explicitly places the statement outside this evaluation.")$decision,
    "not_applicable")
})
