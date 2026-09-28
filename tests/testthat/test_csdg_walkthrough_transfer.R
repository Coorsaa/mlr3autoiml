test_that("predefined cases preserve the decision rule", {
  property = function(gate = "G1", status = "supported") csdg_evidence_record(gate, TRUE, "required_property",
    status = status, rationale = "The stated property was directly evaluated.")
  missing = csdg_evidence_record("G4", TRUE, "required_property", status = "open", availability = "unavailable",
    rationale = "Required local evidence is absent.")
  failed = property("G4", "contradicted")
  irrelevant = csdg_evidence_record("G4", FALSE, "context",
    rationale = "The claim is a global model description and asserts no property of a local surrogate.")
  context = csdg_evidence_record("G5", TRUE, "context", result_direction = "supports",
    rationale = "Some repeated results recur, without a criterion.")
  inconclusive = csdg_evidence_record("G5", TRUE, "required_property", result_direction = "mixed",
    rationale = "The required stability property is not settled by mixed evidence.")
  counterevidence = csdg_evidence_record("G2", TRUE, "established_counterevidence",
    rationale = "A verified column swap invalidates the reported contrast.",
    required_property = "The procedure computes the named quantity.",
    observation = "Two columns were swapped before the PFI computation.",
    relevance_to_proposition = "The swap changes which item the reported value refers to.")
  objection = csdg_evidence_record("G2", TRUE, "context", result_direction = "descriptive",
    rationale = "A possible objection has been documented but not established.")
  # A legacy 0.1.5 record: a graded_support record with an "unresolved" consequence is context in 0.1.6.
  legacy_context = suppressWarnings(csdg_evidence_record("G5", TRUE, "graded_support", result_direction = "supports",
    claim_consequence = "unresolved", rationale = "A remaining qualification recorded in version 0.1.5."))
  threat = csdg_evidence_record("G1", TRUE, "unresolved_threat",
    rationale = "The optimism caused by predictor selection was not quantified.",
    required_property = "Held-out performance is not inflated by predictor selection.",
    observation = "Predictors were selected on the same data outside the cross-validation.")
  scenarios = list(
    list(property()), list(property(), failed), list(property(), missing),
    list(property(), irrelevant), list(irrelevant), list(context),
    list(property(), inconclusive), list(property(), counterevidence), list(property(), objection),
    list(property(), legacy_context), list(failed, missing), list(property(), threat)
  )
  expected = c("met", "not_met", "unresolved", "met", "unresolved", "unresolved", "unresolved",
    "not_met", "met", "met", "not_met", "unresolved")
  actual = vapply(scenarios, function(records) {
    csdg_adjudicate_claim(records, claim_applicable = TRUE)$assessment
  }, character(1L))
  expect_identical(actual, expected)
  expect_identical(csdg_adjudicate_claim(list(), claim_applicable = FALSE,
    applicability_rationale = "This scenario explicitly places the statement outside this evaluation.")$assessment,
    "not_applicable")
})
