scope_evidence = function(...) {
  arguments = list(
    gate_id = "G4", applicable = TRUE, role = "context", result_direction = "descriptive",
    rationale = "A comparison is recorded without an automatic negative judgment.",
    varied_component = "output_scale", held_constant = c("model", "cases", "weights"),
    same_estimand = FALSE, same_estimand_rationale = "The response-scale and logit-scale targets differ.",
    invariance_claimed = FALSE
  )
  do.call(csdg_evidence_record, modifyList(arguments, list(...)))
}

scope_chain = function() {
  list(
    required_property = "The claim explicitly requires invariance over the declared comparison.",
    observation = "The diagnostic contrast changes across variants.",
    relevance_to_proposition = "The observed difference contradicts the particular asserted invariant property."
  )
}

test_that("legitimate changes of the quantity are context, not counterevidence", {
  for (component in c("output_scale", "reference_distribution", "model", "population")) {
    record = scope_evidence(varied_component = component)
    expect_identical(record$role, "context")
    expect_null(record$status)
    expect_false(record$same_estimand)
    expect_identical(record$variation_status, "documented_not_verified")
    expect_identical(record$proposition_linkage, "not_recorded")
  }
  arguments = c(list(role = "established_counterevidence", result_direction = "challenges"), scope_chain())
  expect_error(do.call(scope_evidence, arguments), "claimed invariance")
  arguments$invariance_claimed = TRUE
  record = do.call(scope_evidence, arguments)
  expect_identical(record$proposition_linkage, "documented_not_verified")
  expect_identical(csdg_adjudicate_claim(list(record))$assessment, "not_met")
})

test_that("same-quantity numerical variation requires a complete relevance chain", {
  arguments = list(
    varied_component = "seed", same_estimand = TRUE, same_estimand_rationale = "Only Monte Carlo draws change.",
    role = "required_property", status = "contradicted", result_direction = "challenges"
  )
  expect_error(do.call(scope_evidence, arguments), "property-observation-relevance")
  record = do.call(scope_evidence, c(arguments, scope_chain()))
  expect_identical(csdg_adjudicate_claim(list(record))$proposition_linkage, "documented_not_verified")
  record$same_estimand = FALSE
  expect_error(csdg_adjudicate_claim(list(record)), "claimed invariance")
})

test_that("missing variation metadata is not silently interpreted as the same quantity", {
  expect_error(scope_evidence(same_estimand_rationale = NULL), "same_estimand_rationale")
  record = scope_evidence(same_estimand = NA, same_estimand_rationale = "The target correspondence is uncertain.")
  expect_true(is.na(record$same_estimand))
  expect_error(scope_evidence(required_property = "A property without its diagnostic connection."), "full")
  expect_error(scope_evidence(same_estimand = "unknown"), "logical")
  plain = csdg_evidence_record("G4", TRUE, "context", rationale = "Historical characterization.")
  expect_identical(plain$variation_status, "not_recorded")
  expect_identical(plain$proposition_linkage, "not_recorded")
  expect_identical(csdg_adjudicate_claim(list(plain))$proposition_linkage, "not_recorded_for_all_evidence")
})

test_that("changes of the quantity cannot bypass the guard through required properties", {
  arguments = c(list(role = "required_property", status = "contradicted", result_direction = "challenges"),
    scope_chain())
  expect_error(do.call(scope_evidence, arguments), "claimed invariance")
})
