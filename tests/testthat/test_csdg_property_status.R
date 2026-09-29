test_that("status constructors return the property statuses", {
  expect_identical(supported(), "supported")
  expect_identical(contradicted(), "contradicted")
  expect_identical(open_status(), "open")
  expect_setequal(c(supported(), contradicted(), open_status()), .csdg_property_statuses)
  expect_error(supported("open"))
  expect_error(.csdg_property_status("unresolved"), "status")
})

test_that("status constructors do not mask base::open", {
  exports = getNamespaceExports("mlr3autoiml")
  expect_true(all(c("supported", "contradicted", "open_status") %in% exports))
  expect_false("open" %in% exports)
})

test_that("status constructors are accepted by evidence records and the decision rule", {
  record = function(gate_id, status) {
    csdg_evidence_record(
      gate_id, TRUE, "required_property", status = status,
      rationale = "Illustration.", required_property = "Illustrative property.", observation = "Illustration.",
      relevance_to_proposition = "Illustration."
    )
  }
  met = csdg_adjudicate_claim(list(record("G2", supported()), record("G5", supported())), claim_applicable = TRUE)
  open = csdg_adjudicate_claim(list(record("G2", supported()), record("G5", open_status())), claim_applicable = TRUE)
  not_met = csdg_adjudicate_claim(list(record("G2", contradicted()), record("G5", open_status())),
    claim_applicable = TRUE)
  expect_identical(met$assessment, "met")
  expect_identical(open$assessment, "unresolved")
  expect_identical(not_met$assessment, "not_met")
})
