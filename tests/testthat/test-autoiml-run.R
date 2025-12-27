# FILE: tests/testthat/test-autoiml-run.R

test_that("AutoIML run returns AutoIMLResult with gate results and report card", {
  auto = get_auto_iris(quick_start = FALSE)
  result = auto$result

  expect_true(inherits(result, "AutoIMLResult"))
  expect_true(is.list(result$gate_results))
  expect_true(all(c("G1", "G2", "G3", "G4") %in% names(result$gate_results)))

  # report card shape
  rc = result$report_card
  expect_true(data.table::is.data.table(rc))
  expect_true(all(c("gate_id", "gate_name", "pdr", "status", "summary", "irl", "purpose") %in% names(rc)))
  expect_gte(nrow(rc), 4L)

  # ctx should contain final model
  expect_true(!is.null(auto$ctx$final_model))
  expect_true(inherits(auto$ctx$final_model, "Learner"))

  # Gate 1 should create out-of-fold predictions for downstream gates
  # (requires store_backends = TRUE for future-proofing across mlr3 versions)
  expect_true(!is.null(auto$ctx$pred))
  expect_true(inherits(auto$ctx$pred, "Prediction"))
})
