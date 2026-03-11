# FILE: tests/testthat/test-autoiml-run.R

test_that("AutoIML run returns AutoIMLResult with gate results and report card", {
  auto = get_auto_iris(quick_start = FALSE)
  result = auto$result

  expect_true(inherits(result, "AutoIMLResult"))
  expect_true(is.list(result$gate_results))

  executed = names(result$gate_results)
  expect_true(all(c("G0A", "G0B", "G1", "G2", "G5", "G6", "G7A") %in% executed))
  expect_false(any(c("G3", "G4", "G7B") %in% executed))

  # report_card is a method, not a field
  rc = result$report_card()
  expect_true(data.table::is.data.table(rc))
  expect_true(all(c("gate_id", "gate_name", "pdr", "status", "summary", "iel_overall", "purpose") %in% names(rc)))
  expect_gte(nrow(rc), 5L)

  # ctx should contain final model
  expect_true(!is.null(auto$ctx$final_model))
  expect_true(inherits(auto$ctx$final_model, "Learner"))

  # Gate 1 should create out-of-fold predictions for downstream gates
  expect_true(!is.null(auto$ctx$pred))
  expect_true(inherits(auto$ctx$pred, "Prediction"))
})
