# FILE: tests/testthat/test-gate7b-human-factors.R

test_that("Gate7bHumanFactors skips non-high-stakes uses", {
  gate = mlr3autoiml:::Gate7bHumanFactors$new()
  ctx = list(
    claim = list(
      purpose = "global_insight",
      stakes = "medium",
      claims = list(decision = FALSE)
    )
  )

  out = gate$run(ctx)
  expect_s3_class(out, "R6")
  expect_equal(out$status, "skip")
})

test_that("Gate7bHumanFactors fails high-stakes uses without evidence", {
  gate = mlr3autoiml:::Gate7bHumanFactors$new()
  ctx = list(
    claim = list(
      purpose = "deployment",
      stakes = "high",
      claims = list(decision = TRUE)
    )
  )

  out = gate$run(ctx)
  expect_equal(out$status, "fail")
  expect_match(out$summary, "requires schema-complete human-factors evidence", ignore.case = TRUE)
  expect_true(isTRUE(out$metrics$required[[1L]]))
  expect_true(isFALSE(out$metrics$provided[[1L]]))
})

test_that("Gate7bHumanFactors passes high-stakes uses with evidence", {
  gate = mlr3autoiml:::Gate7bHumanFactors$new()
  ctx = list(
    claim = list(
      purpose = "decision_support",
      stakes = "high",
      claims = list(decision = TRUE),
      human_factors_evidence = list(
        participants = "n=120 clinicians",
        task_design = "pre-registered A/B error-detection study",
        baseline = "no-explanation arm",
        outcomes = "decision quality + calibrated reliance",
        analysis_summary = "mixed-effects model with prespecified endpoints"
      )
    )
  )

  out = gate$run(ctx)
  expect_equal(out$status, "pass")
  expect_match(out$summary, "evidence provided", ignore.case = TRUE)
  expect_true(isTRUE(out$metrics$required[[1L]]))
  expect_true(isTRUE(out$metrics$provided[[1L]]))
  expect_true(is.list(out$artifacts$human_factors_evidence))
})
