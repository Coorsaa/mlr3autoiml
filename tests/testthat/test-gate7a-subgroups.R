# FILE: tests/testthat/test-gate7a-subgroups.R

test_that("Gate7aSubgroups skips exploratory use without subgroup declarations", {
  gate = mlr3autoiml:::Gate7aSubgroups$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(
      purpose = "exploratory",
      stakes = "low",
      claims = list(global = TRUE, local = FALSE, decision = FALSE)
    )
  )

  out = gate$run(ctx)
  expect_equal(out$status, "skip")
})

test_that("Gate7aSubgroups warns non-exploratory use without subgroup declarations", {
  gate = mlr3autoiml:::Gate7aSubgroups$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(
      purpose = "global_insight",
      stakes = "medium",
      claims = list(global = TRUE, local = FALSE, decision = FALSE)
    )
  )

  out = gate$run(ctx)
  expect_equal(out$status, "warn")
})

test_that("Gate7aSubgroups fails high-stakes use without subgroup declarations", {
  gate = mlr3autoiml:::Gate7aSubgroups$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(
      purpose = "deployment",
      stakes = "high",
      claims = list(global = TRUE, local = TRUE, decision = TRUE)
    )
  )

  out = gate$run(ctx)
  expect_equal(out$status, "fail")
  expect_match(out$summary, "requires declared subgroup variables", ignore.case = TRUE)
  expect_true(isTRUE(out$metrics$required[[1L]]))
  expect_true(isFALSE(out$metrics$provided[[1L]]))
})
