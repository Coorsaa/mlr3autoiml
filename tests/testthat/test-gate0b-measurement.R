# FILE: tests/testthat/test-gate0b-measurement.R

test_that("Gate0BMeasurement warns for non-high-stakes missing reliability", {
  gate = mlr3autoiml:::Gate0BMeasurement$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(purpose = "global_insight", stakes = "medium"),
    measurement = list(level = "scale")
  )

  out = gate$run(ctx)
  expect_equal(out$status, "warn")
  expect_match(out$summary, "Measurement readiness screened", ignore.case = TRUE)
})

test_that("Gate0BMeasurement fails high-stakes missing reliability", {
  gate = mlr3autoiml:::Gate0BMeasurement$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(purpose = "deployment", stakes = "high"),
    measurement = list(level = "scale")
  )

  out = gate$run(ctx)
  expect_equal(out$status, "fail")
  expect_match(out$summary, "missing critical evidence", ignore.case = TRUE)
  expect_true(out$metrics$high_stakes[[1L]])
  expect_true(out$metrics$critical_missing_n[[1L]] >= 1L)
})

test_that("Gate0BMeasurement derives plausible-values level and pipeline notes", {
  gate = mlr3autoiml:::Gate0BMeasurement$new()
  ctx = list(
    task = make_task_mtcars_regr(),
    claim = list(purpose = "global_insight", stakes = "medium"),
    plausible_values = list(pv_tasks = list(make_task_mtcars_regr())),
    measurement = list()
  )

  out = gate$run(ctx)
  expect_equal(out$metrics$measurement_level[[1L]], "plausible_values")
  expect_equal(out$metrics$measurement_level_source[[1L]], "analysis")
  expect_true(out$metrics$has_missingness_plan[[1L]])
  expect_true(out$metrics$has_scoring_pipeline[[1L]])
  expect_true(all(c("level", "missingness_plan", "scoring_pipeline") %in% out$artifacts$derived_fields))
})

test_that("Gate0BMeasurement fails high-stakes subgroup use without invariance", {
  gate = mlr3autoiml:::Gate0BMeasurement$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(purpose = "decision_support", stakes = "high"),
    sensitive_features = "Species",
    measurement = list(
      level = "scale",
      reliability = list(alpha = 0.85),
      missingness_plan = "MAR with chained equations imputation nested in folds",
      scoring_pipeline = "reverse coding + scale construction inside resampling"
    )
  )

  out = gate$run(ctx)
  expect_equal(out$status, "fail")
  expect_match(paste(out$messages, collapse = " "), "invariance|comparability", ignore.case = TRUE)
})

test_that("Gate0BMeasurement does not require invariance for item-level subgroup audits", {
  gate = mlr3autoiml:::Gate0BMeasurement$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(purpose = "decision_support", stakes = "high"),
    sensitive_features = "Species",
    measurement = list(level = "item")
  )

  out = gate$run(ctx)
  expect_false("invariance" %in% out$artifacts$critical_missing)
})

test_that("Gate0BMeasurement passes high-stakes when critical evidence is present", {
  gate = mlr3autoiml:::Gate0BMeasurement$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(purpose = "deployment", stakes = "high"),
    sensitive_features = "Species",
    measurement = list(
      level = "scale",
      reliability = list(alpha = 0.90),
      invariance = list(multigroup_cfa = "configural/metric supported"),
      missingness_plan = "MAR with chained equations imputation nested in folds",
      scoring_pipeline = "reverse coding + scale construction inside resampling"
    )
  )

  out = gate$run(ctx)
  expect_equal(out$status, "pass")
  expect_equal(out$metrics$critical_missing_n[[1L]], 0)
})
