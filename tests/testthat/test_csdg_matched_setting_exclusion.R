test_that("matched setting exclusion preserves assessment rows and analysis size", {
  skip_if_not_installed("rpart")
  set.seed(31L)
  n = 120L
  data = data.frame(
    x = rnorm(n),
    setting = rep(letters[1:4], each = 30L)
  )
  data$y = data$x + as.numeric(factor(data$setting)) + rnorm(n, sd = 0.2)
  task = mlr3::as_task_regr(data, target = "y")
  task$set_col_roles("setting", remove_from = "feature")
  learner = mlr3::lrn("regr.rpart")
  reference = csdg_resample(
    task,
    learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 32L
  )
  result = csdg_matched_setting_exclusion(
    reference,
    learner,
    group = data$setting,
    repetitions = 2L,
    seed = 33L
  )

  expect_equal(nrow(result$per_repetition), 8L)
  expect_true(all(result$per_repetition$n_assessment == 30L))
  expect_true(all(result$per_repetition$n_reference_analysis == 80L))
  expect_true(all(result$per_repetition$n_matched_exclusion_analysis == 80L))
  expect_true(all(is.finite(result$per_repetition$matched_exclusion_penalty)))
})
