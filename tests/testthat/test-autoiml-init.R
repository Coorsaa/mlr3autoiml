# FILE: tests/testthat/test-autoiml-init.R

test_that("AutoIML initializes and applies config", {
  task = make_task_iris()
  learner = make_learner_classif_rpart()
  resampling = make_resampling_cv(folds = 3L)

  auto = mlr3autoiml::AutoIML$new(
    task = task,
    learner = learner,
    resampling = resampling,
    purpose = "global_insight",
    quick_start = TRUE,
    seed = 1L,
    config = list(
      structure = list(sample_n = 75L, max_features = 2L),
      calibration = list(n_bins = 8L)
    )
  )

  expect_true(inherits(auto, "AutoIML"))
  expect_true(is.environment(auto$ctx))
  expect_true(is.list(auto$ctx$structure))
  expect_equal(auto$ctx$structure$sample_n, 75L)
  expect_equal(auto$ctx$structure$max_features, 2L)
  expect_true(is.list(auto$ctx$calibration))
  expect_equal(auto$ctx$calibration$n_bins, 8L)

  # wrapper should return AutoIMLResult
  res = mlr3autoiml::autoiml(
    task = task,
    learner = learner,
    resampling = resampling,
    purpose = "global_insight",
    quick_start = TRUE,
    seed = 1L,
    config = list(structure = list(sample_n = 50L))
  )
  expect_true(inherits(res, "AutoIMLResult"))
})
