test_that("nonlinear local comparator is deterministic and scores the target case", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task,
    fx$learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  arguments = list(
    x = out,
    cases = c(3L, 28L),
    seeds = c(9L, 10L),
    kernel_width = 0.75,
    n_perturb = 50L
  )

  first = do.call(csdg_local_tree_comparator, arguments)
  second = do.call(csdg_local_tree_comparator, arguments)

  expect_identical(first$replicates, second$replicates)
  expect_equal(nrow(first$replicates), 4L)
  expect_equal(nrow(first$cases), 2L)
  expect_true(all(is.finite(first$replicates$target_case_absolute_error)))
  expect_true(all(first$replicates$target_case_absolute_error >= 0))
  expect_true(nrow(first$importance_stability) > 0L)
  expect_identical(first$evaluation$surrogate_family, "pre-pruned regression tree")
})

test_that("nonlinear local comparator supports empirical neighborhoods and link scale", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task,
    fx$learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  result = csdg_local_tree_comparator(
    out,
    cases = 3L,
    seeds = c(9L, 10L),
    n_perturb = 50L,
    target_scale = "link",
    neighborhood_method = "empirical_knn",
    empirical_neighbors = 30L
  )

  expect_true(all(result$replicates$target_scale == "link"))
  expect_true(all(result$replicates$neighborhood_method == "empirical_knn"))
})
