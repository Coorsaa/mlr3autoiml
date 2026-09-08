test_that("local-fidelity randomness audit separates a complete crossed seed grid", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task,
    fx$learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  cases = c(3L, 28L)
  perturbation_seeds = matrix(c(11L, 12L, 21L, 22L), nrow = 2L, byrow = TRUE)
  arguments = list(
    x = out,
    cases = cases,
    perturbation_seeds = perturbation_seeds,
    crossfit_seeds = c(31L, 32L, 33L),
    n_perturb = 60L,
    case_labels = c("case_003", "case_028"),
    case_metadata = data.frame(prediction_score_decile = c(1L, 9L))
  )

  set.seed(731L)
  before = .Random.seed
  first = do.call(csdg_local_fidelity_randomness_audit, arguments)
  after = .Random.seed
  second = do.call(csdg_local_fidelity_randomness_audit, arguments)

  expect_identical(after, before)
  expect_identical(first, second)
  expect_equal(nrow(first$grid), 12L)
  expect_equal(nrow(first$cases), 10L)
  expect_equal(nrow(first$summary), 5L)
  expect_equal(nrow(first$support), 2L)
  expect_identical(first$summary$metric, c(
    "weighted_r2", "weighted_rmse", "weighted_mae", "maximum_absolute_error",
    "target_case_absolute_error"
  ))
  expect_equal(
    data.table::uniqueN(
      first$grid,
      by = c("case_label", "perturbation_seed_index", "crossfit_seed_index")
    ),
    12L
  )
  expect_identical(first$grid$perturbation_seed, c(
    rep(11L, 3L), rep(12L, 3L), rep(21L, 3L), rep(22L, 3L)
  ))
  expect_identical(first$grid$crossfit_seed, rep(c(31L, 32L, 33L), 4L))
  expect_identical(first$cases$prediction_score_decile, rep(c(1L, 9L), each = 5L))

  source = first$grid[case_label == "case_003", .(
    perturbation_seed_index,
    crossfit_seed_index,
    value = weighted_r2
  )]
  grand_mean = mean(source$value)
  perturbation_means = source[, .(mean = mean(value)), by = perturbation_seed_index]
  crossfit_means = source[, .(mean = mean(value)), by = crossfit_seed_index]
  source = merge(source, perturbation_means, by = "perturbation_seed_index", sort = FALSE)
  data.table::setnames(source, "mean", "perturbation_mean")
  source = merge(source, crossfit_means, by = "crossfit_seed_index", sort = FALSE)
  data.table::setnames(source, "mean", "crossfit_mean")
  expected = first$cases[case_label == "case_003" & metric == "weighted_r2"]
  expect_equal(expected$perturbation_sum_squares, 3 * sum((perturbation_means$mean - grand_mean)^2))
  expect_equal(expected$crossfit_sum_squares, 2 * sum((crossfit_means$mean - grand_mean)^2))
  expect_equal(
    expected$interaction_sum_squares,
    sum((source$value - source$perturbation_mean - source$crossfit_mean + grand_mean)^2)
  )
  expect_equal(
    expected$total_sum_squares,
    expected$perturbation_sum_squares + expected$crossfit_sum_squares + expected$interaction_sum_squares
  )
  expect_equal(
    expected$perturbation_sum_squares_fraction + expected$crossfit_sum_squares_fraction +
      expected$interaction_sum_squares_fraction,
    1
  )
})

test_that("local-fidelity randomness audit rejects malformed seed grids", {
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
    cases = 3L,
    perturbation_seeds = c(11L, 12L),
    crossfit_seeds = c(31L, 32L),
    n_perturb = 60L
  )

  expect_error(
    do.call(csdg_local_fidelity_randomness_audit, modifyList(arguments, list(perturbation_seeds = 11L))),
    "length >= 2|at least two"
  )
  expect_error(
    do.call(csdg_local_fidelity_randomness_audit, modifyList(arguments, list(crossfit_seeds = c(31L, 31L)))),
    "duplicated|distinct"
  )
  expect_error(
    do.call(csdg_local_fidelity_randomness_audit, modifyList(
      arguments,
      list(perturbation_seeds = matrix(11:14, nrow = 2L))
    )),
    "one row per requested case"
  )
  expect_error(
    do.call(csdg_local_fidelity_randomness_audit, modifyList(
      arguments,
      list(case_metadata = data.frame(metric = "weighted_r2"))
    )),
    "reserved columns"
  )
})
