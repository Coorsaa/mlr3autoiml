make_model_diagnostic_fixture = function() {
  skip_if_not_installed("mlr3learners")
  set.seed(41L)
  data = data.frame(
    x1 = stats::runif(90L, -2, 2),
    x2 = stats::rnorm(90L),
    x3 = stats::runif(90L)
  )
  data$y = data$x1 * data$x2 + 0.3 * data$x3 + stats::rnorm(90L, sd = 0.05)
  task = mlr3::TaskRegr$new("csdg_model_diagnostics", backend = data, target = "y")
  learner = mlr3::lrn("regr.rpart", cp = 0.001, minsplit = 5L)
  learner$train(task)
  list(task = task, learner = learner)
}

test_that("effect diagnostics return labeled ALE, PDP, and ICE outputs", {
  fixture = make_model_diagnostic_fixture()
  set.seed(42L)
  random_seed = .Random.seed

  result = csdg_effect_diagnostics(
    fixture$task,
    fixture$learner,
    features = c("x1", "x2"),
    sample_n = 60L,
    ale_bins = 5L,
    grid_n = 5L,
    ice_keep_n = 6L,
    seed = 43L
  )

  expect_identical(.Random.seed, random_seed)
  expect_named(result, c("ale_1d", "pdp", "ice_private", "ice_quantiles", "ice_spread", "settings"))
  expect_setequal(unique(result$ale_1d$feature), c("x1", "x2"))
  expect_identical(unique(result$ice_private$privacy_scope), "private_row_level")
  expect_true(all(c("estimand", "limitation", "interpretation_scope") %in% names(result$pdp)))
})

test_that("effect support masking preserves counts and suppresses sparse estimates", {
  one_dimensional = data.frame(
    ale = c(0.1, 0.2),
    n_interval = c(2L, 8L),
    limitation = "original"
  )
  two_dimensional = data.frame(
    ale2d = c(0.3, 0.4),
    n_cell = c(3L, 9L),
    limitation = "original"
  )

  masked_1d = csdg_mask_effect_support(one_dimensional, min_n = 5L)
  masked_2d = csdg_mask_effect_support(two_dimensional, min_n = 5L)

  expect_true(is.na(masked_1d$ale[[1L]]))
  expect_equal(masked_1d$n_interval, c(2L, 8L))
  expect_identical(masked_1d$supported, c(FALSE, TRUE))
  expect_true(is.na(masked_2d$ale2d[[1L]]))
  expect_true(is.na(masked_2d$n_cell[[1L]]))
  expect_error(csdg_mask_effect_support(data.frame(value = 1), 5L), "must contain either")
})

test_that("interaction and Shapley diagnostics are reproducible fitted-model queries", {
  fixture = make_model_diagnostic_fixture()
  interaction = csdg_interaction_diagnostics(
    fixture$task,
    fixture$learner,
    features = c("x1", "x2", "x3"),
    sample_n = 60L,
    h_grid_n = 4L,
    ale_bins = 4L,
    seed = 44L
  )
  shapley_1 = csdg_shapley_diagnostics(
    fixture$task,
    fixture$learner,
    case_rows = fixture$task$row_ids[1:3],
    background_n = 20L,
    sample_size = 4L,
    seed = 45L,
    case_selection = "prespecified_external"
  )
  shapley_2 = csdg_shapley_diagnostics(
    fixture$task,
    fixture$learner,
    case_rows = fixture$task$row_ids[1:3],
    background_n = 20L,
    sample_size = 4L,
    seed = 45L,
    case_selection = "prespecified_external"
  )

  expect_equal(nrow(interaction$interaction_screen), 3L)
  expect_identical(interaction$interaction_screen$rank, seq_len(3L))
  expect_identical(shapley_1$local_private$phi, shapley_2$local_private$phi)
  expect_equal(nrow(shapley_1$additivity_private), 3L)
  expect_identical(unique(shapley_1$global$case_selection), "prespecified_external")
})

test_that("prediction multiplicity requires explicit near-equivalent learners", {
  fixture = make_model_diagnostic_fixture()
  learners = list(
    shallow = mlr3::lrn("regr.rpart", cp = 0.05),
    deep = mlr3::lrn("regr.rpart", cp = 0.001)
  )
  result = csdg_prediction_multiplicity(
    fixture$task,
    candidate_learners = learners,
    near_equivalent_ids = names(learners),
    row_ids = fixture$task$row_ids[1:40],
    seed = 46L,
    range_thresholds = c(0.1, 0.5),
    keep_private_predictions = FALSE
  )

  expect_null(result$private_predictions)
  expect_null(result$private_row_dispersion)
  expect_equal(nrow(result$distribution), 7L)
  expect_equal(nrow(result$pairwise), 1L)
  expect_equal(nrow(result$threshold_shares), 2L)
  expect_error(
    csdg_prediction_multiplicity(fixture$task, learners, near_equivalent_ids = "shallow"),
    "Must have length >= 2",
    fixed = TRUE
  )
})

test_that("model diagnostics reject learners that are not full fits", {
  fixture = make_model_diagnostic_fixture()
  partial = mlr3::lrn("regr.rpart")
  partial$train(fixture$task, row_ids = fixture$task$row_ids[-1L])

  expect_error(
    csdg_effect_diagnostics(
      fixture$task,
      partial,
      features = "x1",
      sample_n = 20L,
      ice_keep_n = 10L
    ),
    "must be a full fit"
  )
})
