make_ale_bootstrap_fixture = function() {
  skip_if_not_installed("rpart")
  set.seed(71L)
  data = data.frame(
    x1 = stats::runif(120L, -2, 2),
    x2 = stats::rnorm(120L),
    y = numeric(120L)
  )
  data$y = 1.5 * data$x1 + 0.4 * data$x2 + stats::rnorm(120L, sd = 0.25)
  task = mlr3::TaskRegr$new("ale_bootstrap", backend = data, target = "y")
  learner = mlr3::lrn("regr.rpart", cp = 0.005, minsplit = 8L)
  list(task = task, learner = learner)
}

test_that("ALE bootstrap refits models and returns reproducible pointwise intervals", {
  fixture = make_ale_bootstrap_fixture()
  set.seed(72L)
  random_seed = .Random.seed

  first = csdg_ale_bootstrap(
    fixture$task,
    fixture$learner,
    features = c("x1", "x2"),
    sample_n = 80L,
    ale_bins = 4L,
    replicates = 20L,
    min_interval_n = 2L,
    seed = 73L,
    keep_replicates = TRUE,
    verbose = FALSE
  )
  second = csdg_ale_bootstrap(
    fixture$task,
    fixture$learner,
    features = c("x1", "x2"),
    sample_n = 80L,
    ale_bins = 4L,
    replicates = 20L,
    min_interval_n = 2L,
    seed = 73L,
    keep_replicates = TRUE,
    verbose = FALSE
  )

  expect_identical(.Random.seed, random_seed)
  expect_equal(first$ale_1d, second$ale_1d)
  expect_equal(first$replicates_private, second$replicates_private)
  if (.Platform$OS.type != "windows") {
    parallel = csdg_ale_bootstrap(
      fixture$task,
      fixture$learner,
      features = c("x1", "x2"),
      sample_n = 80L,
      ale_bins = 4L,
      replicates = 20L,
      min_interval_n = 2L,
      seed = 73L,
      keep_replicates = TRUE,
      parallel_workers = 2L,
      verbose = FALSE
    )
    expect_equal(first$ale_1d, parallel$ale_1d)
    expect_equal(first$replicates_private, parallel$replicates_private)
  }
  expect_setequal(unique(first$ale_1d$feature), c("x1", "x2"))
  expect_true(all(c("ale_lower", "ale_upper", "n_bootstrap", "bootstrap_support_rate", "interval_supported") %in%
    names(first$ale_1d)))
  expect_true(all(first$ale_1d$supported))
  expect_true(all(first$ale_1d$interval_supported))
  expect_true(all(first$ale_1d$ale_lower <= first$ale_1d$ale_upper))
  expect_true(all(first$ale_1d$model_refit))
  expect_equal(unique(first$ale_1d$bootstrap_refits_successful), 20L)
  expect_equal(uniqueN(first$replicates_private$bootstrap_id), 20L)
  expect_identical(first$settings$interval_type, "pointwise percentile bootstrap")
})

test_that("ALE bootstrap resamples clusters independently within strata", {
  fixture = make_ale_bootstrap_fixture()
  cluster = rep(sprintf("cluster_%02d", 1:30), each = 4L)
  strata = rep(rep(c("a", "b"), each = 15L), each = 4L)

  result = csdg_ale_bootstrap(
    fixture$task,
    fixture$learner,
    features = "x1",
    sample_n = 80L,
    ale_bins = 4L,
    replicates = 20L,
    resampling_unit = "cluster",
    cluster = cluster,
    strata = strata,
    min_interval_n = 2L,
    seed = 74L,
    verbose = FALSE
  )

  expect_identical(unique(result$ale_1d$resampling_unit), "cluster within stratum")
  expect_true(result$settings$stratified)
  expect_null(result$replicates_private)
  expect_error(
    csdg_ale_bootstrap(
      fixture$task,
      fixture$learner,
      features = "x1",
      sample_n = 80L,
      replicates = 20L,
      resampling_unit = "cluster",
      cluster = cluster,
      strata = replace(strata, 1L, "b"),
      verbose = FALSE
    ),
    "must belong to one stratum"
  )
})

test_that("ALE bootstrap validates row and cluster resampling inputs", {
  fixture = make_ale_bootstrap_fixture()

  expect_error(
    csdg_ale_bootstrap(
      fixture$task,
      fixture$learner,
      features = "x1",
      sample_n = 80L,
      replicates = 19L,
      verbose = FALSE
    ),
    "replicates"
  )
  expect_error(
    csdg_ale_bootstrap(
      fixture$task,
      fixture$learner,
      features = "x1",
      sample_n = 80L,
      replicates = 20L,
      cluster = seq_len(fixture$task$nrow),
      verbose = FALSE
    ),
    "must be NULL"
  )
  expect_error(
    csdg_ale_bootstrap(
      fixture$task,
      fixture$learner,
      features = "x1",
      sample_n = 80L,
      replicates = 20L,
      resampling_unit = "cluster",
      verbose = FALSE
    ),
    "is required"
  )
  expect_error(
    csdg_ale_bootstrap(
      fixture$task,
      fixture$learner,
      features = "x1",
      sample_n = 40L,
      replicates = 20L,
      parallel_workers = 21L
    ),
    "parallel_workers"
  )
})
