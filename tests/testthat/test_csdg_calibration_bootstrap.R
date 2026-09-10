test_that("calibration bootstrap reports reproducible respondent intervals", {
  set.seed(100L)
  n = 240L
  probability = runif(n, 0.05, 0.95)
  prediction = data.frame(
    row_id = seq_len(n),
    truth = factor(rbinom(n, 1L, probability), levels = c(0, 1)),
    prob.0 = 1 - probability,
    prob.1 = probability
  )

  first = csdg_calibration_bootstrap(
    prediction,
    task_type = "classif",
    positive = "1",
    repetitions = 30L,
    grid_points = 25L,
    seed = 11L
  )
  second = csdg_calibration_bootstrap(
    prediction,
    task_type = "classif",
    positive = "1",
    repetitions = 30L,
    grid_points = 25L,
    seed = 11L
  )

  expect_equal(first$summary, second$summary)
  expect_equal(first$curve, second$curve)
  expect_true(all(c("calibration_intercept", "calibration_slope") %in% first$summary$estimand))
  canonical_name = "uniform_grid_mean_absolute_calibration_error"
  deprecated_name = "integrated_absolute_calibration_error"
  canonical = first$summary[estimand == canonical_name]
  deprecated = first$summary[estimand == deprecated_name]
  expect_equal(deprecated[, .(estimate, lower, upper)], canonical[, .(estimate, lower, upper)])
  expect_identical(canonical$canonical_estimand, canonical_name)
  expect_identical(canonical$estimand_status, "canonical")
  expect_identical(deprecated$canonical_estimand, canonical_name)
  expect_identical(deprecated$estimand_status, "deprecated_alias")
  expect_equal(
    canonical$estimate,
    mean(abs(first$curve$estimate - first$curve$prediction), na.rm = TRUE)
  )
  expect_equal(first$bootstrap_summary[[deprecated_name]], first$bootstrap_summary[[canonical_name]])
  expect_true(canonical_name %in% first$estimands$estimand)
  expect_false(deprecated_name %in% first$estimands$estimand)
  expect_identical(
    first$metric_aliases,
    data.table::data.table(
      alias = deprecated_name,
      canonical_estimand = canonical_name,
      alias_status = "deprecated_alias"
    )
  )
  expect_identical(first$grid_specification$points, 25L)
  expect_identical(first$grid_specification$lower_quantile_probability, 0.01)
  expect_identical(first$grid_specification$upper_quantile_probability, 0.99)
  expect_identical(first$grid_specification$quantile_type, 8L)
  expect_identical(first$grid_specification$weighting, "uniform_discrete")
  expect_equal(nrow(first$curve), 25L)
  expect_identical(first$resampling$unit, "respondent")
})

test_that("calibration bootstrap samples nested clusters within strata", {
  set.seed(101L)
  n = 240L
  prediction = data.frame(
    row_id = seq_len(n),
    truth = rnorm(n),
    response = rnorm(n),
    school = rep(seq_len(24L), each = 10L),
    country = rep(rep(c("a", "b"), each = 12L), each = 10L)
  )
  result = csdg_calibration_bootstrap(
    prediction,
    task_type = "regr",
    cluster = "school",
    strata = "country",
    repetitions = 20L,
    grid_points = 20L,
    seed = 12L
  )

  expect_identical(result$resampling$unit, "cluster")
  expect_true(result$resampling$stratified)
  expect_equal(unique(result$bootstrap_summary$n), n)
  expect_true(all(is.finite(result$summary$estimate)))
})

test_that("calibration bootstrap retains singleton clusters in every stratum", {
  sampler = mlr3autoiml:::.calibration_bootstrap_index_sampler(
    4L,
    cluster = c("a1", "a1", "b1", "b1"),
    strata = c("a", "a", "b", "b")
  )
  set.seed(99L)
  draws = replicate(100L, sampler(), simplify = FALSE)

  expect_true(all(vapply(draws, identical, logical(1L), seq_len(4L))))
})

test_that("calibration bootstrap preserves mixed singleton and multiple-cluster strata", {
  cluster = rep(c("a1", "a2", "b1", "c1", "c2"), each = 2L)
  strata = rep(c("a", "a", "b", "c", "c"), each = 2L)
  sampler = mlr3autoiml:::.calibration_bootstrap_index_sampler(length(cluster), cluster, strata)
  set.seed(100L)
  draws = replicate(100L, sampler(), simplify = FALSE)
  stratum_counts = vapply(draws, function(index) {
    tabulate(match(strata[index], c("a", "b", "c")), nbins = 3L)
  }, integer(3L))

  expect_identical(stratum_counts, matrix(rep(c(4L, 2L, 4L), 100L), nrow = 3L))
  expect_true(all(vapply(draws, function(index) {
    identical(index[strata[index] == "b"], c(5L, 6L))
  }, logical(1L))))
})

test_that("calibration bootstrap preindexes clusters without changing sampled rows", {
  cluster = rep(c("a1", "a2", "b1", "b2"), times = c(2L, 3L, 2L, 3L))
  strata = rep(c("a", "a", "b", "b"), times = c(2L, 3L, 2L, 3L))
  legacy_indices = function() {
    cluster_stratum = unique(data.table::data.table(stratum = strata, cluster = cluster))
    sampled = cluster_stratum[, .(sampled_cluster = sample(cluster, .N, replace = TRUE)), by = stratum]
    unlist(lapply(seq_len(nrow(sampled)), function(index) {
      which(strata == sampled$stratum[[index]] & cluster == sampled$sampled_cluster[[index]])
    }), use.names = FALSE)
  }

  set.seed(913L)
  expected = legacy_indices()
  set.seed(913L)
  observed = mlr3autoiml:::.calibration_bootstrap_indices(length(cluster), cluster, strata)

  expect_identical(observed, expected)
})

test_that("parallel calibration evaluation preserves serial results", {
  skip_if_not(identical(.Platform$OS.type, "unix"))
  set.seed(102L)
  prediction = data.frame(
    row_id = seq_len(240L),
    truth = rnorm(240L),
    response = rnorm(240L),
    school = rep(seq_len(24L), each = 10L),
    country = rep(rep(c("a", "b"), each = 12L), each = 10L)
  )
  arguments = list(
    predictions = prediction,
    task_type = "regr",
    cluster = "school",
    strata = "country",
    repetitions = 20L,
    grid_points = 20L,
    seed = 13L
  )

  serial = do.call(csdg_calibration_bootstrap, c(arguments, list(workers = 1L)))
  parallel = do.call(csdg_calibration_bootstrap, c(arguments, list(workers = 2L)))

  expect_equal(parallel$summary, serial$summary)
  expect_equal(parallel$curve, serial$curve)
  expect_equal(parallel$bootstrap_summary, serial$bootstrap_summary)
  expect_identical(parallel$resampling$workers_used, 2L)
})

test_that("calibration bootstrap supports prespecified natural regression splines", {
  set.seed(103L)
  prediction = data.frame(
    row_id = seq_len(240L),
    truth = rnorm(240L),
    response = rnorm(240L)
  )
  result = csdg_calibration_bootstrap(
    prediction,
    task_type = "regr",
    repetitions = 20L,
    grid_points = 20L,
    curve_method = "regression_spline",
    spline_df = 4L,
    seed = 14L
  )

  expect_true(all(is.finite(result$curve$estimate)))
  expect_identical(unique(result$curve$curve_method), "regression_spline")
  expect_identical(unique(result$curve$spline_df), 4L)
  expect_identical(result$resampling$curve_method, "regression_spline")
  expect_identical(result$grid_specification$lower_quantile_probability, 0.005)
  expect_identical(result$grid_specification$upper_quantile_probability, 0.995)
})

test_that("calibration bootstrap rejects clusters that cross strata", {
  prediction = data.frame(
    row_id = 1:40,
    truth = rnorm(40L),
    response = rnorm(40L),
    cluster = rep(1:4, each = 10L),
    stratum = rep(c("a", "b"), each = 20L)
  )
  prediction$stratum[c(1L, 11L, 21L, 31L)] = c("b", "b", "a", "a")
  expect_error(
    csdg_calibration_bootstrap(
      prediction,
      task_type = "regr",
      cluster = "cluster",
      strata = "stratum",
      repetitions = 20L
    ),
    "nested"
  )
})
