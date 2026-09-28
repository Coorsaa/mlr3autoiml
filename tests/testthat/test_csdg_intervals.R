test_that("the corrected resampled interval matches hand computations", {
  values = c(0.030, 0.034, 0.028, 0.036, 0.032)
  out = csdg_learner_pfi_interval(values, n_train = 800, n_test = 200)
  # mean 0.032; var = 4e-05 / 4 = 1e-05; corrected var = (1/5 + 200/800) * 1e-05 = 4.5e-06.
  expect_equal(out$estimate, 0.032)
  expect_equal(out$se, sqrt(4.5e-06))
  expect_equal(out$se, 0.00212132, tolerance = 1e-6)
  expect_identical(out$df, 4L)
  expect_identical(out$K, 5L)
  expect_equal(stats::qt(0.975, 4), 2.776445, tolerance = 1e-6)
  expect_equal(out$lower, 0.02611027, tolerance = 1e-6)
  expect_equal(out$upper, 0.03788973, tolerance = 1e-6)
  expect_equal(out$se_uncorrected, sqrt(2e-06))
  expect_equal(out$correction_factor, 2.25)
  expect_match(attr(out, "note"), "descriptively")

  ten = csdg_learner_pfi_interval(1:10, n_train = 900, n_test = 100)
  expect_equal(ten$estimate, 5.5)
  expect_equal(ten$se^2, (0.1 + 1 / 9) * stats::var(1:10))
  expect_equal(ten$se, 1.391109, tolerance = 1e-6)
  expect_identical(ten$df, 9L)
  expect_equal(ten$upper - ten$estimate, 2.262157 * 1.391109, tolerance = 1e-5)

  greater = csdg_learner_pfi_interval(values, n_train = 800, n_test = 200, alternative = "greater")
  expect_equal(greater$lower, 0.032 - stats::qt(0.95, 4) * sqrt(4.5e-06))
  expect_identical(greater$upper, Inf)
  less = csdg_learner_pfi_interval(values, n_train = 800, n_test = 200, alternative = "less")
  expect_identical(less$lower, -Inf)
  expect_equal(less$upper, 0.032 + stats::qt(0.95, 4) * sqrt(4.5e-06))
})

test_that("the corrected interval agrees with the formula in terms of the test fraction", {
  values = c(0.9, 1.4, 1.1, 0.7, 1.3, 1.0, 1.2, 0.8, 1.5, 1.1)
  n = 1000
  f = 0.1
  out = csdg_learner_pfi_interval(values, n_train = n * (1 - f), n_test = n * f)
  expect_equal(out$se, sqrt((1 / length(values) + f / (1 - f)) * stats::var(values)))
  expect_equal(csdg_learner_pfi_interval(values, n_train = rep(900, 10), n_test = rep(100, 10))$se, out$se)
})

test_that("the corrected interval validates its inputs", {
  expect_error(csdg_learner_pfi_interval(1, n_train = 10, n_test = 5), "length")
  expect_error(csdg_learner_pfi_interval(c(1, NA), n_train = 10, n_test = 5), "missing")
  expect_error(csdg_learner_pfi_interval(c(1, 2), n_train = 0, n_test = 5), "positive|>=|lower")
  expect_error(csdg_learner_pfi_interval(c(1, 2)), "n_train")
  expect_error(csdg_learner_pfi_interval(c(1, 2), n_train = 10, n_test = 5, level = 1), "level")
  degenerate = csdg_learner_pfi_interval(c(2, 2, 2), n_train = 10, n_test = 5)
  expect_identical(degenerate$se, 0)
  expect_identical(degenerate$lower, 2)
  expect_identical(degenerate$upper, 2)
})

test_that("the Monte Carlo rule matches hand computations", {
  x = c(2.00, 2.02, 1.98, 2.01, 1.99)
  y = c(0, 0.01, -0.01, 0.005, -0.005)
  out = csdg_pfi_mc_difference(x, y)
  expect_equal(out$estimate, 2)
  expect_equal(out$se, sqrt(2.5e-04 / 5 + 6.25e-05 / 5))
  expect_equal(out$se, 0.0079057, tolerance = 1e-6)
  expect_identical(out$df, 4L)
  expect_equal(out$threshold, 0.0219497, tolerance = 1e-5)
  expect_true(out$beyond_monte_carlo_error)
  expect_false(out$paired)

  d = c(0.003, 0.001, 0.002, 0.004, 0.000)
  paired = csdg_pfi_mc_difference(d)
  expect_equal(paired$estimate, 0.002)
  expect_equal(paired$se, 0.000707107, tolerance = 1e-5)
  expect_equal(paired$threshold, 0.00196324, tolerance = 1e-5)
  expect_true(paired$beyond_monte_carlo_error)
  expect_true(paired$paired)
  expect_equal(csdg_pfi_mc_difference(d + 1, rep(1, 5), paired = TRUE)$se, paired$se)
  noise = csdg_pfi_mc_difference(c(0.001, -0.001, 0.002, -0.002, 0))
  expect_false(noise$beyond_monte_carlo_error)
  expect_error(csdg_pfi_mc_difference(1, 2), "length")
  expect_error(csdg_pfi_mc_difference(1:3, 1:4, paired = TRUE), "equal length")
})

test_that("interval helpers accept held-out PFI results", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  oof = csdg_resample(fx$task, fx$learner, mlr3::rsmp("cv", folds = 3L), store_models = TRUE, seed = 3L)
  pfi = csdg_fold_pfi(oof, repetitions = 3L, seed = 4L)
  groups = unique(pfi$per_iteration$feature_group)
  interval = csdg_learner_pfi_interval(pfi, n_train = 60, feature_group = groups[[1L]], minus = groups[[2L]])
  first = pfi$per_iteration[feature_group == groups[[1L]]][order(iteration)]$importance
  second = pfi$per_iteration[feature_group == groups[[2L]]][order(iteration)]$importance
  expect_equal(interval$estimate, mean(first - second))
  expect_identical(interval$K, 3L)
  expect_equal(interval$n_test, 30)
  rows = csdg_pfi_mc_difference(pfi, groups[[1L]], groups[[2L]])
  expect_identical(rows$iteration, 1:3)
  raw = pfi$raw
  a = raw[iteration == 1L & feature_group == groups[[1L]], importance]
  b = raw[iteration == 1L & feature_group == groups[[2L]], importance]
  expect_equal(rows$estimate[[1L]], mean(a) - mean(b))
  expect_error(csdg_learner_pfi_interval(pfi, n_train = 60, feature_group = "unknown"), "not in the PFI")
})
