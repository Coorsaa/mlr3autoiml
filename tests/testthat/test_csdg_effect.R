test_that("the data-weighted center weights each interval by its number of rows", {
  expect_equal(.csdg_ale_center(c(0, 1, 3), c(1L, 2L, 2L, 2L)), (1 * 0.5 + 3 * 2) / 4)
  expect_equal(.csdg_ale_center(c(0, 2, 4), c(rep(1L, 9L), 2L)), (9 * 1 + 1 * 3) / 10)
  expect_false(isTRUE(all.equal(.csdg_ale_center(c(0, 2, 4), c(rep(1L, 9L), 2L)), mean(c(0, 2, 4)))))
})

test_that("csdg_effect() centers each held-out ALE curve on the data", {
  testthat::skip_if_not_installed("mlr3learners")
  requireNamespace("mlr3learners", quietly = TRUE)
  set.seed(2)
  n = 300L
  dat = data.frame(x = as.numeric(sample(0:2, n, TRUE, prob = c(0.7, 0.2, 0.1))), w = stats::rnorm(n))
  dat$y = 2 * dat$x + stats::rnorm(n)
  task = mlr3::as_task_regr(dat, target = "y", id = "discrete")
  fits = csdg_fit(task, list(lm = mlr3::lrn("regr.lm")), folds = 3L, seed = 1L)
  eff = csdg_effect(fits, "x")
  for (k in seq_len(fits$K)) {
    curve = eff$curves[iteration == k]
    x_test = dat$x[fits$resamples$lm$test_sets[[k]]]
    interval = pmax(1L, findInterval(x_test, curve$x, rightmost.closed = TRUE, left.open = TRUE))
    counts = tabulate(interval, nbins = nrow(curve) - 1L)
    midpoints = (curve$ale[-nrow(curve)] + curve$ale[-1L]) / 2
    expect_equal(sum(counts * midpoints), 0, tolerance = 1e-10)
    expect_gt(abs(mean(curve$ale)), 0.1)
  }
  slope = vapply(fits$resamples$lm$models, function(model) stats::coef(model$model)[["x"]], numeric(1L))
  expect_equal(eff$curves[iteration == 1L, diff(ale)], rep(slope[[1L]], 2L), tolerance = 1e-10)
})

test_that("the largest decrease and increase are taken between adjacent grid points within the percentiles", {
  breaks = c(0, 1, 2, 3, 4)
  accumulated = c(0, 1, 0.7, 1.5, 2)
  quantiles = c(0.5, 3.5)
  at = stats::approx(breaks, accumulated, xout = quantiles)$y
  steps = .csdg_ale_steps(breaks, accumulated, quantiles, at)
  expect_equal(steps, c(0.5, -0.3, 0.8, 0.25))
  expect_equal(.csdg_ale_steps(breaks, c(0, -2, 1, 2, 3), c(1, 3.5), c(-2, 2.5)), c(3, 1, 0.5))
  testthat::skip_if_not_installed("mlr3learners")
  requireNamespace("mlr3learners", quietly = TRUE)
  set.seed(3)
  dat = data.frame(x = stats::rnorm(200L), w = stats::rnorm(200L))
  dat$y = 2 * dat$x + stats::rnorm(200L)
  fits = csdg_fit(mlr3::as_task_regr(dat, target = "y", id = "line"), list(lm = mlr3::lrn("regr.lm")), folds = 2L,
    seed = 1L)
  eff = csdg_effect(fits, "x")
  expect_equal(eff$changes$max_decrease, c(0, 0))
  expect_true(all(eff$changes$max_increase > 0))
  expect_equal(eff$summary$max_increase, max(eff$changes$max_increase))
})
