loco_task = function(seed) {
  set.seed(seed)
  n = 1000L
  dat = data.frame(a = stats::rnorm(n), b = stats::rnorm(n), c = stats::rnorm(n))
  dat$d = dat$c + stats::rnorm(n, sd = 0.05)
  dat$y = 2 * dat$a + dat$b + dat$c + stats::rnorm(n)
  mlr3::as_task_regr(dat, target = "y", id = paste0("loco_", seed))
}

loco_fits = function() {
  testthat::skip_if_not_installed("mlr3learners")
  requireNamespace("mlr3learners", quietly = TRUE)
  csdg_fit(loco_task(11L), list(lm = mlr3::lrn("regr.lm")), folds = 5L, measure = "regr.mse", seed = 1L)
}

test_that("csdg_loco() is the increase in held-out loss after refitting without each predictor", {
  fits = loco_fits()
  loco = csdg_loco(fits, groups = list(cd = c("c", "d")))
  expect_s3_class(loco, c("CSDGLoco", "CSDGImportance"))
  expect_identical(loco$quantity, "loco")
  expect_identical(loco$predictors, c("a", "b", "c", "d"))
  dat = fits$task$data()
  train = fits$resamples$lm$train_sets[[1L]]
  test = fits$resamples$lm$test_sets[[1L]]
  mse = function(model) mean((dat$y[test] - stats::predict(model, dat[test]))^2)
  full = mse(stats::lm(y ~ a + b + c + d, data = dat[train]))
  reduced = mse(stats::lm(y ~ b + c + d, data = dat[train]))
  expect_equal(loco$fold_pfi[feature_group == "a" & iteration == 1L, importance], reduced - full, tolerance = 1e-8)
  expect_equal(loco$refit_losses[feature_group == "a" & iteration == 1L, loss_full], full, tolerance = 1e-8)
  tab = as.data.table(loco, interval = TRUE)
  expect_identical(tab[feature_group == "a", type], "loco")
  expect_identical(tab[feature_group == "cd", type], "grouped")
  expect_lt(tab[feature_group == "c", mean_importance], 0.05)
  expect_gt(tab[feature_group == "cd", mean_importance], 0.5)
  ci = csdg_learner_pfi_interval(loco$fold_pfi[feature_group == "a", importance], n_train = fits$n_train,
    n_test = fits$n_test)
  expect_equal(tab[feature_group == "a", lower], ci$lower)
  expect_output(print(loco), "<CSDGLoco> LOCO, increase in held-out squared error after refitting")
  expect_output(print(loco), "grouped: cd \\(c, d\\)")
  expect_s3_class(plot(loco), "ggplot")
  expect_identical(csdg_loco(fits, predictors = c("b", "a"))$predictors, c("a", "b"))
  expect_error(csdg_loco(fits, predictors = "z"), "Unknown predictors")
  expect_error(csdg_loco(fits, groups = list(all = c("a", "b", "c", "d"))), "every predictor")
  expect_error(csdg_importance(loco), "csdg_loco")
})

test_that("claims derived from LOCO are worded, checked, and tested on new data with LOCO", {
  fits = loco_fits()
  loco = csdg_loco(fits)
  csdg_claim_log_reset()
  claim = claim_order(loco, "a", "b")
  expect_identical(claim$statement,
    "In refits without each predictor (LOCO), linear regression relies more on a than on b.")
  expect_identical(claim$source$quantity, "loco")
  expect_match(claim$scope$quantity, "^Learner LOCO of each predictor")
  expect_match(claim$scope$procedure, "one refit without each predictor")
  chk = csdg_check(claim, loco, judgments = g0b_judged())
  expect_identical(chk$explanation$method_ids, "loco")
  expect_identical(csdg_assess(chk)$assessment, "met")
  expect_equal(chk$details$content$lm$margins$mc_threshold, 0)
  expect_match(chk$table[check == "procedure", observation], "^LOCO with squared error")
  expect_false(grepl("Monte Carlo", chk$table[check == "content", criterion]))
  expect_identical(chk$table[check == "dependence", role], "context")
  expect_pass_through(chk)
  expect_identical(unique(csdg_claim_log()$quantity), "loco")
  expect_identical(claim_top_k(loco, k = 1)$predictors, "a")
  expect_match(claim_top_k(loco, k = 2)$statement, "^In refits without each predictor \\(LOCO\\), the two predictors")
  expect_error(claim_relies_mainly(csdg_loco(fits, predictors = c("a", "b"))), "compares every predictor")
  expect_error(claim_order(loco, "a", "b", marginal_only = TRUE), "marginal_only")
  imp = csdg_importance(fits, repetitions = 2L, seed = 2L)
  expect_error(csdg_check(claim, imp), "derived from LOCO")
  expect_error(csdg_check(claim_order(imp, "a", "b"), loco), "derived from PFI")
  conf = csdg_confirm(chk, loco_task(12L))
  expect_identical(conf$check$source$quantity, "loco")
  expect_identical(conf$label, "met (tested on new data)")
})

# The importance fixture as LOCO: one value per fold, the center of the permutation values.
loco_fixture = function(centers = fixture_centers()) {
  imp = make_importance_fixture(centers = list(m1 = centers))
  imp$pfi$m1$raw = imp$pfi$m1$raw[permutation_repetition == 2L]
  imp$quantity = "loco"
  imp$repetitions = 1L
  imp$predictors = imp$fits$features
  imp$fold_pfi[type == "marginal", type := "loco"]
  imp$summary[type == "marginal", type := "loco"]
  imp
}

test_that("a LOCO margin fails a fold without Monte Carlo error", {
  centers = fixture_centers()
  centers[3L, c("a", "b")] = c(0.10, 0.30)
  imp = loco_fixture(centers)
  claim = suppressWarnings(claim_order(imp, "a", "b"))
  chk = suppressWarnings(csdg_check(claim, imp, stability = 1))
  folds = chk$details$stability$m1$folds
  expect_identical(folds$class[[3L]], "fails")
  expect_equal(folds$threshold[[3L]], 0)
  expect_identical(chk$table[check == "stability", status], "contradicted")
  expect_match(chk$table[check == "stability", observation], "1 fails by at least the smallest relevant difference")
})

test_that("a LOCO ranking claim compares every other predictor and names LOCO", {
  imp = loco_fixture()
  chk = csdg_check(claim_relies_mainly(imp, k = 1), imp, judgments = g0b_judged())
  comparisons = chk$details$content$m1$comparisons
  expect_identical(comparisons$versus, c("b", "c", "d"))
  expect_identical(unique(comparisons$mc_threshold), 0)
  expect_identical(comparisons$status, c("contradicted", "supported", "supported"))
  res = csdg_assess(chk)
  expect_identical(res$assessment, "not_met")
  expect_match(res$hints, "a has 1.50 times the LOCO of the largest other predictor \\(claimed: 2\\)")
})
