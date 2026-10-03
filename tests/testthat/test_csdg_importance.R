rpart_fits = function() {
  testthat::skip_if_not_installed("rpart")
  csdg_fit(make_task_mtcars_regr(), list(tree = mlr3::lrn("regr.rpart")), folds = 3L, seed = 1L)
}

test_that("held-out losses equal the unpermuted losses of csdg_fold_pfi()", {
  fits = rpart_fits()
  imp = csdg_importance(fits, repetitions = 2L, seed = 2L)
  baseline_loss = unique(imp$pfi$tree$per_iteration[, .(iteration, baseline_loss)])
  expect_equal(imp$performance$loss, baseline_loss$baseline_loss, tolerance = 1e-12)
  expect_identical(imp$loss, "rmse")
  expect_equal(imp$performance$improvement, imp$performance$loss_baseline - imp$performance$loss)
  expect_equal(imp$summary[type == "marginal", relative_to_improvement],
    imp$summary[type == "marginal", mean_importance] / imp$improvement$improvement)
})

test_that("batching leaves the permutation importance unchanged", {
  fits = rpart_fits()
  one = csdg_importance(fits, repetitions = 3L, batch_size = 1L, seed = 2L)
  twenty = csdg_importance(fits, repetitions = 3L, batch_size = 20L, seed = 2L)
  expect_identical(one$fold_pfi, twenty$fold_pfi)
})

test_that("grouped PFI is added without changing the marginal values", {
  fits = rpart_fits()
  plain = csdg_importance(fits, repetitions = 2L, seed = 2L)
  grouped = csdg_importance(fits, repetitions = 2L, seed = 2L, groups = list(size = c("cyl", "disp")))
  expect_identical(grouped$fold_pfi[type == "marginal"], plain$fold_pfi)
  expect_identical(unique(grouped$fold_pfi[type == "grouped", feature_group]), "size")
  expect_output(print(grouped), "grouped: size \\(cyl, disp\\)")
  expect_error(csdg_importance(fits, groups = list(cyl = c("cyl", "disp"))), "differ from predictor names")
  expect_error(csdg_importance(fits, groups = list(g = c("cyl", "nope"))), "Unknown grouped features")
})

test_that("strata are resolved from vectors, lists, and conditioning predictors", {
  set.seed(3)
  n = 60L
  dat = data.frame(item = sample(1:5, n, TRUE), score = stats::rnorm(n), flag = sample(c(TRUE, FALSE), n, TRUE))
  dat$y = dat$score + stats::rnorm(n)
  task = mlr3::as_task_regr(dat, target = "y")
  vec = rep(c("u", "v"), n / 2L)
  a = .csdg_resolve_conditioning(NULL, vec, task)
  expect_identical(names(a$strata), task$feature_names)
  expect_identical(a$strata$score, vec)
  b = .csdg_resolve_conditioning(NULL, list(score = vec), task)
  expect_identical(b$strata$score, vec)
  expect_identical(b$definition[["score"]], "the supplied strata")
  c_levels = .csdg_resolve_conditioning(list(score = "item"), NULL, task)
  expect_identical(c_levels$strata$score, as.character(task$data(cols = "item")$item))
  expect_identical(c_levels$definition[["score"]], "the levels of item")
  expect_identical(c_levels$conditioning$score, "item")
  c_deciles = .csdg_resolve_conditioning(list(item = "score"), NULL, task)
  expect_identical(data.table::uniqueN(c_deciles$strata$item), 10L)
  expect_identical(c_deciles$definition[["item"]], "deciles of score")
  crossed = .csdg_resolve_conditioning(list(score = c("item", "flag")), NULL, task)
  expect_identical(crossed$definition[["score"]], "the levels of item and the levels of flag")
  expect_error(.csdg_resolve_conditioning(NULL, list(nope = vec), task), "Unknown predictors")
  expect_error(.csdg_resolve_conditioning(list(score = "score"), NULL, task), "condition on itself")
  expect_error(.csdg_resolve_conditioning(NULL, list(score = "item"), task), "use `conditional`")
  expect_error(.csdg_resolve_conditioning(list(score = "item"), list(score = vec), task), "both")
})

test_that("a warning flags small strata in every form", {
  fits = rpart_fits()
  expect_warning(csdg_importance(fits, repetitions = 2L, seed = 2L, conditional = list(wt = "qsec")),
    "median stratum size")
  expect_warning(csdg_importance(fits, repetitions = 2L, seed = 2L, strata = list(wt = seq_len(32L))),
    "median stratum size in the held-out folds is 1")
  expect_no_warning(csdg_importance(fits, repetitions = 2L, seed = 2L, strata = list(wt = rep(1:2, 16L))))
})

test_that("conditional PFI uses distinct seeds per predictor, and extension equals a fresh call", {
  expect_false(.csdg_conditional_seed(2L, 1L) + 1L * 1e5 + 1000L + 2L ==
    .csdg_conditional_seed(2L, 2L) + 1L * 1e5 + 1000L + 1L)
  expect_identical(.csdg_conditional_seed(2L, 3L), 30000002L)
  fits = rpart_fits()
  strata = rep(1:2, 16L)
  base = csdg_importance(fits, repetitions = 3L, seed = 2L)
  extended = csdg_importance(base, strata = list(wt = strata, hp = strata), groups = list(size = c("cyl", "disp")))
  fresh = csdg_importance(fits, repetitions = 3L, seed = 2L, strata = list(wt = strata, hp = strata),
    groups = list(size = c("cyl", "disp")))
  expect_identical(extended$fold_pfi, fresh$fold_pfi)
  expect_false(identical(extended$conditional$tree$wt$raw$importance, extended$conditional$tree$hp$raw$importance))
  expect_error(csdg_importance(base, repetitions = 5L), "taken from it")
  expect_output(print(extended), "conditional: wt, within the supplied strata")
})

test_that("ties count against a predictor, intervals are available, and a missing baseline is an error", {
  fits = rpart_fits()
  imp = csdg_importance(fits, repetitions = 2L, seed = 2L)
  marginal = imp$fold_pfi[type == "marginal"]
  expected = marginal[, .(feature_group, expected = vapply(importance, function(v) sum(importance >= v),
    integer(1L))), by = iteration]
  merged = merge(marginal, expected, by = c("iteration", "feature_group"))
  expect_identical(merged$rank_in_fold, merged$expected)
  tab = as.data.table(imp, interval = TRUE)
  expect_true(all(c("lower", "upper") %in% names(tab)))
  expect_true(all(tab$lower <= tab$mean_importance & tab$mean_importance <= tab$upper))
  expect_identical(summary(imp)$lower, tab$lower)
  broken = fits
  broken$baseline = NULL
  expect_error(csdg_importance(broken, repetitions = 2L), "no model without predictors")
})

test_that("classification runs end to end with log loss", {
  testthat::skip_if_not_installed("rpart")
  fits = csdg_fit(make_task_iris_binary(), list(tree = mlr3::lrn("classif.rpart", predict_type = "prob")),
    folds = 3L, seed = 1L)
  imp = csdg_importance(fits, repetitions = 2L, seed = 2L)
  expect_identical(imp$loss, "logloss")
  claim = claim_top_k(imp, k = 1L)
  chk = suppressWarnings(csdg_check(claim, imp))
  expect_s3_class(chk, "CSDGCheck")
  expect_s3_class(csdg_assess(chk), "CSDGAssessment")
  expect_error(csdg_fit(make_task_iris_binary(), list(tree = mlr3::lrn("classif.rpart", predict_type = "prob")),
    measure = "regr.rmse"), "PFI is computed for the measures")
})

test_that("csdg_importance() validates its input", {
  fits = rpart_fits()
  expect_error(csdg_importance(fits, repetitions = 1L), "repetitions")
  expect_error(csdg_importance(list()), "CSDGFits")
  expect_error(csdg_importance(fits, conditional = list(nope = "cyl")), "Unknown predictors")
})
