numerical_example = function(n = 2000L, folds = 5L, strata = FALSE) {
  testthat::skip_if_not_installed("mlr3learners")
  testthat::skip_if_not_installed("mlr3pipelines")
  requireNamespace("mlr3learners", quietly = TRUE)
  set.seed(1)
  z = stats::rnorm(n)
  task = mlr3::as_task_regr(data.frame(item_1 = z, item_2 = z, y = z), target = "y", id = "numerical_example")
  model_using = function(item) {
    mlr3::as_learner(mlr3pipelines::`%>>%`(
      mlr3pipelines::po("select", selector = mlr3pipelines::selector_name(item)), mlr3::lrn("regr.lm")))
  }
  fits = csdg_fit(task, list(A = model_using("item_1"), B = model_using("item_2")), folds = folds,
    measure = "regr.mse", labels = c(A = "model A", B = "model B"), seed = 1L)
  list(z = z, task = task, fits = fits)
}

test_that("csdg_fit() fits all learners and the baseline on the same splits", {
  ex = numerical_example()
  fits = ex$fits
  expect_s3_class(fits, "CSDGFits")
  expect_identical(fits$resamples$A$test_sets, fits$resamples$B$test_sets)
  expect_identical(fits$resamples$A$test_sets, fits$baseline$test_sets)
  expect_identical(fits$K, 5L)
  expect_identical(fits$n_train + fits$n_test, rep(2000L, 5L))
  expect_output(print(fits), "5-fold cross-validation; the same splits")
  expect_setequal(unique(as.data.table(fits)$learner), c("A", "B", "baseline"))
})

test_that("numerical example: exact PFI, improvement, and the three assessments", {
  ex = numerical_example()
  fits = ex$fits
  imp = csdg_importance(fits, repetitions = 5L, seed = 2L)
  expect_identical(imp$loss, "mse")
  expect_s3_class(fits$bmr, "BenchmarkResult")
  raw_a = imp$pfi$A$raw
  expect_true(all(raw_a[feature_group == "item_2", importance] == 0))
  expect_true(all(imp$pfi$B$raw[feature_group == "item_1", importance] == 0))
  expect_true(all(imp$performance$loss < 1e-20))
  y = ex$z
  manual = mean(vapply(seq_len(fits$K), function(k) {
    train = fits$resamples$A$train_sets[[k]]
    test = fits$resamples$A$test_sets[[k]]
    mean((y[test] - mean(y[train]))^2)
  }, numeric(1L)))
  expect_equal(imp$improvement[learner == "A", improvement], manual, tolerance = 1e-12)
  direct = csdg_fold_pfi(fits$resamples$A, loss = "mse", repetitions = 5L, seed = 2L)
  expect_identical(imp$pfi$A$raw[feature_group == "item_1", importance],
    direct$raw[feature_group == "item_1", importance])

  both = claim_order(imp, "item_1", "item_2", learners = c("A", "B"), marginal_only = TRUE)
  res = csdg_assess(csdg_check(both, imp))
  expect_identical(res$assessment, "not_met")
  expect_identical(unique(res$decisive$gate), "G6a")
  model_a = claim_order(imp, "item_1", "item_2", learners = "A", marginal_only = TRUE)
  expect_identical(csdg_assess(csdg_check(model_a, imp))$assessment, "met")

  expect_warning(strata <- csdg_importance(imp, strata = list(item_1 = ex$z, item_2 = ex$z)),
    "median stratum size in the held-out folds is 1")
  fresh = suppressWarnings(csdg_importance(fits, repetitions = 5L, seed = 2L,
    strata = list(item_1 = ex$z, item_2 = ex$z)))
  expect_identical(strata$fold_pfi, fresh$fold_pfi)
  expect_true(all(strata$conditional$A$item_1$raw$importance == 0))
  expect_identical(strata$pfi$A$raw, imp$pfi$A$raw)
  unqualified = claim_order(strata, "item_1", "item_2", learners = "A")
  chk = csdg_check(unqualified, strata)
  expect_identical(chk$table[check == "procedure", status], "contradicted")
  expect_identical(csdg_assess(chk)$assessment, "not_met")
  expect_pass_through(chk)
})

test_that("csdg_fit() and csdg_importance() leave the caller's random number state unchanged", {
  testthat::skip_if_not_installed("rpart")
  task = make_task_mtcars_regr()
  set.seed(42)
  before = .Random.seed
  fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3L, seed = 1L)
  imp = csdg_importance(fits, repetitions = 2L, seed = 2L)
  expect_identical(.Random.seed, before)
  drawn = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3L)
  expect_true(is.integer(drawn$seed) && !is.na(drawn$seed))
  again = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3L, seed = drawn$seed)
  expect_identical(again$resamples$tree$test_sets, drawn$resamples$tree$test_sets)
})

test_that("csdg_fit() validates its input", {
  testthat::skip_if_not_installed("rpart")
  task = make_task_mtcars_regr()
  tree = mlr3::lrn("regr.rpart")
  expect_identical(names(csdg_fit(task, list(tree), folds = 3L, seed = 1L)$resamples), "rpart")
  expect_error(csdg_fit(task, list(a = tree, a = tree)), "unique")
  expect_error(csdg_fit(task, list(tree = tree), resampling = mlr3::rsmp("holdout")), "at least two iterations")
  expect_error(csdg_fit(task, list(tree = tree), measure = "regr.rsq"), "loss")
  expect_error(csdg_fit(task, list(tree = tree), measure = "regr.medae"), "PFI is computed for the measures")
  expect_error(csdg_fit(task, list(baseline = tree)), "reserved")
  expect_error(csdg_fit(task, list(`1x` = tree)), "start with a letter")
  expect_error(csdg_fit(make_task_iris_binary(), list(tree = mlr3::lrn("classif.rpart"))), "predict probabilities")
  expect_error(csdg_fit(make_task_iris(), list(tree = mlr3::lrn("classif.rpart", predict_type = "prob"))),
    "binary classification")
  expect_error(csdg_fit(task, list(tree = tree), labels = c(other = "x")), "Unknown names")
})

test_that("csdg_fit() accepts a benchmark result and labels common learners", {
  testthat::skip_if_not_installed("rpart")
  task = make_task_mtcars_regr()
  fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3L, seed = 1L)
  expect_identical(unname(fits$labels), "decision tree")
  expect_output(print(fits), "Loss for PFI and improvement: RMSE \\(regr.rmse\\)")
  from_bmr = csdg_fit(fits$bmr)
  expect_identical(from_bmr$resamples$tree$test_sets, fits$resamples$tree$test_sets)
  expect_equal(from_bmr$baseline$predictions$response, fits$baseline$predictions$response)
  expect_identical(length(fits$row_hashes), task$nrow)
  expect_false(anyDuplicated(fits$row_hashes) > 0L)
})
