test_that("claims derived from results record scope, selection, and exploratory origin", {
  imp = make_importance_fixture(labels = c(m1 = "XGBoost"))
  claim = claim_relies_mainly(imp, k = 2, factor = 2)
  expect_s3_class(claim, "CSDGDerivedClaim")
  expect_identical(claim$id, "relies_mainly_a_b_m1")
  expect_identical(claim$predictors, c("a", "b"))
  expect_identical(claim$selection, paste(
    "Formulated after inspecting the held-out PFI of XGBoost; a and b are its two leading predictors on average."
  ))
  expect_identical(claim$record$model_scope, "learner")
  expect_identical(claim$record$provenance$origin, "retrospective_exploratory")
  expect_identical(names(claim$scope), .csdg_scope_elements)
  expect_match(claim$scope$quantity, "training sets of 300 rows \\(3/4 of the sample\\)")
  expect_identical(claim$scope$model, "XGBoost.")
  expect_identical(claim_relies_mainly(imp, learners = "XGBoost")$learner, "m1")
  expect_identical(claim_relies_mainly(imp, learners = "xgboost")$learner, "m1")
  expect_identical(claim_order(imp, "a", "b", note = "from the plot")$note, "from the plot")
  expect_output(print(claim), "Origin: exploratory")
})

test_that("claim constructors validate their input and warn about unusual selections", {
  imp = make_importance_fixture()
  expect_warning(claim_relies_mainly(imp, predictors = c("a", "c")), "not the two largest")
  expect_identical(suppressWarnings(claim_relies_mainly(imp, predictors = c("a", "c")))$k, 2L)
  expect_warning(claim_order(imp, "b", "a"), "does not have the larger average PFI")
  ordered = claim_order(imp, c("b", "a"))
  expect_identical(c(ordered$first, ordered$second), c("a", "b"))
  expect_match(ordered$selection, "ordered by these averages")
  expect_error(claim_relies_mainly(imp, k = 4), "k")
  expect_error(claim_relies_mainly(imp, factor = 0.5), "factor")
  expect_error(claim_relies_mainly(imp, predictors = c("a", "z")), "Unknown predictors")
  expect_error(claim_order(imp, "a", "a"), "different")
  expect_error(claim_top_k(imp, learners = "m9"), "Unknown learner")
  expect_error(claim_top_k(imp, "m1", 3), "name the learners with `learners = \"m1\"`")
  expect_error(claim_top_k(imp, k = 2, learners = 3), "numbers belong to `k`")
  expect_error(claim_relies_mainly(imp, labels = c(z = "zz")), "Unknown predictors")
  expect_error(claim_direction(imp, "a"), "numeric predictor")
})

test_that("the ALE change of a linear model is its slope times the percentile distance", {
  testthat::skip_if_not_installed("mlr3learners")
  requireNamespace("mlr3learners", quietly = TRUE)
  set.seed(5)
  n = 400L
  dat = data.frame(x = stats::rnorm(n), w = stats::rnorm(n))
  dat$y = 2 * dat$x + stats::rnorm(n)
  task = mlr3::as_task_regr(dat, target = "y", id = "slope")
  fits = csdg_fit(task, list(lm = mlr3::lrn("regr.lm")), folds = 4L, seed = 1L)
  claim = claim_direction(fits, "x")
  expect_identical(claim$direction, "rise")
  expect_match(claim$selection, "computed by claim_direction\\(\\); the average change between the 10th and 90th percentiles of x is [0-9.]+ \\(positive\\)")
  eff = csdg_effect(fits, "x")
  expect_equal(eff$changes$change, claim$effect$change, tolerance = 1e-12)
  from_effect = claim_direction(eff, "x")
  expect_match(from_effect$selection, "after inspecting the ALE of linear regression \\(csdg_effect\\(\\)\\)")
  expect_output(print(eff), "folds with a positive change")
  expect_s3_class(plot(eff), "ggplot")
  expect_identical(nrow(as.data.table(eff, level = "change")), 4L)
  two = csdg_effect(fits, c("x", "w"))
  expect_identical(nrow(claim_direction(two, "x")$effect), 4L)
  expect_equal(claim_direction(two, "x")$effect$change, claim$effect$change, tolerance = 1e-12)
  chk_eff = csdg_check(from_effect, eff, judgments = g0b_judged())
  expect_identical(csdg_assess(chk_eff)$assessment, "met")
  expect_identical(csdg_check(from_effect, fits, judgments = g0b_judged())$table$status, chk_eff$table$status)
  expected = vapply(seq_len(4L), function(k) {
    model = fits$resamples$lm$models[[k]]
    x_test = dat$x[fits$resamples$lm$test_sets[[k]]]
    stats::coef(model$model)[["x"]] * diff(stats::quantile(x_test, c(0.1, 0.9), names = FALSE))
  }, numeric(1L))
  expect_equal(claim$effect$change, expected, tolerance = 1e-10)
  expect_identical(claim$statement, "The predictions of linear regression rise with x.")
  imp = csdg_importance(fits, repetitions = 2L, seed = 2L)
  chk = csdg_check(claim, imp, judgments = g0b_judged())
  expect_equal(chk$criteria$min_change, 0.1 * stats::sd(dat$y))
  expect_identical(csdg_assess(chk)$assessment, "met")
  expect_identical(chk$explanation$method_ids, "ale")
  expect_pass_through(chk)
  expect_error(claim_direction(fits, "x", probs = c(0.9, 0.1)), "probs")
})

test_that("each fold of a rise needs the change and no larger decrease between adjacent grid points", {
  testthat::skip_if_not_installed("mlr3learners")
  requireNamespace("mlr3learners", quietly = TRUE)
  set.seed(5)
  n = 400L
  dat = data.frame(x = stats::rnorm(n), w = stats::rnorm(n))
  dat$y = 2 * dat$x + stats::rnorm(n)
  fits = csdg_fit(mlr3::as_task_regr(dat, target = "y", id = "slope"), list(lm = mlr3::lrn("regr.lm")), folds = 4L,
    seed = 1L)
  claim = claim_direction(fits, "x")
  expect_true(claim$monotone)
  expect_equal(claim$effect$max_decrease, rep(0, 4L))
  expect_match(claim$scope$quantity, "and its largest decrease between adjacent grid points in this range")
  claim$effect = data.table::copy(claim$effect)[, max_decrease := c(0, 0.5, 0.3, 0)]
  chk = csdg_check(claim, fits, min_change = 0.3, stability = 0.75, judgments = g0b_judged())
  folds = chk$details$stability$lm$folds
  expect_identical(folds$change_holds, rep(TRUE, 4L))
  expect_identical(folds$shape_holds, c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(folds$class, c("holds", "fails", "holds", "holds"))
  expect_identical(chk$table[check == "stability", status], "supported")
  expect_match(chk$table[check == "stability", observation], paste("in 3 of 4 folds; a decrease exceeds 0.3 in 1",
    "fold; the largest decrease between adjacent grid points is 0.500 \\(fold 2\\)"))
  expect_match(.csdg_criteria_text(chk$criteria, claim, 4L), "minimum change 0.3, with no decrease beyond it")
  strict = csdg_check(claim, fits, min_change = 0.3, judgments = g0b_judged())
  expect_identical(strict$table[check == "stability", status], "contradicted")
  res = csdg_assess(strict)
  expect_identical(res$assessment, "not_met")
  expect_match(res$hints, "'higher at the 90th than at the 10th percentile' \\(claim_direction\\(monotone = FALSE")
  expect_s3_class(plot(strict), "ggplot")
  fall = suppressWarnings(claim_direction(fits, "x", direction = "fall"))
  expect_equal(.csdg_direction_reversals(fall, "lm"), fall$effect$max_increase)
  old = claim
  old$effect = claim$effect[, !c("max_decrease", "max_increase")]
  old$monotone = NULL
  chk_old = csdg_check(old, fits, judgments = g0b_judged())
  expect_equal(chk_old$claim$effect$max_decrease, rep(0, 4L))
  expect_identical(chk_old$table$observation,
    csdg_check(claim_direction(fits, "x"), fits, judgments = g0b_judged())$table$observation)
})

test_that("a rise is not met when the ALE dips between adjacent grid points, but higher at the 90th percentile is", {
  testthat::skip_if_not_installed("rpart")
  set.seed(6)
  n = 600L
  dat = data.frame(x = stats::runif(n, -2, 2), w = stats::rnorm(n))
  dat$y = dat$x + 1.5 * sin(4 * dat$x) + stats::rnorm(n, sd = 0.2)
  task = mlr3::as_task_regr(dat, target = "y", id = "wave")
  fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart", cp = 0.001)), folds = 5L, seed = 1L)
  eff = csdg_effect(fits, "x")
  rise = claim_direction(eff, "x")
  expect_identical(rise$statement, "The predictions of decision tree rise with x.")
  chk = csdg_check(rise, eff, judgments = g0b_judged())
  minimum = chk$criteria$min_change
  expect_true(all(eff$changes$change >= minimum))
  expect_true(all(eff$changes$max_decrease > minimum))
  expect_identical(chk$table[check == "content", status], "supported")
  expect_identical(chk$table[check == "stability", status], "contradicted")
  expect_match(chk$table[check == "stability", observation], sprintf(
    "in 0 of 5 folds; a decrease exceeds %s in 5 folds; the largest decrease between adjacent grid points is %s",
    .csdg_crit(minimum), .csdg_num(max(eff$changes$max_decrease))))
  expect_identical(csdg_assess(chk)$assessment, "not_met")
  higher = claim_direction(eff, "x", monotone = FALSE)
  expect_identical(higher$statement,
    "The predictions of decision tree are higher at the 90th than at the 10th percentile of x.")
  expect_identical(higher$id, "direction_x_higher_tree")
  expect_false(grepl("adjacent", higher$scope$quantity))
  chk_higher = csdg_check(higher, eff, judgments = g0b_judged())
  expect_identical(chk_higher$table[check == "stability", observation],
    sprintf("The change in the claimed direction is at least %s in 5 of 5 folds.", .csdg_crit(minimum)))
  expect_match(chk_higher$table[check == "content", observation], "Change in the claimed direction \\(higher\\)")
  expect_identical(csdg_assess(chk_higher)$assessment, "met")
  expect_error(claim_direction(eff, "x", monotone = NA), "monotone")
})
