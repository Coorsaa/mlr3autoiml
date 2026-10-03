test_that("text helpers format lists, sentences, and numbers", {
  expect_identical(.csdg_and(c("a", "b", "c")), "a, b, and c")
  expect_identical(.csdg_and(c("a", "b")), "a and b")
  expect_identical(.csdg_and("a"), "a")
  expect_identical(substr(.csdg_sentence("ridge logistic regression relies more on a"), 1L, 5L), "Ridge")
  expect_identical(.csdg_sentence("XGBoost relies"), "XGBoost relies")
  expect_identical(.csdg_number_word(3L), "three")
  expect_identical(.csdg_number_word(12L), "12")
  expect_identical(.csdg_num(c(0.15, 2, 0.0233, -0.0004, 0, 123.4)), c("0.150", "2.00", "0.0233", "-0.0004", "0",
    "123"))
  expect_identical(.csdg_pct(c(0.34, 0.012, 1.17)), c("34%", "1.2%", "117%"))
  expect_identical(.csdg_ci(0.15, 0.121, 0.179), "0.150 [0.121, 0.179]")
  expect_identical(.csdg_folds(9L, 10L), "9 of 10 folds")
  expect_identical(.csdg_r(0.97), ".97")
  expect_identical(.csdg_r(-0.712), "-.71")
  expect_identical(.csdg_loss_label("logloss"), "log loss")
  expect_identical(.csdg_ordinal(c(0.1)), "10th")
  expect_identical(.csdg_ordinal(0.91), "91st")
  expect_identical(.csdg_ordinal(c(0.025, 0.02, 0.12)), c("2.5th", "2nd", "12th"))
  expect_identical(.csdg_ratio(c(1.96, 6)), c("1.96", "6.00"))
  expect_identical(.csdg_loss_label("mae"), "absolute error")
})

test_that("claim statements follow the templates", {
  imp = make_importance_fixture(
    centers = list(m1 = fixture_centers(), m2 = fixture_centers()),
    labels = c(m1 = "XGBoost", m2 = "ridge logistic regression")
  )
  expect_identical(claim_relies_mainly(imp, k = 2)$statement, "XGBoost relies mainly on a and b.")
  expect_identical(claim_relies_mainly(imp, k = 2, learners = c("m1", "m2"))$statement,
    "XGBoost and ridge logistic regression both rely mainly on a and b.")
  expect_identical(claim_top_k(imp, k = 1)$statement, "XGBoost relies most on a.")
  expect_identical(claim_top_k(imp, k = 3)$statement,
    "The three predictors on which XGBoost relies most are a, b, and c.")
  expect_identical(claim_top_k(imp, k = 2, learners = c("m1", "m2"))$statement,
    "The two predictors on which XGBoost and ridge logistic regression each rely most are a and b.")
  expect_identical(claim_order(imp, "a", "b", learners = "m2")$statement,
    "Ridge logistic regression relies more on a than on b.")
  expect_identical(
    claim_order(imp, "a", "b", learners = c("m1", "m2"), marginal_only = TRUE,
      labels = c(a = "item 1", b = "item 2"))$statement,
    "Under marginal permutation, XGBoost and ridge logistic regression both rely more on item 1 than on item 2."
  )
})

test_that("identifiers are never capitalized as learner labels", {
  imp = make_importance_fixture()
  expect_identical(claim_relies_mainly(imp, k = 2)$statement, "Learner m1 relies mainly on a and b.")
  testthat::skip_if_not_installed("mlr3learners")
  requireNamespace("mlr3learners", quietly = TRUE)
  learners = list(xgb = mlr3::lrn("regr.xgboost"), ridge = mlr3::lrn("regr.cv_glmnet", alpha = 0),
    other = mlr3::lrn("regr.featureless"))
  expect_identical(unname(.csdg_default_labels(learners)), c("XGBoost", "ridge regression", "learner other"))
})
