# FILE: tests/testthat/test-iml-parity.R

# These tests are optional and only run if the 'iml' package is installed.
# They provide a lightweight parity check against iml::FeatureEffect.

test_that("Gate 2 ALE/PDP agree (approximately) with iml::FeatureEffect for regression", {
  skip_on_cran()
  skip_if_not_installed("iml")

  auto = get_auto_mtcars_regr(quick_start = FALSE)
  task = auto$task
  model = auto$ctx$final_model

  # Gate 2 artifacts
  g2 = auto$result$gate_results[["G2"]]
  a = g2$artifacts

  feat = "wt"

  dt_ale = a$ale_curves
  dt_pd = a$pd_curves
  expect_true(data.table::is.data.table(dt_ale))
  expect_true(data.table::is.data.table(dt_pd))

  dt_ale = dt_ale[feature == feat & class_label == "response"]
  dt_pd = dt_pd[feature == feat & class_label == "response"]
  expect_gt(nrow(dt_ale), 5)
  expect_gt(nrow(dt_pd), 5)

  # iml predictor using mlr3 learner via predict.function
  X_train = task$data(cols = task$feature_names)
  y_train = task$truth()

  pred_fun = function(model, newdata) {
    model$predict_newdata(newdata)$response
  }

  predictor = iml::Predictor$new(
    model = model,
    data = X_train,
    y = y_train,
    predict.function = pred_fun
  )

  # ALE parity
  eff_ale = iml::FeatureEffect$new(
    predictor = predictor,
    feature = feat,
    method = "ale",
    grid.size = max(2L, max(dt_ale$n_bin))
  )

  iml_ale = eff_ale$results
  expect_true(is.data.frame(iml_ale))
  expect_true(all(c(feat, ".value") %in% names(iml_ale)))

  m_ale = merge(
    data.table::data.table(x = round(dt_ale$x_mid, 6), ours = dt_ale$ale),
    data.table::data.table(x = round(iml_ale[[feat]], 6), iml = iml_ale$.value),
    by = "x"
  )

  # If grids don't align perfectly, fall back to interpolation (still informative).
  if (nrow(m_ale) >= 5) {
    expect_lt(max(abs(m_ale$ours - m_ale$iml)), 0.10)
  } else {
    approx_iml = stats::approx(x = iml_ale[[feat]], y = iml_ale$.value, xout = dt_ale$x_mid, rule = 2)$y
    expect_lt(max(abs(dt_ale$ale - approx_iml)), 0.15)
  }

  # PDP parity
  grid_pts = sort(unique(dt_pd$x))
  eff_pd = iml::FeatureEffect$new(
    predictor = predictor,
    feature = feat,
    method = "pdp",
    grid.points = grid_pts
  )
  iml_pd = eff_pd$results

  m_pd = merge(
    data.table::data.table(x = round(dt_pd$x, 6), ours = dt_pd$pd),
    data.table::data.table(x = round(iml_pd[[feat]], 6), iml = iml_pd$.value),
    by = "x"
  )
  if (nrow(m_pd) >= 5) {
    expect_lt(max(abs(m_pd$ours - m_pd$iml)), 0.10)
  } else {
    approx_iml = stats::approx(x = iml_pd[[feat]], y = iml_pd$.value, xout = dt_pd$x, rule = 2)$y
    expect_lt(max(abs(dt_pd$pd - approx_iml)), 0.15)
  }
})
