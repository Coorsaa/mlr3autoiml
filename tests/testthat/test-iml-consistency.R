# FILE: tests/testthat/test-iml-consistency.R

# These tests are optional and only run if the 'iml' package is installed.
# They provide a lightweight consistency check against iml::FeatureEffect.

test_that("Gate 2 ALE/PDP agree (approximately) with iml::FeatureEffect for regression", {
  skip_on_cran()
  skip_if_not_installed("iml")

  auto = get_auto_mtcars_regr(quick_start = FALSE)
  task = auto$task
  model = auto$ctx$final_model

  # Gate 2 artifacts
  g2 = auto$result$gate_results[["G2"]]
  a = g2$artifacts

  # Pick a feature that was actually computed (depends on max_features config)
  dt_ale_all = a$ale_curves
  dt_pd_all = a$pd_curves
  expect_true(data.table::is.data.table(dt_ale_all))
  expect_true(data.table::is.data.table(dt_pd_all))

  # Get the first available feature
  feat = unique(dt_ale_all$feature)[1L]
  if (is.na(feat) || length(feat) == 0L) {
    skip("No ALE features computed")
  }

  # For regression tasks, class_label is NA
  dt_ale = dt_ale_all[feature == feat & is.na(class_label)]
  dt_pd = dt_pd_all[feature == feat & is.na(class_label)]
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

  # ALE consistency
  # Note: our ALE uses 'x' column for midpoints, iml uses 'n_interval' for bin counts
  n_grid = if ("n_interval" %in% names(dt_ale)) max(dt_ale$n_interval) else nrow(dt_ale)
  eff_ale = iml::FeatureEffect$new(
    predictor = predictor,
    feature = feat,
    method = "ale",
    grid.size = max(2L, n_grid)
  )

  iml_ale = eff_ale$results
  expect_true(is.data.frame(iml_ale))
  expect_true(all(c(feat, ".value") %in% names(iml_ale)))

  m_ale = merge(
    data.table::data.table(x = round(dt_ale$x, 6), ours = dt_ale$ale),
    data.table::data.table(x = round(iml_ale[[feat]], 6), iml = iml_ale$.value),
    by = "x"
  )

  # If grids don't align perfectly, fall back to interpolation (still informative).
  if (nrow(m_ale) >= 5) {
    expect_lt(max(abs(m_ale$ours - m_ale$iml)), 0.10)
  } else {
    approx_iml = stats::approx(x = iml_ale[[feat]], y = iml_ale$.value, xout = dt_ale$x, rule = 2)$y
    expect_lt(max(abs(dt_ale$ale - approx_iml)), 0.15)
  }

  # PDP consistency
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
