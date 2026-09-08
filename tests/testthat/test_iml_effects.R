.formula_prediction_model = function(predict_fun) {
  list(
    predict_newdata = function(newdata) {
      response = as.numeric(predict_fun(newdata))
      mlr3::PredictionRegr$new(
        row_ids = seq_along(response),
        truth = numeric(length(response)),
        response = response
      )
    }
  )
}

.ale_2d_test_design = function() {
  design = data.table::CJ(
    x1 = seq(-2, 2, length.out = 31L),
    x2 = seq(-1.5, 1.5, length.out = 31L)
  )
  design[, target := 0]
  design
}

test_that("one-dimensional ALE aligns midpoint effects on uneven intervals", {
  design = data.table::data.table(
    x1 = seq(0, 1, length.out = 401L)^3,
    x2 = seq(-2, 2, length.out = 401L),
    target = 0
  )
  task = mlr3::TaskRegr$new("ale-one-dimensional-linear", backend = design, target = "target")
  model = .formula_prediction_model(function(newdata) 2 * newdata$x1 - 3 * newdata$x2)

  result = .autoiml_ale_1d_iml(
    task = task,
    model = model,
    X = task$data(cols = task$feature_names),
    feature = "x1",
    bins = 8L
  )

  expect_equal(diff(result$ale) / diff(result$x), rep(2, nrow(result) - 1L), tolerance = 1e-10)
  expect_equal(stats::weighted.mean(result$ale, result$n_interval), 0, tolerance = 1e-12)
})

test_that("second-order ALE removes additive main effects", {
  design = .ale_2d_test_design()
  task = mlr3::TaskRegr$new("ale-additive", backend = design, target = "target")
  model = .formula_prediction_model(function(newdata) 3 * newdata$x1 - 2 * newdata$x2)

  result = .autoiml_ale_2d(
    task = task,
    model = model,
    X = task$data(cols = task$feature_names),
    feature1 = "x1",
    feature2 = "x2",
    bins = 6L
  )

  expect_s3_class(result, "data.table")
  expect_lt(max(abs(result$ale2d)), 1e-10)
})

test_that("second-order ALE isolates and centers a known interaction", {
  design = .ale_2d_test_design()
  task = mlr3::TaskRegr$new("ale-interaction", backend = design, target = "target")
  model = .formula_prediction_model(function(newdata) {
    3 * newdata$x1 - 2 * newdata$x2 + 1.5 * newdata$x1 * newdata$x2
  })

  result = .autoiml_ale_2d(
    task = task,
    model = model,
    X = task$data(cols = task$feature_names),
    feature1 = "x1",
    feature2 = "x2",
    bins = 6L
  )

  expect_gt(diff(range(result$ale2d)), 1)
  expect_equal(stats::weighted.mean(result$ale2d, result$n_cell), 0, tolerance = 1e-10)

  row_margins = result[, .(margin = stats::weighted.mean(ale2d, n_cell)), by = x1]
  column_margins = result[, .(margin = stats::weighted.mean(ale2d, n_cell)), by = x2]
  expect_lt(max(abs(row_margins$margin)), 1e-10)
  expect_lt(max(abs(column_margins$margin)), 1e-10)
})

test_that("second-order ALE preserves rectangular output geometry", {
  design = .ale_2d_test_design()
  task = mlr3::TaskRegr$new("ale-geometry", backend = design, target = "target")
  model = .formula_prediction_model(function(newdata) newdata$x1 * newdata$x2)

  result = .autoiml_ale_2d(
    task = task,
    model = model,
    X = task$data(cols = task$feature_names),
    feature1 = "x1",
    feature2 = "x2",
    bins = 6L
  )

  expect_named(result, c(
    "feature1", "feature2", "x1_left", "x1_right", "x2_bottom", "x2_top",
    "x1", "x2", "ale2d", "n_cell", "class_label"
  ))
  expect_equal(nrow(result), 36L)
  expect_true(all(result$x1_right > result$x1_left))
  expect_true(all(result$x2_top > result$x2_bottom))
  expect_equal(result$x1, (result$x1_left + result$x1_right) / 2)
  expect_equal(result$x2, (result$x2_bottom + result$x2_top) / 2)
  expect_equal(sum(result$n_cell), nrow(design))
})
