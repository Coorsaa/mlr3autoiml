# FILE: tests/testthat/test-plots.R

test_that("AutoIML$plot produces ggplot/patchwork objects and does not accept non-plot types", {
  skip_if_not_installed("ggplot2")

  auto = get_auto_iris(quick_start = FALSE)

  # Non-plot types should not be routed through $plot
  expect_error(auto$plot("report_card"), regexp = "Unknown plot type")

  # Gate 1 plot
  p1 = auto$plot("g1_scores", measure = "classif.logloss")
  expect_s3_class(p1, "ggplot")

  # Gate 2 plots
  p2 = auto$plot("g2_ice_spread", top_n = 6)
  expect_s3_class(p2, "ggplot")

  p3 = auto$plot("g2_hstats", top_n = 6)
  expect_s3_class(p3, "ggplot")

  p3b = auto$plot("g2_ale_2d")
  expect_s3_class(p3b, "ggplot")
  expect_true(inherits(p3b$layers[[1L]]$geom, "GeomRect"))
  expect_identical(mlr3autoiml:::.autoiml_plot_palette()$gradient[["mid"]], "#E6DED3")
  expect_identical(unname(p3b$scales$get_scales("fill")$palette(c(0, 0.5, 1))[2]), "#E6DED3")

  # Effect plot; if patchwork is available, show_ice produces a patchwork; otherwise ggplot.
  p4 = auto$plot("g2_effect", feature = "Petal.Length", method = "auto", show_ice = TRUE)
  expect_true(inherits(p4, "ggplot") || inherits(p4, "patchwork"))

  # SHAP plot (if implemented)
  p5 = auto$plot("shap_local", row_id = 1L, sample_size = 20L, background_n = 30L)
  expect_s3_class(p5, "ggplot")

  p6 = auto$plot("overview")
  expect_true(inherits(p6, "ggplot") || inherits(p6, "patchwork") || is.list(p6))
})

test_that("cached global SHAP accepts AutoIMLResult", {
  auto = get_auto_iris(quick_start = FALSE)

  dt = mlr3autoiml:::.autoiml_shap_global_cached(
    result = auto$result,
    class_label = auto$task$class_names[[1L]],
    sample_rows = 5L,
    sample_size = 5L,
    background_n = 10L,
    seed = 1L
  )

  expect_true(data.table::is.data.table(dt))
  expect_gt(nrow(dt), 0L)
})

test_that("regression g2_effect does not facet missing class labels and pads ALE", {
  skip_if_not_installed("ggplot2")

  auto = get_auto_mtcars(quick_start = FALSE)
  top_feature = auto$result$gate_results$G2$artifacts$recommendation$top_features[[1L]]
  ale_dt = data.table::as.data.table(auto$result$gate_results$G2$artifacts$ale_curves)[feature == top_feature]

  p = auto$plot("g2_effect", feature = top_feature, method = "ale")
  expect_s3_class(p, "ggplot")
  expect_identical(class(p$facet)[1L], "FacetNull")

  built = ggplot2::ggplot_build(p)
  panel = built$layout$panel_params[[1L]]
  y_range = if (!is.null(panel$y.range)) panel$y.range else panel$y$continuous_range

  expect_lt(y_range[1L], min(ale_dt$ale))
  expect_gt(y_range[2L], max(ale_dt$ale))
})

test_that("ALE keeps full support for low-cardinality numeric features", {
  task = make_task_mtcars_regr()
  learner = make_learner_regr()
  learner$train(task)

  ale_dt = mlr3autoiml:::.autoiml_ale_1d_iml(
    task = task,
    model = learner,
    X = task$data(cols = task$feature_names),
    feature = "cyl",
    bins = 10L
  )

  raw_x = sort(unique(task$data(cols = "cyl")[["cyl"]]))

  expect_true(data.table::is.data.table(ale_dt))
  expect_equal(min(ale_dt$x_left), min(raw_x))
  expect_equal(max(ale_dt$x_right), max(raw_x))
  expect_equal(nrow(ale_dt), length(raw_x) - 1L)
})
