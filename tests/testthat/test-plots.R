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
