test_that("export_analysis_bundle can skip standard plot types", {
  skip_if_not_installed("ggplot2")

  auto = get_auto_iris(quick_start = FALSE)
  dir = tempfile("bundle-")

  paths = export_analysis_bundle(
    auto,
    dir = dir,
    prefix = "iris",
    exclude_plot_types = c("g2_effect", "g2_hstats")
  )

  expect_true(is.list(paths))
  expect_false("fig_g2_effect" %in% names(paths))
  expect_false("fig_g2_hstats" %in% names(paths))
  expect_true("fig_g1_scores" %in% names(paths))
})