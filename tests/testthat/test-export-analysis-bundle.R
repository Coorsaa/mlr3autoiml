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

test_that("save_analysis_plot writes an opaque white PNG background", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("png")

  stem = tempfile("opaque-plot-")
  plot = ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = NA, color = NA))

  expect_true(save_analysis_plot(plot, stem, width = 2, height = 2))
  image = png::readPNG(paste0(stem, ".png"), native = FALSE)
  expect_true(dim(image)[[3L]] == 3L || all(image[, , 4L] == 1))
})
