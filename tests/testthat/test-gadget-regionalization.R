# FILE: tests/testthat/test-gadget-regionalization.R

make_gadget_toy = function(n = 80L) {
  set.seed(2026)
  z = seq(-1, 1, length.out = n)
  x = data.frame(
    z = z,
    a = stats::rnorm(n),
    b = stats::rnorm(n)
  )
  grid = seq(-1, 1, length.out = 7L)
  side = ifelse(z <= 0, -1, 1)
  list(
    X = x,
    grids = list(a = grid, b = grid),
    pred_mats = list(
      a = outer(side, grid * 2.0),
      b = outer(-side, grid * 1.5)
    )
  )
}

test_that("joint GADGET regionalization returns multi-feature regional artifacts", {
  toy = make_gadget_toy()

  out = mlr3autoiml:::.autoiml_gadget_regionalize_multi(
    X = toy$X,
    row_ids = seq_len(nrow(toy$X)),
    features = c("a", "b"),
    grids = toy$grids,
    pred_mats = toy$pred_mats,
    split_candidates = c("z", "a", "b"),
    max_depth = 2L,
    min_bucket = 10L,
    gamma = 0,
    n_thresholds = 7L,
    local_keep_n = 12L,
    class_label = NULL,
    method = "pdp_cice",
    seed = 2026L
  )

  expect_true(is.list(out))
  expect_identical(out$features, c("a", "b"))
  expect_true(data.table::is.data.table(out$regions))
  expect_true(data.table::is.data.table(out$curves))
  expect_true(data.table::is.data.table(out$splits))
  expect_true(data.table::is.data.table(out$feature_metrics))
  expect_true(data.table::is.data.table(out$total_metrics))
  expect_true(all(c("path", "n", "total_loss", "bounds") %in% names(out$regions)))
  expect_true(all(c("feature", "path", "x", "y_region", "y_global") %in% names(out$curves)))
  expect_true(all(c("depth", "split_feature", "threshold", "gain") %in% names(out$splits)))
  expect_true(out$total_metrics$heterogeneity_reduction[1L] > 0.8)
})

test_that("GADGET feature view preserves legacy single-feature shape", {
  toy = make_gadget_toy()
  out = mlr3autoiml:::.autoiml_gadget_regionalize_multi(
    X = toy$X,
    row_ids = seq_len(nrow(toy$X)),
    features = c("a", "b"),
    grids = toy$grids,
    pred_mats = toy$pred_mats,
    split_candidates = c("z", "a", "b"),
    max_depth = 1L,
    min_bucket = 10L,
    gamma = 0,
    n_thresholds = 5L,
    local_keep_n = 8L,
    class_label = NULL,
    method = "pdp_cice",
    seed = 2027L
  )

  view = mlr3autoiml:::.autoiml_gadget_feature_view(out, "a")
  expect_true(is.list(view))
  expect_true(data.table::is.data.table(view$regions))
  expect_true(data.table::is.data.table(view$curves))
  expect_true(data.table::is.data.table(view$local_curves))
  expect_true(all(view$curves$feature == "a"))
  expect_true(all(c("x", "y_region", "y_global", "path") %in% names(view$curves)))
})
