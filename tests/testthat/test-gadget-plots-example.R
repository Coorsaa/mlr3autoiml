make_task_gadget_demo_regr = function(n = 240L) {
  set.seed(2026L)
  dat = data.frame(
    z = runif(n, -1, 1),
    x_signal = rnorm(n),
    x_noise = rnorm(n)
  )
  dat$y = ifelse(dat$z < 0, 2 * dat$x_signal, -2 * dat$x_signal) +
    0.2 * dat$x_noise +
    rnorm(n, sd = 0.1)
  mlr3::as_task_regr(y ~ ., data = dat, id = "gadget_demo_regr")
}

test_that("synthetic trigger configuration produces GADGET plots", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("rpart")

  task = make_task_gadget_demo_regr()
  learner = mlr3::lrn("regr.rpart")
  resampling = mlr3::rsmp("cv", folds = 3L)

  auto = mlr3autoiml::AutoIML$new(
    task = task,
    learner = learner,
    resampling = resampling,
    purpose = "global_insight",
    quick_start = FALSE,
    seed = 2026L,
    config = list(
      structure = list(
        sample_n = min(220L, task$nrow),
        max_features = 3L,
        grid_n = 10L,
        ale_bins = 10L,
        regionalize = TRUE,
        gadget_top_k = 2L,
        gadget_max_depth = 2L,
        gadget_gamma = 0,
        gadget_min_bucket = 20L,
        gadget_local_keep_n = 32L,
        pint_enabled = TRUE,
        pint_permutations = 5L
      )
    )
  )

  result = auto$run(verbose = FALSE)
  g2 = result$gate_results[["G2"]]

  expect_true(isTRUE(g2$metrics$interaction_flag[[1L]]))
  expect_true(isTRUE(g2$metrics$gadget_available[[1L]]))
  expect_true(isTRUE(g2$metrics$pint_available[[1L]]))
  expect_true(nrow(g2$artifacts$gadget_regions) > 0L)
  expect_true(nrow(g2$artifacts$gadget_splits) > 0L)
  expect_true(nrow(g2$artifacts$pint) > 0L)
  expect_true("x_signal" %in% g2$artifacts$gadget_multi$features)

  expect_s3_class(auto$plot("g2_gadget", feature = "x_signal"), "ggplot")
  tree_plot = auto$plot("g2_gadget_tree")
  expect_s3_class(tree_plot, "ggplot")
  layer_geoms = vapply(tree_plot$layers, function(layer) class(layer$geom)[1L], character(1L))
  expect_false("GeomPoint" %in% layer_geoms)
  expect_true("GeomCustomAnn" %in% layer_geoms)
  expect_s3_class(auto$plot("g2_pint"), "ggplot")
})