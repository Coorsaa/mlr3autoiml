test_that("mixed dependence returns support and pairwise tables", {
  dat = data.frame(
    a = 1:20,
    b = rev(1:20),
    c = factor(rep(c("x", "y"), 10))
  )
  out = csdg_dependence(dat)
  expect_equal(nrow(out$support), 3L)
  expect_equal(nrow(out$pairwise), 3L)
  expect_true(all(out$pairwise$method %in%
                    c("spearman", "cramers_v", "eta_squared")))
})

test_that("dependence handles a single feature", {
  out = csdg_dependence(data.frame(a = 1:10))
  expect_equal(nrow(out$support), 1L)
  expect_equal(nrow(out$pairwise), 0L)
  expect_named(
    out$pairwise,
    c("feature_1", "feature_2", "kind_1", "kind_2", "method", "association", "n_complete")
  )
})

test_that("Cramer's V ignores unused factor levels", {
  x = factor(c("a", "a", "b", "b"), levels = c("a", "b", "unused_x"))
  y = factor(c("u", "u", "v", "v"), levels = c("u", "v", "unused_y"))
  out = csdg_dependence(data.frame(x = x, y = y))

  expect_equal(.cramers_v(x, y), 1)
  expect_equal(.cramers_v(x, y), .cramers_v(droplevels(x), droplevels(y)))
  expect_equal(out$pairwise$association, 1)
})

test_that("calibration and decision curves use OOF-style tables", {
  pred = data.table::data.table(
    row_id = 1:8,
    truth = factor(c(0, 0, 0, 1, 1, 1, 1, 0), levels = c(0, 1)),
    response = factor(c(0, 0, 1, 1, 1, 1, 0, 0), levels = c(0, 1)),
    prob.0 = c(.9, .8, .4, .3, .2, .1, .6, .7),
    prob.1 = c(.1, .2, .6, .7, .8, .9, .4, .3)
  )
  cal = csdg_calibration(pred, task_type = "classif", positive = "1", bins = 4L)
  expect_true(all(c("summary", "curve") %in% names(cal)))
  expect_equal(cal$summary$n, 8L)
  expect_true(all(c(
    "calibration_in_the_large", "calibration_intercept", "calibration_slope"
  ) %in% names(cal$summary)))
  expect_false("calibration_model_intercept" %in% names(cal$summary))
  expect_false(is.unsorted(cal$curve$predicted, strictly = FALSE))

  dc = csdg_decision_curve(pred, positive = "1", thresholds = c(.25, .5, .75))
  expect_equal(nrow(dc), 3L)
  expect_true(all(is.finite(dc$net_benefit_model)))
})

test_that("subgroup audit does not refit models", {
  pred = data.table::data.table(
    row_id = 1:10,
    truth = factor(rep(c(0, 1), 5), levels = c(0, 1)),
    response = factor(rep(c(0, 1), 5), levels = c(0, 1)),
    prob.0 = rep(c(.8, .2), 5),
    prob.1 = rep(c(.2, .8), 5)
  )
  sg = data.table::data.table(
    row_id = 1:10,
    age_band = rep(c("younger", "older"), each = 5)
  )
  out = csdg_subgroup_metrics(
    pred, subgroup = sg, positive = "1", min_n = 3L
  )
  expect_equal(nrow(out$metrics), 2L)
  expect_match(out$metrics$evidence_scope[[1L]], "no subgroup refitting")
  expect_true(all(c(
    "decision_threshold", "threshold_degenerate", "calibration_in_the_large", "calibration_intercept"
  ) %in% names(out$metrics)))
})

test_that("subgroup audit reports reproducible conditional bootstrap intervals and contrasts", {
  pred = data.table::data.table(
    row_id = 1:20,
    truth = factor(rep(c(0, 1), 10), levels = c(0, 1)),
    response = factor(rep(c(0, 1), 10), levels = c(0, 1)),
    prob.0 = rep(c(.8, .2), 10),
    prob.1 = rep(c(.2, .8), 10)
  )
  sg = data.table::data.table(
    row_id = 1:20,
    age_band = rep(c("younger", "older"), each = 10)
  )
  cluster = data.table::data.table(row_id = 1:20, school = rep(1:10, each = 2))
  set.seed(91L)
  before = .Random.seed
  out = csdg_subgroup_metrics(
    pred,
    subgroup = sg,
    positive = "1",
    min_n = 3L,
    cluster = cluster,
    bootstrap_repetitions = 40L,
    seed = 17L
  )

  expect_identical(.Random.seed, before)
  expect_true(all(c("uncertainty", "contrasts") %in% names(out)))
  expect_equal(nrow(out$uncertainty[metric == "auc"]), 2L)
  expect_equal(nrow(out$contrasts[metric == "auc"]), 1L)
  expect_true(all(out$uncertainty$n_bootstrap > 0L))
  expect_true(all(out$uncertainty$bootstrap_unit == "school"))
  expect_match(out$uncertainty$interpretation[[1L]], "model-training variation is not included")
})

test_that("subgroup cluster bootstrap can preserve supplied strata", {
  pred = data.table::data.table(
    row_id = 1:24,
    truth = rep(c(400, 500, 600), 8),
    response = rep(c(410, 490, 590), 8)
  )
  sg = data.table::data.table(row_id = 1:24, group = rep(c("a", "b"), each = 12))
  cluster = data.table::data.table(row_id = 1:24, school = rep(1:12, each = 2))
  strata = data.table::data.table(row_id = 1:24, country = rep(c("north", "south"), each = 12))
  out = csdg_subgroup_metrics(
    pred,
    subgroup = sg,
    task_type = "regr",
    cluster = cluster,
    bootstrap_strata = strata,
    bootstrap_repetitions = 30L,
    seed = 19L
  )

  expect_true(all(out$uncertainty$bootstrap_unit == "school within country"))
  expect_match(out$limitations[[4L]], "independently within each supplied stratum")

  current = data.table::data.table(
    bootstrap_stratum = rep(c("north", "south"), each = 6),
    bootstrap_cluster = rep(1:6, each = 2),
    row_id = 1:12
  )
  set.seed(20L)
  sampled = .subgroup_bootstrap_sample(current)
  draws = sampled[, .(n_draws = data.table::uniqueN(bootstrap_draw)), by = bootstrap_stratum]
  expect_equal(draws$n_draws, c(3L, 3L))
})

test_that("dynamic G6 and G7 plot metrics are evaluated inside the plot data", {
  skip_if_not_installed("ggplot2")
  transport = list(setting_generalization = list(scores = data.table::data.table(
    held_out_group = c("a", "b"),
    n_train = c(80L, 80L),
    n_assessment = c(20L, 20L),
    `regr.rmse` = c(1.2, 1.4)
  )))
  subgroup = list(subgroup_audit = list(
    metrics = data.table::data.table(
      audit_variable = "group",
      subgroup = c("a", "b"),
      n = c(20L, 20L),
      rmse = c(1.1, 1.3)
    ),
    uncertainty = data.table::data.table(
      audit_variable = "group",
      subgroup = c("a", "b"),
      metric = "rmse",
      ci_low = c(1.0, 1.2),
      ci_high = c(1.2, 1.4)
    )
  ))

  expect_no_error(ggplot2::ggplot_build(csdg_plot(transport, gate = "G6b", type = "regr.rmse")))
  subgroup_plot = ggplot2::ggplot_build(csdg_plot(subgroup, gate = "G7a", type = "rmse"))
  expect_equal(sort(subgroup_plot$data[[1L]]$xmin), c(1.0, 1.2))
  expect_equal(sort(subgroup_plot$data[[1L]]$xmax), c(1.2, 1.4))
})

test_that("G1 plot explains descriptive ranges and mean points", {
  skip_if_not_installed("ggplot2")
  evidence = list(performance = list(summary = data.table::data.table(
    measure = c("regr.rmse", "regr.mae"),
    mean = c(1.2, 0.8),
    q10 = c(1.1, 0.7),
    q90 = c(1.3, 0.9)
  )))
  plot = csdg_plot(evidence, gate = "G1")
  built = ggplot2::ggplot_build(plot)
  range_layer = built$data[[1L]]

  expect_match(plot$labels$subtitle, "Points are means", fixed = TRUE)
  expect_match(plot$labels$subtitle, "\nnot confidence intervals", fixed = TRUE)
  expect_identical(plot$labels$x, "Held-out metric value")
  expect_identical(plot$facet$params$ncol, 2L)
  expect_equal(sort(range_layer$xmin), c(0.7, 1.1))
  expect_equal(sort(range_layer$xmax), c(0.9, 1.3))
  expect_true(all(range_layer$ymin < range_layer$ymax))
  expect_true(all(range_layer$colour == "#4C72B0"))
  expect_identical(plot$layers[[1L]]$aes_params$width, 0.06)
  expect_identical(plot$layers[[1L]]$aes_params$linewidth, 0.6)
  expect_identical(plot$layers[[2L]]$aes_params$size, 1.8)
  expect_identical(plot$layers[[3L]]$aes_params$size, 2.55)
  expect_true(all(abs(built$data[[3L]]$y - 1.07) < 1e-12))
})

test_that("csdg_plot base_size scales theme text and direct labels", {
  skip_if_not_installed("ggplot2")
  evidence = list(performance = list(summary = data.table::data.table(
    measure = "regr.rmse",
    mean = 1.2,
    q10 = 1.1,
    q90 = 1.3
  )))
  default_plot = csdg_plot(evidence, gate = "G1")
  larger_plot = csdg_plot(evidence, gate = "G1", base_size = 13)

  expect_identical(default_plot$theme$text$size, 11)
  expect_identical(default_plot$layers[[3L]]$aes_params$size, 2.55)
  expect_identical(larger_plot$theme$text$size, 13)
  expect_equal(larger_plot$layers[[3L]]$aes_params$size, 2.55 * 13 / 11)
  expect_identical(
    ggplot2::calc_element("panel.grid.major", default_plot$theme)$linewidth,
    ggplot2::calc_element("panel.grid.major", larger_plot$theme)$linewidth
  )
  expect_error(csdg_plot(evidence, gate = "G1", base_size = 5), "not >= 6", fixed = TRUE)
})

test_that("G1 plot uses a compact one-row layout for three performance measures", {
  skip_if_not_installed("ggplot2")
  evidence = list(performance = list(summary = data.table::data.table(
    measure = c("regr.rmse", "regr.mae", "regr.rsq"),
    mean = c(72.1, 56.8, 0.49),
    q10 = c(71.4, 56.2, 0.48),
    q90 = c(72.8, 57.4, 0.50)
  )))
  plot = csdg_plot(evidence, gate = "G1")
  x_scale = plot$scales$get_scales("x")

  expect_identical(plot$facet$params$ncol, 3L)
  expect_equal(x_scale$expand, ggplot2::expansion(mult = c(0.04, 0.20)))
  built = ggplot2::ggplot_build(plot)
  expect_true(all(vapply(built$layout$panel_params, function(panel) panel$x.range[[1L]] > 0, logical(1L))))
})

test_that("G7 plot uses compact conditional-interval geometry", {
  skip_if_not_installed("ggplot2")
  evidence = list(subgroup_audit = list(
    metrics = data.table::data.table(
      audit_variable = "group",
      subgroup = c("a", "b"),
      n = c(20L, 20L),
      rmse = c(1.1, 1.3)
    ),
    uncertainty = data.table::data.table(
      audit_variable = "group",
      subgroup = c("a", "b"),
      metric = "rmse",
      ci_low = c(1.0, 1.2),
      ci_high = c(1.2, 1.4)
    )
  ))
  plot = csdg_plot(evidence, gate = "G7a", type = "rmse")

  expect_identical(plot$layers[[1L]]$aes_params$width, 0.09)
  expect_identical(plot$layers[[1L]]$aes_params$linewidth, 0.7)
  expect_identical(plot$layers[[2L]]$aes_params$size, 1.9)
})

test_that("G5 plot keeps short descriptive ranges visible around mean points", {
  skip_if_not_installed("ggplot2")
  evidence = list(pfi = list(summary = data.table::data.table(
    feature_group = c("feature_a", "feature_b"),
    mean_importance = c(0.20, 0.001),
    q10_importance = c(0.18, 0.0009),
    q90_importance = c(0.22, 0.0011)
  )))
  plot = csdg_plot(evidence, gate = "G5")

  expect_s3_class(plot$layers[[2L]]$geom, "GeomErrorbar")
  expect_identical(plot$layers[[2L]]$aes_params$width, 0.28)
  expect_s3_class(plot$layers[[3L]]$geom, "GeomPoint")
  expect_identical(plot$layers[[3L]]$aes_params$size, 2.2)
})

test_that("G4 plots use pseudonymous labels instead of row identifiers", {
  skip_if_not_installed("ggplot2")
  evidence = list(summary = data.table::data.table(
    case_id = c("10963", "15811"),
    weighted_r2 = c(0.82, 0.91)
  ))
  plot = csdg_plot(evidence, gate = "G4")

  expect_setequal(plot$data$case_label, c("Communication case 1", "Communication case 2"))
  expect_false(any(plot$data$case_id %in% plot$data$case_label))
  expect_no_error(ggplot2::ggplot_build(plot))
})
