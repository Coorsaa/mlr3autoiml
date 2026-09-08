test_that("table-oriented estimate plots preserve blue-red interval semantics", {
  performance = data.table::data.table(
    learner_id = rep(c("xgboost", "ridge"), 2L),
    measure_id = rep(c("regr.rmse", "regr.rsq"), each = 2L),
    mean = c(1.0, 1.1, 0.50, 0.47),
    q10 = c(0.9, 1.0, 0.48, 0.45),
    q90 = c(1.1, 1.2, 0.52, 0.49)
  )
  performance_plot = csdg_plot_data(
    performance,
    type = "performance",
    base_size = 13,
    direct_label_size = 2.9,
    axis_text_size = 10.5,
    axis_title_size = 9.2
  )
  performance_built = ggplot2::ggplot_build(performance_plot)

  expect_s3_class(performance_plot, "ggplot")
  expect_equal(performance_plot$theme$text$size, 13)
  expect_equal(performance_plot$theme$axis.text$size, 10.5)
  expect_equal(performance_plot$theme$axis.title$size, 9.2)
  expect_true(all(performance_built$data[[1L]]$colour == "#4C72B0"))
  expect_true(all(performance_built$data[[2L]]$colour == "#C44E52"))
  expect_false(grepl("includes zero", performance_plot$labels$x, fixed = TRUE))
  expect_gt(performance_built$layout$panel_params[[1L]]$x.range[[1L]], 0)

  zero_plot = csdg_plot_data(performance, type = "performance", include_zero = TRUE)
  zero_built = ggplot2::ggplot_build(zero_plot)
  expect_lte(zero_built$layout$panel_params[[1L]]$x.range[[1L]], 0)

  focal_performance = list(summary = performance[learner_id == "xgboost", .(
    measure = measure_id,
    mean,
    q10,
    q90
  )])
  focal_plot = csdg_plot_data(focal_performance, type = "performance")
  expect_identical(levels(focal_plot$data$learner_label), "Focal model")

  edge_performance = data.table::data.table(
    learner_id = c("xgboost", "ridge", "random_forest", "featureless"),
    measure_id = "classif.bbrier",
    mean = c(0.0301, 0.0326, 0.0342, 0.0569),
    q10 = c(0.0297, 0.0321, 0.0337, 0.0566),
    q90 = c(0.0305, 0.0331, 0.0348, 0.0572)
  )
  edge_plot = csdg_plot_data(edge_performance, type = "performance", direct_label_size = 2.9)
  edge_built = ggplot2::ggplot_build(edge_plot)
  edge_row = edge_plot$data[estimate__ == max(estimate__)]
  expect_equal(edge_row$label_hjust, 1)
  expect_lt(edge_row$label_x, edge_row$low__)
  expect_gt(edge_built$layout$panel_params[[1L]]$x.range[[2L]], max(edge_plot$data$high__))
  edge_x_range = edge_built$layout$panel_params[[1L]]$x.range
  expect_gt((edge_row$label_x - edge_x_range[[1L]]) / diff(edge_x_range), 0.10)

  pfi = data.table::data.table(
    feature_group = paste0("feature_", 1:4),
    mean_importance = c(0.4, 0.3, 0.2, 0.1),
    q10_importance = c(0.35, 0.25, 0.15, 0.05),
    q90_importance = c(0.45, 0.35, 0.25, 0.15)
  )
  pfi_plot = csdg_plot_data(pfi, type = "pfi", top_n = 3L, direct_label_size = 3.4)
  pfi_built = ggplot2::ggplot_build(pfi_plot)

  expect_equal(nrow(pfi_built$data[[2L]]), 3L)
  expect_true(all(pfi_built$data[[2L]]$colour == "#4C72B0"))
  expect_true(all(pfi_built$data[[3L]]$colour == "#C44E52"))
  expect_equal(pfi_plot$layers[[4L]]$aes_params$size, 3.4)
  expect_equal(pfi_plot$data[estimate__ == max(estimate__), label_hjust], 1)
  expect_lt(pfi_plot$data[estimate__ == max(estimate__), label_x], max(pfi_plot$data$estimate__))
})

test_that("calibration plots compute valid Wilson intervals without an uninformative size legend", {
  calibration = data.table::data.table(
    predicted = c(0.05, 0.20, 0.55),
    observed = c(0.04, 0.18, 0.50),
    n = rep(100L, 3L)
  )
  plot = csdg_plot_data(calibration, type = "calibration")
  built = ggplot2::ggplot_build(plot)
  interval_layer = built$data[[2L]]

  expect_s3_class(plot, "ggplot")
  expect_true(all(interval_layer$ymin <= calibration$observed))
  expect_true(all(interval_layer$ymax >= calibration$observed))
  expect_true(all(interval_layer$colour == "#4C72B0"))
  expect_identical(plot$theme$legend.position, "none")
  expect_equal(plot$layers[[3L]]$aes_params$size, 1.2)
  expect_identical(plot$layers[[3L]]$aes_params$colour, "#C44E52")
  expect_false(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1L))))

  unequal = data.table::copy(calibration)
  unequal$n = c(50L, 100L, 200L)
  unequal_plot = csdg_plot_data(unequal, type = "calibration")
  expect_identical(unequal_plot$theme$legend.position, "none")
  expect_null(unequal_plot$scales$get_scales("size"))
  expect_equal(unequal_plot$layers[[3L]]$aes_params$size, 1.2)

  sparse = data.table::data.table(
    predicted = c(0.05, 0.10),
    observed = c(0.10, 0.20),
    n = c(10L, 10L),
    ci_low = NA_real_,
    ci_high = NA_real_
  )
  sparse_plot = csdg_plot_data(sparse, type = "calibration")
  sparse_built = ggplot2::ggplot_build(sparse_plot)
  sparse_intervals = sparse_built$data[[2L]]
  expect_true(all(is.finite(sparse_intervals$ymin)))
  expect_true(all(is.finite(sparse_intervals$ymax)))
  expect_gte(sparse_plot$coordinates$limits$y[[2L]], max(sparse_intervals$ymax))
  expect_identical(sparse_plot$coordinates$limits$x, sparse_plot$coordinates$limits$y)

  expect_error(
    csdg_plot_data(
      transform(calibration, ci_low = observed + 0.01, ci_high = observed + 0.02),
      type = "calibration"
    ),
    "valid for their estimand"
  )
  expect_error(
    csdg_plot_data(transform(calibration, ci_low = NA_real_), type = "calibration"),
    "both `ci_low` and `ci_high`"
  )
  expect_error(
    csdg_plot_data(
      transform(calibration, ci_low = c(NA, 0.10, 0.40), ci_high = c(NA, 0.25, 0.60)),
      type = "calibration"
    ),
    "must be finite"
  )

  regression = list(curve = data.table::data.table(
    truth = c(1.0, 2.1, 2.8, 4.2),
    prediction = c(1.2, 1.9, 3.0, 4.0)
  ))
  regression_plot = csdg_plot_data(regression, type = "calibration")
  expect_s3_class(regression_plot$layers[[2L]]$geom, "GeomSmooth")

  pisa_regression_bins = data.table::data.table(
    predicted_value = c(420, 480, 540),
    observed_value = c(425, 477, 536),
    n = c(4800L, 4800L, 4800L),
    ci_low = NA_real_,
    ci_high = NA_real_
  )
  pisa_plot = csdg_plot_data(pisa_regression_bins, type = "calibration")
  pisa_built = ggplot2::ggplot_build(pisa_plot)
  expect_s3_class(pisa_plot, "ggplot")
  expect_false(any(vapply(pisa_built$data, function(layer) "ymin" %in% names(layer), logical(1L))))
})

test_that("flexible calibration curves use pointwise interval ribbons without bin markers", {
  prediction = seq(0.05, 0.75, length.out = 25L)
  curve = data.table::data.table(
    prediction = prediction,
    estimate = pmin(1, 0.01 + 0.96 * prediction),
    lower = pmax(0, 0.01 + 0.96 * prediction - 0.03),
    upper = pmin(1, 0.01 + 0.96 * prediction + 0.03)
  )
  plot = csdg_plot_data(curve, type = "calibration")

  expect_true(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1L))))
  expect_true(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1L))))
  expect_false(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1L))))
})

test_that("model, rank, and setting plots accept package result structures", {
  candidates = data.table::data.table(
    learner_name = c("xgboost", "ridge", "random_forest"),
    learner_id = c("regr.xgboost", "regr.glmnet", "regr.ranger"),
    primary_measure = "regr.rmse",
    mean_score = c(1.00, 1.03, 1.08),
    accepted = c(TRUE, TRUE, FALSE),
    acceptance_limit = 1.05,
    direction = "minimize"
  )
  fold_scores = data.table::CJ(learner_id = candidates$learner_name, fold = 1:3)
  fold_scores[, `:=`(
    iteration = fold,
    repetition = 1L,
    measure_id = "regr.rmse",
    estimate = rep(c(1.00, 1.03, 1.08), each = 3L) + c(-0.01, 0, 0.01)
  )]
  model_plot = csdg_plot_data(
    list(candidates = candidates),
    type = "model_comparison",
    fold_scores = fold_scores,
    reference_learner = "xgboost",
    direct_label_size = 3.6,
    limit_label_size = 3.5
  )
  model_built = ggplot2::ggplot_build(model_plot)

  expect_s3_class(model_plot, "ggplot")
  expect_true(any(vapply(model_built$data, function(layer) any(layer[["x"]] == 0), logical(1L))))
  expect_match(model_plot$labels$x, "difference from XGBoost")

  ranks = data.table::data.table(
    learner_name = rep(c("xgboost", "ridge", "random_forest"), each = 3L),
    feature_group = rep(c("a", "b", "c"), 3L),
    rank = c(1, 2, 3, 2, 1, 3, 1, 3, 2)
  )
  rank_plot = csdg_plot_data(
    list(ranks = ranks),
    type = "rank_heatmap",
    learner_order = c("xgboost", "random_forest", "ridge"),
    cell_label_size = 4.2
  )
  expect_equal(rank_plot$layers[[2L]]$aes_params$size, 4.2)
  expect_equal(levels(rank_plot$data$learner_label), c("XGBoost", "Random forest", "Ridge regression"))

  importance_values = data.table::data.table(
    learner_name = rep(c("xgboost", "ridge", "random_forest"), each = 3L),
    feature_group = rep(c("a", "b", "c"), 3L),
    mean_importance = c(0.4, 0.2, 0.1, 0.3, 0.25, 0.05, 0.35, 0.15, 0.12),
    minimum_importance = c(0.35, 0.15, 0.08, 0.25, 0.2, 0.02, 0.3, 0.1, 0.08),
    maximum_importance = c(0.45, 0.25, 0.12, 0.35, 0.3, 0.08, 0.4, 0.2, 0.16),
    rank = rep(1:3, 3L)
  )
  importance_plot = csdg_plot_data(
    importance_values,
    type = "importance_comparison",
    learner_order = c("xgboost", "random_forest", "ridge"),
    top_n = 2L
  )
  importance_built = ggplot2::ggplot_build(importance_plot)
  expect_s3_class(importance_plot, "ggplot")
  expect_equal(levels(importance_plot$data$learner_label), c("XGBoost", "Random forest", "Ridge regression"))
  expect_true(all(importance_built$data[[3L]]$shape == 16L))

  setting = list(scores = data.table::data.table(
    held_out_group = c("A", "B", "C"),
    n_train = 100L,
    n_assessment = 50L,
    `regr.rmse` = c(1.2, 1.0, 1.1)
  ))
  setting_plot = csdg_plot_data(setting, type = "setting_generalization", measure = "regr.rmse")
  expect_s3_class(setting_plot, "ggplot")
  expect_identical(setting_plot$labels$x, "RMSE")
})

test_that("dependence plots rank features and apply publication labels", {
  features = paste0("x", 1:4)
  dependence = data.table::CJ(feature_1 = features, feature_2 = features)[feature_1 < feature_2]
  dependence[, association := seq(0.1, 0.6, length.out = .N)]
  labels = setNames(paste("Feature", 1:4), features)
  plot = csdg_plot_data(
    list(pairwise = dependence),
    type = "dependence",
    feature_labels = labels,
    top_n = 3L,
    axis_text_size = 12.5,
    legend_text_size = 12.5
  )
  built = ggplot2::ggplot_build(plot)

  expect_s3_class(plot, "ggplot")
  expect_equal(length(levels(plot$data$feature_1)), 3L)
  expect_true(all(unname(plot$scales$get_scales("x")$labels) %in% labels))
  expect_true(all(built$data[[1L]]$fill != "grey90"))
  expect_equal(plot$theme$axis.text$size, 12.5)
  expect_equal(plot$theme$legend.text$size, 12.5)
})

test_that("measurement trajectories and setting penalties retain descriptive semantics", {
  trajectory = data.table::CJ(
    outcome_id = paste0("PV", 1:3, "READ"),
    learner_id = c("xgboost", "ridge"),
    measure_id = c("regr.rmse", "regr.rsq")
  )
  trajectory[, estimate := seq(0.2, 1.3, length.out = .N)]
  trajectory_plot = csdg_plot_data(
    trajectory,
    type = "measurement_trajectory",
    outcome_order = paste0("PV", 1:3, "READ")
  )
  legend_plot = csdg_plot_legend(trajectory_plot)

  expect_s3_class(trajectory_plot, "ggplot")
  expect_s3_class(legend_plot, "ggplot")
  expect_equal(levels(trajectory_plot$data$outcome_label), paste0("PV", 1:3, "READ"))
  expect_identical(trajectory_plot$theme$legend.position, "bottom")
  expect_gt(ggplot2::ggplot_build(trajectory_plot)$layout$panel_params[[1L]]$y.range[[1L]], 0)

  zero_trajectory = csdg_plot_data(
    trajectory,
    type = "measurement_trajectory",
    outcome_order = paste0("PV", 1:3, "READ"),
    include_zero = TRUE
  )
  expect_lte(ggplot2::ggplot_build(zero_trajectory)$layout$panel_params[[1L]]$y.range[[1L]], 0)

  penalties = data.table::data.table(
    held_out_group = c("A", "B", "C"),
    mean_penalty = c(-0.2, 0.1, 0.4),
    minimum_penalty = c(-0.4, -0.1, 0.2),
    maximum_penalty = c(0, 0.3, 0.7)
  )
  penalty_plot = csdg_plot_data(penalties, type = "setting_penalty")
  penalty_built = ggplot2::ggplot_build(penalty_plot)

  expect_s3_class(penalty_plot, "ggplot")
  expect_true(any(vapply(penalty_built$data, function(layer) any(layer$xintercept == 0), logical(1L))))
  expect_true(all(penalty_built$data[[2L]]$colour == "#4C72B0"))
  expect_true(all(penalty_built$data[[3L]]$colour == "#C44E52"))
})

test_that("subgroup plots retain conditional intervals, mappings, and contrasts", {
  subgroup = list(
    metrics = data.table::data.table(
      audit_variable = rep(c("audit_gender", "audit_age"), each = 2L),
      subgroup = c("woman", "man", "young", "older"),
      n = c(100L, 110L, 80L, 130L),
      auc = c(0.80, 0.82, 0.78, 0.84)
    ),
    uncertainty = data.table::data.table(
      audit_variable = rep(c("audit_gender", "audit_age"), each = 2L),
      subgroup = c("woman", "man", "young", "older"),
      metric = "auc",
      ci_low = c(0.77, 0.79, 0.74, 0.81),
      ci_high = c(0.83, 0.85, 0.82, 0.87)
    ),
    contrasts = data.table::data.table(
      audit_variable = c("audit_gender", "audit_age"),
      subgroup_1 = c("woman", "young"),
      subgroup_2 = c("man", "older"),
      metric = "auc",
      estimate_difference = c(-0.02, -0.06),
      ci_low = c(-0.05, -0.10),
      ci_high = c(0.01, -0.02)
    )
  )
  subgroup$uncertainty = data.table::rbindlist(list(
    subgroup$uncertainty,
    data.table::copy(subgroup$uncertainty)[, `:=`(metric = "rmse", ci_low = 1, ci_high = 2)]
  ))
  subgroup$contrasts = data.table::rbindlist(list(
    subgroup$contrasts,
    data.table::copy(subgroup$contrasts)[, `:=`(
      metric = "rmse",
      estimate_difference = 1.5,
      ci_low = 1,
      ci_high = 2
    )]
  ))
  labels = c("audit_gender::woman" = "Women", "audit_gender::man" = "Men")
  subgroup_plot = csdg_plot_data(
    subgroup,
    type = "subgroup",
    metric = "auc",
    audit_labels = c(audit_gender = "Gender", audit_age = "Age group"),
    subgroup_labels = labels,
    subgroup_order = c("young", "older", "Women", "Men"),
    x_limits = c(0.5, 1),
    direct_label_size = 3
  )
  subgroup_built = ggplot2::ggplot_build(subgroup_plot)

  expect_equal(sort(subgroup_built$data[[1L]]$xmin), sort(subgroup$uncertainty[metric == "auc", ci_low]))
  expect_equal(sort(subgroup_built$data[[1L]]$xmax), sort(subgroup$uncertainty[metric == "auc", ci_high]))
  expect_setequal(as.character(subgroup_plot$data$audit_label), c("Gender", "Age group"))
  expect_true(all(c("Women", "Men") %in% as.character(subgroup_plot$data$subgroup_label)))
  expect_identical(subgroup_plot$labels$x, "ROC AUC")
  expect_true(all(subgroup_plot$data$label_hjust == 1))
  expect_equal(subgroup_plot$scales$get_scales("x")$limits, c(0.5, 1))

  long_labels = list(
    metrics = data.table::data.table(
      audit_variable = "audit_immigration",
      subgroup = c("native", "second_generation", "first_generation"),
      n = c(23120L, 22234L, 2658L),
      rmse = c(84.921, 89.847, 104.213)
    ),
    uncertainty = data.table::data.table(
      audit_variable = "audit_immigration",
      subgroup = c("native", "second_generation", "first_generation"),
      metric = "rmse",
      ci_low = c(82.5, 87.2, 100.1),
      ci_high = c(87.4, 92.6, 108.8)
    )
  )
  long_label_plot = csdg_plot_data(
    long_labels,
    type = "subgroup",
    metric = "rmse",
    x_limits = c(0, 115),
    direct_label_size = 3
  )
  expect_identical(long_label_plot$labels$x, "RMSE")
  expect_true(all(long_label_plot$data$label_hjust == 1))
  expect_true(all(long_label_plot$data$label_x < long_label_plot$data$value__))
  expect_equal(long_label_plot$scales$get_scales("x")$limits, c(0, 115))

  contrast_plot = csdg_plot_data(
    subgroup,
    type = "subgroup_contrast",
    metric = "auc",
    audit_labels = c(audit_gender = "Gender", audit_age = "Age group"),
    subgroup_labels = labels,
    caption = "Conditional intervals",
    direct_label_size = 3.8
  )
  expect_identical(contrast_plot$labels$caption, "Conditional intervals")
  expect_equal(nrow(contrast_plot$data), 2L)
  expect_true(any(grepl("Women - Men", contrast_plot$data$axis_label, fixed = TRUE)))
})

test_that("local fidelity and effect plots use validated aggregate inputs", {
  fidelity = list(cases = data.table::data.table(
    case_label = c("Case 1", "Case 2"),
    median_weighted_r2 = c(0.83, 0.91),
    fidelity_threshold = 0.80
  ))
  fidelity_plot = csdg_plot_data(fidelity, type = "local_fidelity")
  expect_s3_class(fidelity_plot, "ggplot")
  expect_true(all(ggplot2::ggplot_build(fidelity_plot)$data[[3L]]$colour == "#C44E52"))

  ale = data.table::data.table(
    feature = rep(c("x1", "x2"), each = 3L),
    x_left = rep(0:2, 2L),
    x_right = rep(1:3, 2L),
    x = rep(c(0.5, 1.5, 2.5), 2L),
    ale = c(-0.1, 0, 0.1, -0.2, 0, 0.2),
    n_interval = 20L,
    supported = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )
  expect_s3_class(csdg_plot_data(ale, type = "ale"), "ggplot")

  ale_interval = data.table::copy(ale)
  ale_interval[, `:=`(ale_lower = ale - 0.04, ale_upper = ale + 0.04)]
  ale_interval[supported == FALSE, `:=`(ale_lower = NA_real_, ale_upper = NA_real_)]
  ale_interval_plot = csdg_plot_data(ale_interval, type = "ale", style = "monochrome")
  ale_interval_built = ggplot2::ggplot_build(ale_interval_plot)
  expect_s3_class(ale_interval_plot$layers[[2L]]$geom, "GeomRibbon")
  expect_true(all(ale_interval_built$data[[2L]]$fill == "grey70"))
  expect_gte(
    ale_interval_plot$scales$get_scales("y")$limits[[2L]],
    max(ale_interval$ale_upper, na.rm = TRUE)
  )
  partial_interval = data.table::copy(ale_interval)
  partial_interval[, interval_supported := supported]
  partial_interval[feature == "x1" & x == 1.5, `:=`(
    interval_supported = FALSE,
    ale_lower = NA_real_,
    ale_upper = NA_real_
  )]
  partial_interval_plot = csdg_plot_data(partial_interval, type = "ale")
  expect_true(any(partial_interval_plot$data$supported & !partial_interval_plot$data$interval_supported))
  expect_equal(
    nrow(partial_interval_plot$layers[[2L]]$data),
    sum(partial_interval$supported & partial_interval$interval_supported)
  )
  expect_error(
    csdg_plot_data(data.table::copy(ale_interval)[, ale_upper := NULL], type = "ale"),
    "must contain both"
  )
  invalid_ale_interval = data.table::copy(ale_interval)
  invalid_ale_interval[supported == TRUE, ale_lower := ale_upper + 0.01]
  expect_error(csdg_plot_data(invalid_ale_interval, type = "ale"), "finite ordered")

  ice = data.table::data.table(
    feature = rep(c("x1", "x2"), each = 3L),
    x = rep(1:3, 2L),
    q05_prediction = c(0.1, 0.2, 0.3, 0.2, 0.3, 0.4),
    q25_prediction = c(0.2, 0.3, 0.4, 0.3, 0.4, 0.5),
    median_prediction = c(0.3, 0.4, 0.5, 0.4, 0.5, 0.6),
    q75_prediction = c(0.4, 0.5, 0.6, 0.5, 0.6, 0.7),
    q95_prediction = c(0.5, 0.6, 0.7, 0.6, 0.7, 0.8),
    n_curves = 100L
  )
  ice_plot = csdg_plot_data(ice, type = "ice", y_limits = c(0, 1))
  ice_built = ggplot2::ggplot_build(ice_plot)
  expect_s3_class(ice_plot, "ggplot")
  expect_equal(
    as.character(ice_built$plot$scales$get_scales("fill")$get_breaks()),
    c("5th-95th percentile band", "25th-75th percentile band")
  )
  expect_equal(
    as.character(ice_built$plot$scales$get_scales("colour")$get_breaks()),
    "Pointwise median"
  )
  expect_identical(ice_plot$theme$legend.position, "bottom")
  expect_gt(length(ice_built$layout$panel_params[[1L]]$y$breaks), 0L)
  expect_error(
    csdg_plot_data(
      transform(ice, q25_prediction = q95_prediction + 0.01),
      type = "ice"
    ),
    "finite, ordered"
  )
  invalid_ice_x = data.table::copy(ice)
  invalid_ice_x[, x := as.numeric(x)]
  invalid_ice_x[1L, x := Inf]
  expect_error(csdg_plot_data(invalid_ice_x, type = "ice"), "finite x value")
  expect_error(csdg_plot_data(transform(ice, feature = ""), type = "ice"), "non-empty feature")
  expect_error(csdg_plot_data(rbind(ice, ice[1L]), type = "ice"), "duplicate feature-x")
  expect_error(csdg_plot_data(ice, type = "ice", y_limits = c(0.2, 0.7)), "contain every pointwise")
  logit_breaks = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90)
  ice_logit_plot = csdg_plot_data(
    ice,
    type = "ice",
    y_scale = "logit",
    y_limits = c(0.05, 0.90),
    y_breaks = logit_breaks
  )
  ice_logit_scale = ice_logit_plot$scales$get_scales("y")
  expect_identical(ice_logit_scale$trans$name, "prob-logis")
  expect_equal(ice_logit_scale$trans$inverse(ice_logit_scale$limits), c(0.05, 0.90))
  expect_equal(ice_logit_scale$breaks, logit_breaks)
  invalid_ice_probability = data.table::copy(ice)
  invalid_ice_probability[1L, q05_prediction := 0]
  expect_error(
    csdg_plot_data(invalid_ice_probability, type = "ice", y_scale = "logit"),
    "strictly between zero and one"
  )

  interaction = data.table::data.table(
    feature_1 = c("x1", "x1"),
    feature_2 = c("x2", "x3"),
    h_statistic = c(0.2, NA_real_)
  )
  interaction_plot = csdg_plot_data(interaction, type = "interaction")
  interaction_built = ggplot2::ggplot_build(interaction_plot)
  expect_s3_class(interaction_plot, "ggplot")
  expect_lt(interaction_built$layout$panel_params[[1L]]$x.range[[1L]], 0)
  expect_lt(interaction_built$layout$panel_params[[1L]]$x.range[[2L]], 0.5)
  expect_error(
    csdg_plot_data(interaction, type = "interaction", x_limits = c(0, 0.1)),
    "must include zero"
  )

  ale_2d = data.table::data.table(
    feature1 = "x1",
    feature2 = "x2",
    x1_left = c(0, 1),
    x1_right = c(1, 2),
    x2_bottom = c(0, 0),
    x2_top = c(1, 1),
    ale2d = c(-0.2, 0.2)
  )
  expect_s3_class(csdg_plot_data(ale_2d, type = "ale_2d"), "ggplot")
})

test_that("gate-status plots support single reports and multi-study matrices", {
  single = data.table::data.table(
    gate_id = c("G0a", "G1", "G2"),
    gate_name = c("Claim", "Performance", "Structure"),
    status = c("met", "unresolved", "not_applicable")
  )
  single_plot = csdg_plot_data(
    single,
    type = "gate_status",
    gate_order = c("G2", "G1", "G0a"),
    cell_label_size = 3.7
  )

  expect_s3_class(single_plot, "ggplot")
  expect_equal(single_plot$layers[[2L]]$aes_params$size, 3.7)
  expect_true(all(grepl("G", levels(single_plot$data$gate_label__), fixed = TRUE)))
  expect_identical(
    rev(as.character(levels(single_plot$data$gate_label__))),
    c("G2 - Structure", "G1 - Performance", "G0a - Claim")
  )

  matrix = data.table::CJ(study = c("SHILD", "PISA"), gate_id = c("G1", "G2"))
  matrix[, status := c("met", "unresolved", "not_met", "error")]
  matrix_plot = csdg_plot_data(
    matrix,
    type = "gate_status",
    gate_order = c("G1", "G2"),
    group_order = c("SHILD", "PISA"),
    cell_label_size = 3.8
  )
  matrix_built = ggplot2::ggplot_build(matrix_plot)

  expect_s3_class(matrix_plot, "ggplot")
  expect_equal(matrix_plot$layers[[2L]]$aes_params$size, 3.8)
  expect_setequal(matrix_built$data[[1L]]$fill, c("#4C72B0", "#D98C8F", "#C44E52", "#8F2D31"))
  expect_identical(matrix_plot$labels$x, "Diagnostic gate")
  expect_error(
    csdg_plot_data(transform(single, status = "unknown"), type = "gate_status"),
    "supported status"
  )
  expect_error(
    csdg_plot_data(matrix[-1L], type = "gate_status"),
    "complete group-by-gate grid"
  )
  expect_error(
    csdg_plot_data(
      matrix,
      type = "gate_status",
      group_labels = c(SHILD = "Study", PISA = "Study")
    ),
    "unique non-empty label"
  )
})

test_that("explanation-sensitivity plots use bounded fractions and origin-safe labels", {
  sensitivity = data.table::data.table(
    study = rep(c("SHILD", "PISA"), each = 3L),
    learner_name = rep(c("xgboost", "ridge", "random_forest"), 2L),
    learner_label = rep(c("XGBoost", "Ridge regression", "Random forest"), 2L),
    accepted = TRUE,
    performance_tolerance_fraction = c(0, 0.35, 0.80, 0, 0.45, 0.90),
    rank_discordance_vs_focal = c(0, 0.08, 0.20, 0, 0.12, 0.30)
  )
  plot = csdg_plot_data(
    sensitivity,
    type = "explanation_sensitivity",
    group_order = c("SHILD", "PISA"),
    group_colors = c(SHILD = "#C44E52", PISA = "#4C72B0"),
    y_limits = c(0, 0.35),
    direct_label_size = 4.1,
    caption = "Descriptive sensitivity only."
  )
  built = ggplot2::ggplot_build(plot)

  expect_s3_class(plot, "ggplot")
  expect_equal(length(built$layout$panel_params), 2L)
  expect_equal(plot$layers[[4L]]$aes_params$size, 4.1)
  expect_true(all(plot$data[performance_fraction__ == 0, label_x__] > 0))
  expect_equal(plot$scales$get_scales("x")$limits, c(0, 1.03))
  expect_equal(plot$scales$get_scales("y")$limits, c(0, 0.35))
  expect_lt(built$layout$panel_params[[1L]]$x.range[[1L]], 0)
  expect_lt(built$layout$panel_params[[1L]]$y.range[[1L]], 0)
  expect_gt(built$layout$panel_params[[1L]]$x.range[[2L]], 1.03)
  expect_gt(built$layout$panel_params[[1L]]$y.range[[2L]], 0.35)

  relabeled = data.table::copy(sensitivity)
  relabeled[, study := fifelse(study == "SHILD", "s1", "s2")]
  relabeled_plot = csdg_plot_data(
    relabeled,
    type = "explanation_sensitivity",
    learner_labels = c(
      xgboost = "Focal gradient boosting",
      ridge = "Penalized linear model",
      random_forest = "Forest"
    ),
    group_order = c("s1", "s2"),
    group_labels = c(s1 = "Study A", s2 = "Study B"),
    group_colors = c(s1 = "#C44E52", s2 = "#4C72B0"),
    y_limits = c(0, 0.35)
  )
  expect_setequal(
    unique(relabeled_plot$data$learner_label__),
    c("Focal gradient boosting", "Penalized linear model", "Forest")
  )
  expect_identical(
    unname(relabeled_plot$scales$get_scales("colour")$palette(2L)),
    c("#C44E52", "#4C72B0")
  )
  expect_s3_class(
    csdg_plot_data(
      relabeled,
      type = "explanation_sensitivity",
      group_order = c("s1", "s2"),
      group_labels = c(s1 = "Study A", s2 = "Study B"),
      group_colors = c("Study A" = "#C44E52", "Study B" = "#4C72B0"),
      y_limits = c(0, 0.35)
    ),
    "ggplot"
  )
  expect_error(
    csdg_plot_data(
      relabeled,
      type = "explanation_sensitivity",
      group_labels = c(s1 = "Study", s2 = "Study"),
      y_limits = c(0, 0.35)
    ),
    "unique non-empty label"
  )
  expect_error(
    csdg_plot_data(
      relabeled,
      type = "explanation_sensitivity",
      group_order = c("s1", "s2"),
      group_labels = c(s1 = "Study A", s2 = "Study B"),
      group_colors = c(
        s1 = "#C44E52", s2 = "#4C72B0", "Study A" = "#4C72B0", "Study B" = "#C44E52"
      ),
      y_limits = c(0, 0.35)
    ),
    "ambiguous"
  )
  expect_identical(plot$labels$caption, "Descriptive sensitivity only.")

  aligned = csdg_plot_data(
    sensitivity,
    type = "explanation_sensitivity",
    display = "aligned",
    include_focal = FALSE,
    reference_learner = "xgboost",
    learner_order = c("ridge", "random_forest"),
    group_order = c("SHILD", "PISA"),
    x_limits = c(0, 1),
    style = "monochrome"
  )
  aligned_built = ggplot2::ggplot_build(aligned)
  expect_equal(nrow(aligned$data), 8L)
  expect_equal(length(aligned_built$layout$panel_params), 2L)
  expect_false(any(vapply(aligned$layers, function(layer) inherits(layer$geom, "GeomRect"), logical(1L))))
  expect_setequal(aligned_built$data[[1L]]$shape, 21L)
  expect_setequal(aligned_built$data[[2L]]$shape, 22L)
  expect_identical(rev(levels(aligned$data$learner_label__)), c("Ridge regression", "Random forest"))
  expect_error(
    csdg_plot_data(sensitivity, type = "explanation_sensitivity", display = "aligned", include_focal = FALSE),
    "reference_learner"
  )

  diverging = csdg_plot_data(
    sensitivity,
    type = "explanation_sensitivity",
    display = "diverging",
    include_focal = FALSE,
    reference_learner = "xgboost",
    learner_order = c("ridge", "random_forest"),
    group_order = c("SHILD", "PISA"),
    x_limits = c(-1, 1),
    bar_width = 0.24,
    style = "monochrome"
  )
  diverging_built = ggplot2::ggplot_build(diverging)
  layer_classes = unname(vapply(diverging$layers, function(layer) class(layer$geom)[[1L]], character(1L)))
  expect_identical(layer_classes, c("GeomVline", "GeomCol", "GeomCol"))
  expect_false(any(vapply(diverging$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1L))))
  expect_true(all(diverging$data$reversal_display__ <= 0))
  expect_true(all(diverging$data$performance_display__ >= 0))
  expect_equal(length(diverging_built$layout$panel_params), 2L)
  expect_equal(diverging$layers[[2L]]$aes_params$width, 0.24)
  expect_equal(diverging$layers[[3L]]$aes_params$width, 0.24)
  expect_identical(diverging$theme$text$family, "Arial")
  expect_error(
    csdg_plot_data(sensitivity, type = "explanation_sensitivity", display = "diverging", x_limits = c(0, 1)),
    "straddle zero"
  )
  expect_error(
    csdg_plot_data(sensitivity, type = "explanation_sensitivity", display = "diverging", bar_width = 0.01),
    "not >= 0.05",
    fixed = TRUE
  )
  expect_error(
    csdg_plot_data(sensitivity, type = "explanation_sensitivity", display = "aligned", bar_width = 0.24),
    "applies only to the diverging"
  )

  direct = sensitivity[study == "SHILD", .(
    learner_name,
    fraction_of_prespecified_tolerance = performance_tolerance_fraction,
    full_pair_reversal_rate_vs_focal = rank_discordance_vs_focal
  )]
  expect_s3_class(csdg_plot_data(direct, type = "explanation_sensitivity"), "ggplot")
  expect_error(
    csdg_plot_data(
      transform(direct, fraction_of_prespecified_tolerance = 1.1),
      type = "explanation_sensitivity"
    ),
    "between zero and one"
  )
})

test_that("multiplicity and decision-curve plots use aggregate scientific tables", {
  bins = data.table::data.table(
    bin_left = c(0, 0.1, 0.2, 0.3),
    bin_right = c(0.1, 0.2, 0.3, 0.4),
    N = c(40L, 30L, 20L, 10L)
  )
  multiplicity_plot = csdg_plot_data(bins, type = "multiplicity")
  multiplicity_built = ggplot2::ggplot_build(multiplicity_plot)

  expect_s3_class(multiplicity_plot, "ggplot")
  expect_s3_class(multiplicity_plot$layers[[1L]]$geom, "GeomStep")
  expect_identical(multiplicity_plot$labels$y, "Cumulative share of rows (binned)")
  expect_true(all(multiplicity_built$data[[1L]]$colour == "#4C72B0"))
  expect_true(all(multiplicity_built$data[[5L]]$colour == "#C44E52"))
  expect_identical(multiplicity_plot$theme$legend.position, "none")
  expect_true(any(vapply(
    multiplicity_plot$layers,
    function(layer) inherits(layer$geom, "GeomLabel"),
    logical(1L)
  )))
  expect_s3_class(tail(multiplicity_plot$layers, 1L)[[1L]]$geom, "GeomPoint")
  curve = multiplicity_built$data[[1L]][, c("x", "y")]
  markers = multiplicity_built$data[[5L]][, c("x", "y")]
  expect_true(all(vapply(seq_len(nrow(markers)), function(index) {
    any(abs(curve$x - markers$x[[index]]) < 1e-12 & abs(curve$y - markers$y[[index]]) < 1e-12)
  }, logical(1L))))

  decision = data.table::data.table(
    threshold = c(0.1, 0.2, 0.3),
    net_benefit_model = c(0.20, 0.15, 0.10),
    net_benefit_treat_all = c(0.18, 0.10, 0.02),
    net_benefit_treat_none = 0
  )
  decision_plot = csdg_plot_data(decision, type = "decision_curve")
  expect_s3_class(decision_plot, "ggplot")
  expect_setequal(levels(decision_plot$data$strategy), c("Focal model", "Treat all", "Treat none"))

  expect_error(csdg_plot_data(bins, type = "multiplicity", bogus = TRUE), "Unknown option")
  expect_error(csdg_plot_data(bins, type = "multiplicity", base_size = 5), "not >= 6", fixed = TRUE)
})

test_that("explicit monochrome styles remain identifiable without color", {
  is_achromatic = function(colors) {
    channels = grDevices::col2rgb(unique(colors[!is.na(colors)]))
    all(channels[1L, ] == channels[2L, ] & channels[2L, ] == channels[3L, ])
  }

  expect_identical(autoiml_palette(), autoiml_palette("color"))
  expect_true(is_achromatic(unlist(autoiml_palette("monochrome"), use.names = FALSE)))
  expect_true(is_achromatic(autoiml_model_colors("monochrome")))
  expect_error(autoiml_palette("sepia"), "should be one of")

  performance = data.table::data.table(
    learner_id = c("xgboost", "ridge"),
    measure_id = "regr.rmse",
    mean = c(1.0, 1.1),
    q10 = c(0.9, 1.0),
    q90 = c(1.1, 1.2)
  )
  performance_plot = csdg_plot_data(performance, type = "performance", style = "monochrome")
  performance_built = ggplot2::ggplot_build(performance_plot)
  expect_true(is_achromatic(c(performance_built$data[[1L]]$colour, performance_built$data[[2L]]$colour)))
  expect_true(all(performance_built$data[[2L]]$shape == 21L))
  expect_true(all(performance_built$data[[2L]]$fill == "white"))

  gate_plot = csdg_plot(
    list(performance = list(summary = performance[, .(measure = measure_id, mean, q10, q90)])),
    gate = "G1",
    style = "monochrome"
  )
  gate_built = ggplot2::ggplot_build(gate_plot)
  expect_true(is_achromatic(c(gate_built$data[[1L]]$colour, gate_built$data[[2L]]$colour)))
  expect_true(all(gate_built$data[[2L]]$shape == 21L))

  trajectory = data.table::CJ(
    outcome_id = c("PV1", "PV2"),
    learner_id = c("xgboost", "ridge")
  )
  trajectory[, `:=`(measure_id = "regr.rmse", estimate = c(1.0, 1.2, 1.1, 1.3))]
  trajectory_built = ggplot2::ggplot_build(csdg_plot_data(
    trajectory,
    type = "measurement_trajectory",
    style = "monochrome"
  ))
  expect_setequal(trajectory_built$data[[1L]]$colour, "#111111")
  expect_gt(data.table::uniqueN(trajectory_built$data[[1L]]$linetype), 1L)
  expect_gt(data.table::uniqueN(trajectory_built$data[[2L]]$shape), 1L)

  ranks = data.table::data.table(
    learner_name = rep(c("xgboost", "ridge"), each = 3L),
    feature_group = rep(c("a", "b", "c"), 2L),
    rank = c(1, 2, 3, 2, 1, 3)
  )
  rank_built = ggplot2::ggplot_build(csdg_plot_data(ranks, type = "rank_heatmap", style = "monochrome"))
  expect_true(is_achromatic(rank_built$data[[1L]]$fill))

  importance_values = data.table::data.table(
    learner_name = rep(c("xgboost", "ridge"), each = 3L),
    feature_group = rep(c("a", "b", "c"), 2L),
    mean_importance = c(0.4, 0.2, 0.1, 0.3, 0.25, 0.05),
    minimum_importance = c(0.35, 0.15, 0.08, 0.25, 0.2, 0.02),
    maximum_importance = c(0.45, 0.25, 0.12, 0.35, 0.3, 0.08)
  )
  importance_built = ggplot2::ggplot_build(csdg_plot_data(
    importance_values,
    type = "importance_comparison",
    style = "monochrome"
  ))
  expect_true(is_achromatic(c(
    importance_built$data[[2L]]$colour,
    importance_built$data[[3L]]$colour,
    importance_built$data[[3L]]$fill
  )))
  expect_true(all(importance_built$data[[3L]]$shape == 21L))

  gate_status = data.table::CJ(study = c("SHILD", "PISA"), gate_id = c("G1", "G2"))
  gate_status[, status := c("met", "unresolved", "not_met", "error")]
  gate_status_built = ggplot2::ggplot_build(csdg_plot_data(
    gate_status,
    type = "gate_status",
    style = "monochrome"
  ))
  expect_true(is_achromatic(gate_status_built$data[[1L]]$fill))
  expect_setequal(gate_status_built$data[[2L]]$colour, c("grey12", "white"))

  ice = data.table::data.table(
    feature = "x1",
    x = 1:3,
    q05_prediction = c(0.1, 0.2, 0.3),
    q25_prediction = c(0.2, 0.3, 0.4),
    median_prediction = c(0.3, 0.4, 0.5),
    q75_prediction = c(0.4, 0.5, 0.6),
    q95_prediction = c(0.5, 0.6, 0.7),
    n_curves = 100L
  )
  ice_built = ggplot2::ggplot_build(csdg_plot_data(ice, type = "ice", style = "monochrome"))
  expect_true(is_achromatic(c(ice_built$data[[1L]]$fill, ice_built$data[[2L]]$fill)))

  ale_2d = data.table::data.table(
    feature1 = "x1",
    feature2 = "x2",
    x1_left = c(0, 1),
    x1_right = c(1, 2),
    x2_bottom = c(0, 0),
    x2_top = c(1, 1),
    ale2d = c(-0.2, 0.2)
  )
  ale_2d_built = ggplot2::ggplot_build(csdg_plot_data(ale_2d, type = "ale_2d", style = "monochrome"))
  expect_true(is_achromatic(ale_2d_built$data[[1L]]$fill))
  expect_setequal(ale_2d_built$data[[2L]]$label, c("-", "+"))

  bins = data.table::data.table(
    bin_left = c(0, 0.1, 0.2, 0.3),
    bin_right = c(0.1, 0.2, 0.3, 0.4),
    N = c(40L, 30L, 20L, 10L)
  )
  multiplicity_built = ggplot2::ggplot_build(csdg_plot_data(bins, type = "multiplicity", style = "monochrome"))
  expect_gt(data.table::uniqueN(multiplicity_built$data[[2L]]$linetype), 1L)
  expect_gt(data.table::uniqueN(multiplicity_built$data[[5L]]$shape), 1L)

  decision = data.table::data.table(
    threshold = c(0.1, 0.2, 0.3),
    net_benefit_model = c(0.20, 0.15, 0.10),
    net_benefit_treat_all = c(0.18, 0.10, 0.02),
    net_benefit_treat_none = 0
  )
  decision_built = ggplot2::ggplot_build(csdg_plot_data(
    decision,
    type = "decision_curve",
    style = "monochrome"
  ))
  expect_setequal(decision_built$data[[2L]]$colour, "#111111")
  expect_gt(data.table::uniqueN(decision_built$data[[2L]]$linetype), 1L)
  expect_error(csdg_plot_data(bins, type = "multiplicity", style = "sepia"), "should be one of")
})
