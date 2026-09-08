test_that("Gate1Validity emits uncertainty and leakage checklist artifacts", {
  g1 = mlr3autoiml:::Gate1Validity$new()

  task = make_task_iris_binary()
  learner = make_learner_classif_rpart()
  resampling = make_resampling_cv(folds = 3L)

  ctx = list(
    task = task,
    learner = learner,
    resampling = resampling,
    seed = 1L,
    validation = list(split_policy = "cv")
  )

  out = g1$run(ctx)
  expect_true(out$status %in% c("pass", "warn"))
  expect_true("uncertainty" %in% names(out$artifacts))
  expect_true("leakage_checklist" %in% names(out$artifacts))
  expect_true(data.table::is.data.table(out$artifacts$uncertainty))
  expect_true(data.table::is.data.table(out$artifacts$leakage_checklist))
  expect_true(all(c("median", "q10", "q90", "minimum", "maximum", "uncertainty_label") %in%
    names(out$artifacts$uncertainty)))
  expect_false(any(c("se", "ci_low", "ci_high") %in% names(out$artifacts$uncertainty)))
  expect_match(out$artifacts$uncertainty$uncertainty_label[[1L]], "not independent-sample inference")
})


test_that("Gate1Validity summarizes plausible values on identical resampling splits", {
  g1 = mlr3autoiml:::Gate1Validity$new()

  task = make_task_mtcars_regr()
  learner = make_learner_regr_rpart()
  resampling = make_resampling_cv(folds = 3L)

  dt_pv2 = mtcars
  dt_pv2$mpg = dt_pv2$mpg + 1
  task_pv2 = mlr3::as_task_regr(mpg ~ ., data = dt_pv2, id = "mtcars_regr_pv2")

  ctx = list(
    task = task,
    learner = learner,
    resampling = resampling,
    seed = 1L,
    validation = list(split_policy = "cv"),
    plausible_values = list(pv_tasks = list(task_pv2))
  )

  out = g1$run(ctx)
  pv_summary = out$artifacts$pv_summary
  pv_per_value = out$artifacts$pv_per_value

  expect_true(out$status %in% c("pass", "warn"))
  expect_true(data.table::is.data.table(pv_summary))
  expect_true(data.table::is.data.table(pv_per_value))
  expect_setequal(pv_summary$measure_id, c("regr.rmse", "regr.rsq"))
  expect_true(all(pv_summary$n_pv == 2L))
  expect_true(all(c("mean_across_pv", "sd_across_pv", "uncertainty_label") %in% names(pv_summary)))
  expect_false(any(c("pooled_se", "total_var", "df", "ci_low", "ci_high") %in% names(pv_summary)))
  expect_match(pv_summary$uncertainty_label[[1L]], "descriptive variation")
})


test_that("Gate3Calibration emits decision_range artifact", {
  task = make_task_iris_binary()
  learner = make_learner_classif_rpart()
  resampling = make_resampling_cv(folds = 3L)

  auto = mlr3autoiml::AutoIML$new(
    task = task,
    learner = learner,
    resampling = resampling,
    purpose = "decision_support",
    seed = 2L,
    config = list(
      claim = list(
        claims = list(global = TRUE, local = TRUE, decision = TRUE),
        semantics = "within_support",
        target_population = "Adults in outpatient setting",
        setting = "Outpatient primary care",
        time_horizon = "12 months",
        transport_boundary = "No transport beyond OECD outpatient cohorts",
        intended_use = "Risk stratification support",
        intended_non_use = "Not for autonomous treatment decisions",
        prohibited_interpretations = "No causal attribution",
        decision_policy_rationale = "Thresholds chosen by stakeholder utility workshop",
        decision_spec = list(
          thresholds = c(0.2, 0.4, 0.6),
          utility = list(tp = 1, tn = 0, fp = -1, fn = -2)
        )
      ),
      measurement = list(
        level = "scale",
        reliability = list(alpha = 0.90),
        invariance = list(multigroup_cfa = "configural/metric supported"),
        missingness_plan = "MAR with chained equations imputation nested in folds",
        scoring_pipeline = "reverse coding + scale construction inside resampling"
      ),
      calibration = list(bins = 10L)
    )
  )

  res = auto$run(verbose = FALSE)
  out3 = res$gate_results[["G3"]]
  expect_equal(out3$status, "warn")
  expect_false(out3$metrics$calibration_criteria_declared[[1L]])
  expect_true(any(grepl("no universal cutoff", out3$messages, fixed = TRUE)))
  expect_true("decision_range" %in% names(out3$artifacts))
  dr = out3$artifacts$decision_range
  expect_true(data.table::is.data.table(dr))
  expect_equal(dr$decision_claim[[1L]], TRUE)
  expect_equal(dr$n_thresholds[[1L]], 3L)

  declared_ctx = as.list.environment(res$extras$ctx, all.names = TRUE)
  declared_ctx$calibration = list(
    bins = 10L,
    maximum_ece = 1,
    calibration_slope_range = c(0, 100),
    maximum_abs_intercept = 100
  )
  declared = mlr3autoiml:::Gate3Calibration$new()$run(declared_ctx)
  expect_equal(declared$status, "pass")
  expect_true(declared$metrics$calibration_criteria_declared[[1L]])
})
