test_that("Gate4 emits perturbation and surrogate artifacts for local/decision claims", {
  task = make_task_iris_binary()
  learner = make_learner_classif_rpart()
  resampling = make_resampling_cv(folds = 3L)

  auto = mlr3autoiml::AutoIML$new(
    task = task,
    learner = learner,
    resampling = resampling,
    purpose = "decision_support",
    seed = 5L,
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
      )
    )
  )

  res = auto$run(verbose = FALSE)
  g4 = res$gate_results[["G4"]]
  expect_true(g4$status %in% c("pass", "warn", "fail"))
  expect_true("faithfulness_summary" %in% names(g4$artifacts))
  expect_true("perturbation_design" %in% names(g4$artifacts))
  expect_true("surrogate_spec" %in% names(g4$artifacts))
  expect_true(data.table::is.data.table(g4$artifacts$faithfulness_summary))
  expect_true(data.table::is.data.table(g4$artifacts$perturbation_design))
  expect_true(data.table::is.data.table(g4$artifacts$surrogate_spec))
})


test_that("Gate5 emits stability tier and sanity check artifacts", {
  auto = get_auto_iris_binary(quick_start = FALSE, seed = 42)
  g5 = auto$result$gate_results[["G5"]]

  expect_true(inherits(g5, "GateResult"))
  expect_true(data.table::is.data.table(g5$metrics))
  expect_true(all(c("stability_tier", "sanity_checks", "sanity_pass") %in% names(g5$metrics)))
  expect_true(g5$metrics$stability_tier[[1L]] %in% c("stable", "partially_stable", "unstable", "unknown"))

  expect_true("sanity_check_result" %in% names(g5$artifacts))
  expect_true(data.table::is.data.table(g5$artifacts$sanity_check_result))
})
