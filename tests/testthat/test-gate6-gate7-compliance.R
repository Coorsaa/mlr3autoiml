test_that("Gate6 returns provenance and fails high-stakes without transport evidence", {
  auto = get_auto_iris_binary(quick_start = FALSE, seed = 11L)
  gate = mlr3autoiml:::Gate6Multiplicity$new()

  ctx = list(
    task = auto$task,
    pred = auto$ctx$pred,
    learner = auto$learner,
    resampling = auto$resampling,
    primary_measure_id = auto$ctx$primary_measure_id,
    seed = 11L,
    claim = list(purpose = "deployment", stakes = "high"),
    multiplicity = list(
      enabled = TRUE,
      rashomon_rule = "1se",
      max_alt_learners = 3L,
      importance_n = 80L,
      importance_max_features = 5L,
      require_transport_for_high_stakes = TRUE
    ),
    alt_learners = list()
  )

  out = NULL
  expect_warning({
    out = gate$run(ctx)
  }, "deprecated")
  expect_true(inherits(out, "GateResult"))
  expect_equal(out$status, "fail")
  expect_true("rashomon_provenance" %in% names(out$artifacts))
  expect_true("shift_assessment" %in% names(out$artifacts))
  expect_true(data.table::is.data.table(out$artifacts$rashomon_provenance))
  expect_true(is.null(out$artifacts$shift_assessment))
  expect_true("explanation_multiplicity" %in% names(out$artifacts))
  expect_equal(out$artifacts$rashomon_provenance$rashomon_rule_requested[[1L]], "1se")
  expect_equal(out$artifacts$rashomon_provenance$rashomon_rule[[1L]], "descriptive_sd")
  if (!is.null(out$artifacts$alt_learner_performance)) {
    performance = out$artifacts$alt_learner_performance
    expect_true(all(c("q10", "q90", "minimum", "maximum", "uncertainty_label") %in% names(performance)))
    expect_false(any(c("se", "ci_low", "ci_high") %in% names(performance)))
    expect_match(performance$uncertainty_label[[1L]], "not independent-sample inference")
  }
})


test_that("Gate6 computes grouped classification transport without probability type errors", {
  gate = mlr3autoiml:::Gate6Multiplicity$new()

  dat = iris[iris$Species != "setosa", ]
  dat$Species = droplevels(dat$Species)
  dat$group_var = factor(ifelse(dat$Sepal.Length > median(dat$Sepal.Length), "high", "low"))

  task = mlr3::as_task_classif(Species ~ ., data = dat, id = "iris_binary_transport")
  task$set_col_roles("group_var", add_to = "group")

  learner = make_learner_classif_rpart()
  fitted = learner$clone(deep = TRUE)
  fitted$train(task)
  pred = fitted$predict(task)

  ctx = list(
    task = task,
    pred = pred,
    learner = learner,
    resampling = make_resampling_cv(folds = 3L),
    primary_measure_id = "classif.auc",
    seed = 13L,
    claim = list(purpose = "decision_support", stakes = "medium"),
    multiplicity = list(
      enabled = FALSE,
      rashomon_rule = "descriptive_sd",
      max_alt_learners = 2L,
      importance_n = 40L,
      importance_max_features = 4L,
      group_col = "group_var",
      transport_mode = "group_performance",
      require_transport_for_high_stakes = TRUE
    ),
    alt_learners = list(
      mlr3::lrn("classif.featureless", predict_type = "prob")
    )
  )

  out = gate$run(ctx)
  expect_true(inherits(out, "GateResult"))
  expect_false(identical(out$status, "error"))
  expect_true("shift_assessment" %in% names(out$artifacts))
  expect_true(!is.null(out$artifacts$shift_assessment))
  expect_true(data.table::is.data.table(out$artifacts$group_performance))
  expect_gt(nrow(out$artifacts$group_performance), 0L)
})


test_that("Gate7A fails high-stakes subgroup claims without invariance evidence", {
  gate = mlr3autoiml:::Gate7aSubgroups$new()

  dat = iris
  dat$group_var = factor(ifelse(dat$Sepal.Length > median(dat$Sepal.Length), "high", "low"))
  task = mlr3::as_task_classif(Species ~ ., data = dat, id = "iris_grouped")

  ctx = list(
    task = task,
    sensitive_features = "group_var",
    claim = list(
      purpose = "deployment",
      stakes = "high",
      claims = list(global = TRUE, local = TRUE, decision = TRUE)
    ),
    measurement = list(level = "scale", reliability = list(alpha = 0.9))
  )

  out = gate$run(ctx)
  expect_equal(out$status, "fail")
  expect_match(out$summary, "measurement comparability evidence", ignore.case = TRUE)
})


test_that("Gate7A does not require invariance for item-level subgroup audits", {
  gate = mlr3autoiml:::Gate7aSubgroups$new()

  dat = iris
  dat$group_var = factor(ifelse(dat$Sepal.Length > median(dat$Sepal.Length), "high", "low"))
  task = mlr3::as_task_classif(Species ~ ., data = dat, id = "iris_item_grouped")
  learner = make_learner_classif_rpart()
  learner$train(task)
  pred = learner$predict(task)

  ctx = list(
    task = task,
    pred = pred,
    final_model = learner,
    sensitive_features = "group_var",
    claim = list(
      purpose = "decision_support",
      stakes = "high",
      claims = list(global = TRUE, local = TRUE, decision = TRUE),
      decision_spec = list(
        thresholds = c(0.2, 0.4, 0.6),
        utility = list(tp = 1, tn = 0, fp = -1, fn = -2)
      )
    ),
    measurement = list(level = "item")
  )

  out = gate$run(ctx)
  expect_true(out$status %in% c("pass", "warn"))
})


test_that("Gate7A emits subgroup explanation stability artifacts", {
  gate = mlr3autoiml:::Gate7aSubgroups$new()

  dat = iris[iris$Species != "setosa", ]
  dat$Species = droplevels(dat$Species)
  dat$group_var = factor(ifelse(dat$Sepal.Length > median(dat$Sepal.Length), "high", "low"))

  task = mlr3::as_task_classif(Species ~ ., data = dat, id = "iris_binary_grouped")
  learner = make_learner_classif_rpart()
  learner$train(task)
  pred = learner$predict(task)

  ctx = list(
    task = task,
    pred = pred,
    final_model = learner,
    sensitive_features = "group_var",
    claim = list(
      purpose = "decision_support",
      stakes = "high",
      claims = list(global = TRUE, local = TRUE, decision = TRUE),
      decision_spec = list(
        thresholds = c(0.2, 0.4, 0.6),
        utility = list(tp = 1, tn = 0, fp = -1, fn = -2)
      )
    ),
    measurement = list(
      level = "scale",
      reliability = list(alpha = 0.9),
      invariance = list(multigroup_cfa = "supported")
    )
  )

  out = gate$run(ctx)
  expect_equal(out$status, "pass")
  expect_true("subgroup_explanation_stability" %in% names(out$artifacts))
  expect_true("subgroup_explanation_stability_summary" %in% names(out$artifacts))
  expect_true(data.table::is.data.table(out$artifacts$subgroup_explanation_stability_summary))
})


test_that("Gate7A does not turn descriptive subgroup ECE into a universal adequacy rule", {
  gate = mlr3autoiml:::Gate7aSubgroups$new()
  data = data.frame(
    outcome = factor(rep(c("negative", "positive"), 10L)),
    group_var = factor(rep(c("a", "b"), each = 10L)),
    x = seq_len(20L)
  )
  task = mlr3::as_task_classif(outcome ~ ., data = data, id = "descriptive_subgroup_calibration")
  positive_probability = rep(seq(0.80, 0.98, length.out = 10L), 2L)
  probability = cbind(negative = 1 - positive_probability, positive = positive_probability)
  prediction = mlr3::PredictionClassif$new(
    task = task,
    row_ids = task$row_ids,
    truth = data$outcome,
    response = factor(rep("positive", 20L), levels = levels(data$outcome)),
    prob = probability
  )
  ctx = list(
    task = task,
    pred = prediction,
    sensitive_features = "group_var",
    claim = list(
      purpose = "global_insight",
      stakes = "medium",
      claims = list(global = TRUE, local = FALSE, decision = FALSE)
    ),
    measurement = list(level = "item")
  )

  out = gate$run(ctx)
  expect_gt(max(out$artifacts$subgroup$ece), 0.10)
  expect_equal(out$status, "pass")
  expect_match(out$summary, "descriptive")
})
