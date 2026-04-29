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

  out = gate$run(ctx)
  expect_true(inherits(out, "GateResult"))
  expect_equal(out$status, "fail")
  expect_true("rashomon_provenance" %in% names(out$artifacts))
  expect_true("shift_assessment" %in% names(out$artifacts))
  expect_true(data.table::is.data.table(out$artifacts$rashomon_provenance))
  expect_true(is.null(out$artifacts$shift_assessment))
  expect_true("explanation_multiplicity" %in% names(out$artifacts))
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
      rashomon_rule = "1se",
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
  expect_true(out$status %in% c("pass", "warn"))
  expect_true("subgroup_explanation_stability" %in% names(out$artifacts))
  expect_true("subgroup_explanation_stability_summary" %in% names(out$artifacts))
  expect_true(data.table::is.data.table(out$artifacts$subgroup_explanation_stability_summary))
})
