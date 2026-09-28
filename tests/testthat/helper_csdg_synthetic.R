make_classif_fixture = function(n = 120L, seed = 1L) {
  set.seed(seed)
  dat = data.frame(
    cluster = rep(seq_len(n / 3L), each = 3L),
    x1 = rnorm(n),
    x2 = rnorm(n),
    cat = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )
  eta = 0.7 * dat$x1 - 0.4 * dat$x2 + 0.2 * (dat$cat == "b")
  dat$y = factor(rbinom(n, 1, plogis(eta)), levels = c(0, 1))
  task = mlr3::as_task_classif(
    dat[, setdiff(names(dat), "cluster")],
    target = "y",
    positive = "1"
  )
  list(
    data = dat,
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    cluster = dat$cluster
  )
}

make_cards = function(task) {
  list(
    claim = csdg_claim(
      id = "test",
      statement = "Held-out predictions and global importance are adequate in the analytic sample.",
      claim_type = c(
        "predictive_performance", "calibration", "global_explanation"
      ),
      target = "binary outcome",
      unit = "row",
      population = "synthetic population",
      analytic_distribution = "synthetic analytic sample",
      model_scope = "fitted_model",
      setting_scope = "analytic_sample",
      scientific_use = "Methodological evaluation of the fitted prediction pipeline",
      explanation_design = "Held-out marginal PFI for a global selected-model description"
    ),
    measurement = csdg_measurement(
      outcome = task$target_names,
      predictors = task$feature_names,
      data_source = "synthetic fixture",
      sample_definition = "all rows",
      missingness = "none",
      preprocessing = "fold-contained learner",
      verification = list(
        status = "author_reviewed",
        artifact = "test fixture"
      )
    ),
    explanation = csdg_explanation(
      method_ids = "pfi",
      scope = "global",
      target = "positive-class probability"
    )
  )
}
