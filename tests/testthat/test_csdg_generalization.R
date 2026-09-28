test_that("directional transport thresholds are not conflated", {
  scores = data.table::data.table(
    setting = c("a", "b"),
    auc = c(.72, .69),
    rmse = c(1.1, 1.3)
  )
  high = csdg_transport_status(
    scores, "auc", direction = "maximize",
    minimum_transport_score = .68,
    criterion_source = "Prospective analysis protocol",
    criterion_rationale = "Minimum discrimination required for the declared comparison."
  )
  low = csdg_transport_status(
    scores, "rmse", direction = "minimize",
    maximum_transport_score = 1.2,
    criterion_source = "Prospective analysis protocol",
    criterion_rationale = "Maximum prediction error required for the declared comparison."
  )
  expect_equal(high$status, "supported")
  expect_equal(high$legacy_status, "met")
  expect_equal(low$status, "contradicted")
})

test_that("all-missing transport scores are incomplete", {
  out = csdg_transport_status(
    data.table::data.table(auc = c(NA_real_, NaN)),
    "auc",
    direction = "maximize",
    minimum_transport_score = 0.7,
    criterion_source = "Prospective analysis protocol",
    criterion_rationale = "Minimum discrimination required for the declared comparison."
  )
  expect_equal(out$status, "open")
  expect_true(is.na(out$passed))
  expect_match(out$reason, "No finite")
})

test_that("transport thresholds without provenance do not adjudicate", {
  out = csdg_transport_status(
    data.table::data.table(auc = c(.72, .69)),
    "auc",
    direction = "maximize",
    minimum_transport_score = .68
  )

  expect_identical(out$status, "open")
  expect_false(out$criterion_complete)
  expect_match(out$reason, "source or rationale")
})

test_that("Rashomon agreement honors a requested all-feature top-k", {
  rashomon = structure(
    list(candidates = data.table::data.table(
      learner_name = c("a", "b"),
      accepted = TRUE
    )),
    class = c("CSDGRashomon", "list")
  )
  summary = data.table::data.table(
    feature_group = c("x1", "x2"),
    rank = c(1, 2),
    mean_importance = c(2, 1)
  )
  out = csdg_rashomon_agreement(
    rashomon,
    pfi = list(a = list(summary = summary), b = list(summary = summary)),
    top_k = 2L
  )
  expect_equal(out$pairwise_top_k$top_k, 2L)
  expect_equal(out$pairwise_top_k$jaccard, 1)
})

test_that("Rashomon candidates share one split definition", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  learners = list(
    shallow = mlr3::lrn(
      "classif.rpart", predict_type = "prob",
      cp = 0.05
    ),
    deep = mlr3::lrn(
      "classif.rpart", predict_type = "prob",
      cp = 0.001
    )
  )
  rs = mlr3::rsmp("cv", folds = 3L)
  rs$instantiate(fx$task)
  out = csdg_rashomon(
    fx$task, learners, rs,
    primary_measure = "classif.logloss",
    tolerance_relative = .25,
    tolerance_source = "Prospective analysis protocol",
    tolerance_rationale = "Defines the near-equivalent candidate set for this test.",
    seed = 10L
  )
  expect_s3_class(out, "CSDGRashomon")
  expect_equal(nrow(out$candidates), 2L)
  expect_true(any(out$candidates$accepted))
  expect_equal(
    out$resamples[[1L]]$test_sets,
    out$resamples[[2L]]$test_sets
  )
})

test_that("Rashomon tolerance can be anchored to a declared focal learner", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  learners = list(
    focal = mlr3::lrn("classif.rpart", predict_type = "prob", cp = 0.05),
    alternative = mlr3::lrn("classif.rpart", predict_type = "prob", cp = 0.001)
  )
  rs = mlr3::rsmp("cv", folds = 3L)
  rs$instantiate(fx$task)
  out = csdg_rashomon(
    fx$task,
    learners,
    rs,
    primary_measure = "classif.logloss",
    tolerance_relative = 0.10,
    tolerance_source = "Prospective analysis protocol",
    tolerance_rationale = "Defines the focal-relative candidate set for this test.",
    reference_learner = "focal",
    seed = 11L
  )
  focal_score = out$candidates[learner_name == "focal", mean_score]
  expect_equal(out$acceptance_reference_learner, "focal")
  expect_equal(out$acceptance_reference_score, focal_score)
  expect_equal(out$acceptance_limit, focal_score * 1.10)
  expect_true(out$candidates[learner_name == "focal", accepted])
  expect_error(
    csdg_rashomon(
      fx$task,
      learners,
      rs,
      primary_measure = "classif.logloss",
      reference_learner = "unknown",
      seed = 11L
    ),
    "reference_learner"
  )
})

test_that("near-equivalence tolerance grids remain focal-relative and direction-aware", {
  candidates = data.table::data.table(
    learner_name = c("better", "focal", "worse"),
    mean_score = c(0.09, 0.10, 0.12),
    direction = "minimize"
  )
  out = csdg_near_equivalence_sensitivity(
    candidates,
    tolerance_relative = c(0.05, 0.20),
    tolerance_source = "Prospective sensitivity grid",
    tolerance_rationale = "Compares two declared near-equivalence boundaries.",
    reference_learner = "focal",
    scenario_id = c("narrow", "wide")
  )
  expect_equal(out[scenario_id == "narrow", acceptance_limit], rep(0.105, 3L))
  expect_equal(out[scenario_id == "wide", acceptance_limit], rep(0.12, 3L))
  expect_true(out[learner_name == "better", all(fraction_of_tolerance_consumed == 0)])
  expect_false(out[scenario_id == "narrow" & learner_name == "worse", accepted])
  expect_true(out[scenario_id == "wide" & learner_name == "worse", accepted])

  maximizing = data.table::data.table(
    learner_name = c("focal", "alternative"),
    mean_score = c(0.80, 0.78)
  )
  max_out = csdg_near_equivalence_sensitivity(
    maximizing,
    tolerance_absolute = 0.03,
    tolerance_source = "Prospective sensitivity grid",
    tolerance_rationale = "Defines the absolute maximizing-metric boundary.",
    reference_learner = "focal",
    direction = "maximize"
  )
  expect_true(max_out[learner_name == "alternative", accepted])
  expect_equal(max_out[learner_name == "alternative", fraction_of_tolerance_consumed], 2 / 3)
})

test_that("Rashomon comparisons are descriptive when no tolerance is supplied", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 60L)
  learners = list(
    shallow = mlr3::lrn("classif.rpart", predict_type = "prob", cp = 0.05),
    deep = mlr3::lrn("classif.rpart", predict_type = "prob", cp = 0.001)
  )
  rs = mlr3::rsmp("cv", folds = 2L)
  rs$instantiate(fx$task)

  out = csdg_rashomon(fx$task, learners, rs, primary_measure = "classif.logloss", seed = 12L)

  expect_true(all(is.na(out$candidates$accepted)))
  expect_match(out$acceptance_basis, "descriptive")
  expect_error(csdg_rashomon_agreement(out, list()), "no accepted set")
})
