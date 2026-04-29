# FILE: tests/testthat/test-claim-helpers.R

# ---- make_claim_card --------------------------------------------------------

test_that("make_claim_card returns only non-NULL fields", {
  card = make_claim_card(
    purpose   = "global_insight",
    semantics = "within_support",
    stakes    = "low",
    claims    = list(global = TRUE, local = FALSE, decision = FALSE)
  )
  expect_type(card, "list")
  expect_setequal(names(card), c("purpose", "semantics", "stakes", "claims"))
  expect_null(card$audience)
  expect_null(card$decision_spec)
})

test_that("make_claim_card includes narrative fields when supplied", {
  card = make_claim_card(
    purpose = "exploratory",
    semantics = "within_support",
    stakes = "low",
    claims = list(global = TRUE),
    audience = "research team",
    target_population = "Adults 18+",
    intended_use = "Hypothesis generation",
    prohibited_interpretations = "No causal claims."
  )
  expect_equal(card$audience, "research team")
  expect_equal(card$target_population, "Adults 18+")
  expect_equal(card$prohibited_interpretations, "No causal claims.")
})

test_that("make_claim_card includes decision_spec when provided", {
  dspec = list(thresholds = c(0.1, 0.2, 0.3), utility = list(tp = 1, fp = 0))
  card = make_claim_card(
    purpose       = "decision_support",
    semantics     = "within_support",
    stakes        = "medium",
    claims        = list(global = TRUE, local = FALSE, decision = TRUE),
    decision_spec = dspec
  )
  expect_equal(card$decision_spec, dspec)
})

test_that("make_claim_card warns when decision=TRUE and no decision_spec", {
  expect_warning(
    make_claim_card(
      purpose   = "decision_support",
      semantics = "within_support",
      stakes    = "medium",
      claims    = list(global = TRUE, decision = TRUE)
    ),
    regexp = "decision_spec"
  )
})

test_that("make_claim_card rejects invalid purpose", {
  expect_error(
    make_claim_card("predict", "within_support", "low", list(global = TRUE)),
    regexp = "purpose"
  )
})

test_that("make_claim_card rejects invalid semantics", {
  expect_error(
    make_claim_card("exploratory", "causal", "low", list(global = TRUE)),
    regexp = "semantics"
  )
})

test_that("make_claim_card rejects invalid stakes", {
  expect_error(
    make_claim_card("exploratory", "within_support", "critical", list(global = TRUE)),
    regexp = "stakes"
  )
})

test_that("make_claim_card rejects non-flag values in claims", {
  expect_error(
    make_claim_card("exploratory", "within_support", "low",
      list(global = "yes")),
    regexp = "logical flag"
  )
})

# ---- make_measurement_card --------------------------------------------------

test_that("make_measurement_card returns only non-NULL fields", {
  card = make_measurement_card("item")
  expect_type(card, "list")
  expect_equal(names(card), "level")
  expect_equal(card$level, "item")
})

test_that("make_measurement_card includes optional fields when supplied", {
  card = make_measurement_card(
    level            = "scale",
    missingness_plan = "Median impute inside folds.",
    reliability      = list(method = "alpha", value = 0.87),
    invariance       = list(status = "partial")
  )
  expect_equal(card$level, "scale")
  expect_equal(card$missingness_plan, "Median impute inside folds.")
  expect_equal(card$reliability$value, 0.87)
  expect_equal(card$invariance$status, "partial")
  expect_null(card$scoring_pipeline)
})

test_that("make_measurement_card rejects invalid level", {
  expect_error(
    make_measurement_card("raw_scores"),
    regexp = "level"
  )
})

test_that("make_measurement_card normalizes legacy measurement-level aliases", {
  card = make_measurement_card("latent")
  expect_equal(card$level, "factor_score")
})

# ---- Gate round-trip integration --------------------------------------------

test_that("make_claim_card output passes Gate0A for low-stakes global claim", {
  gate = mlr3autoiml:::Gate0AClaim$new()
  ctx = list(
    purpose = "global_insight",
    claim = make_claim_card(
      purpose   = "global_insight",
      semantics = "within_support",
      stakes    = "low",
      claims    = list(global = TRUE, local = FALSE, decision = FALSE)
    )
  )
  out = gate$run(ctx)
  expect_true(out$status %in% c("pass", "warn"))
})

test_that("make_claim_card output passes Gate0A for high-stakes deployment", {
  gate = mlr3autoiml:::Gate0AClaim$new()
  ctx = list(
    purpose = "deployment",
    claim = make_claim_card(
      purpose = "deployment",
      semantics = "within_support",
      stakes = "high",
      claims = list(global = TRUE, local = TRUE, decision = TRUE),
      decision_spec = list(thresholds = c(0.2, 0.4),
        utility = list(tp = 1, tn = 0, fp = -1, fn = -2)),
      target_population = "Adults in outpatient setting",
      setting = "Outpatient primary care",
      time_horizon = "12 months",
      transport_boundary = "No transport beyond OECD outpatient cohorts",
      intended_use = "Risk stratification support",
      intended_non_use = "Not for autonomous treatment decisions",
      prohibited_interpretations = "No causal attribution",
      decision_policy_rationale = "Thresholds chosen by stakeholder utility workshop"
    )
  )
  out = gate$run(ctx)
  expect_true(out$status %in% c("pass", "warn"))
  expect_true(out$metrics$has_transport_scope[[1L]])
  expect_true(out$metrics$has_use_boundaries[[1L]])
})

test_that("make_measurement_card output passes Gate0B for medium-stakes claim", {
  gate = mlr3autoiml:::Gate0BMeasurement$new()
  ctx = list(
    task = mlr3::tsk("iris"),
    claim = list(purpose = "global_insight", stakes = "medium"),
    measurement = make_measurement_card(
      level            = "scale",
      missingness_plan = "Median impute.",
      reliability      = list(method = "alpha", value = 0.85)
    )
  )
  out = gate$run(ctx)
  expect_true(out$status %in% c("pass", "warn"))
})

test_that("make_measurement_card + make_claim_card feed AutoIML without error", {
  skip_if_not_installed("rpart")
  task = make_task_iris()
  learner = make_learner_classif()
  auto = mlr3autoiml::AutoIML$new(
    task = task,
    learner = learner,
    resampling = make_resampling_cv(folds = 3L),
    purpose = "global_insight",
    quick_start = TRUE,
    seed = 1L
  )
  auto$ctx$claim = make_claim_card(
    purpose = "global_insight",
    semantics = "within_support",
    stakes = "low",
    claims = list(global = TRUE, local = FALSE, decision = FALSE),
    intended_use = "Testing helper integration."
  )
  auto$ctx$measurement = make_measurement_card(
    level            = "item",
    missingness_plan = "None; iris has no missings."
  )
  expect_no_error(auto$run(verbose = FALSE))
  expect_true(!is.null(auto$result))
})
