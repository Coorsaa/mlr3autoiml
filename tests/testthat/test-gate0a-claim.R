test_that("Gate0AClaim enforces high-stakes transport and use boundaries", {
  gate = mlr3autoiml:::Gate0AClaim$new()
  ctx = list(
    purpose = "deployment",
    claim = list(
      purpose = "deployment",
      stakes = "high",
      claims = list(global = TRUE, local = TRUE, decision = TRUE),
      semantics = "within_support",
      audience = "technical",
      decision_spec = list(thresholds = c(0.2, 0.4, 0.6), utility = list(tp = 1, tn = 0, fp = -1, fn = -2)),
      actionability = list(mutable_features = c("x1")),
      # missing on purpose:
      # target_population, setting, time_horizon, transport_boundary,
      # intended_use, intended_non_use, prohibited_interpretations,
      # decision_policy_rationale

      # present to avoid causal hard-stop branch:
      causal_assumptions = NULL
    ),
    calibration = list(thresholds = seq(0.01, 0.99, by = 0.01))
  )

  out = gate$run(ctx)
  expect_equal(out$status, "fail")
  miss = out$artifacts$missing_codes
  expect_true("transport_scope" %in% miss)
  expect_true("use_boundaries" %in% miss)
  expect_true("decision_policy_rationale" %in% miss)
})


test_that("Gate0AClaim passes high-stakes with complete scoping metadata", {
  gate = mlr3autoiml:::Gate0AClaim$new()
  ctx = list(
    purpose = "deployment",
    claim = list(
      purpose = "deployment",
      stakes = "high",
      claims = list(global = TRUE, local = TRUE, decision = TRUE),
      semantics = "within_support",
      audience = "technical",
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
      ),
      actionability = list(mutable_features = c("x1"))
    ),
    calibration = list(thresholds = seq(0.01, 0.99, by = 0.01))
  )

  out = gate$run(ctx)
  expect_true(out$status %in% c("pass", "warn"))
  expect_true(out$metrics$has_transport_scope[[1L]])
  expect_true(out$metrics$has_use_boundaries[[1L]])
  expect_true(out$metrics$has_decision_policy_rationale[[1L]])
})
