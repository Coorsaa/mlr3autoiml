# FILE: tests/testthat/test-iel.R

make_test_gate_result = function(id, status, artifacts = NULL) {
  mlr3autoiml::GateResult$new(
    gate_id = id,
    gate_name = id,
    pdr = "P",
    status = status,
    summary = "",
    artifacts = artifacts
  )
}

test_that("iel_from_gates returns claim-scoped IELs aligned with paper logic", {
  claim_art = list(claim = list(
    claims = list(global = TRUE, local = FALSE, decision = FALSE),
    stakes = "medium",
    purpose = "global_insight",
    audience = "technical"
  ))

  g_base = list(
    G0A = make_test_gate_result("G0A", "pass", artifacts = claim_art),
    G0B = make_test_gate_result("G0B", "pass"),
    G1  = make_test_gate_result("G1", "pass")
  )

  iel0 = mlr3autoiml::iel_from_gates(g_base)
  expect_true(is.list(iel0))
  expect_equal(iel0$requested, "global")
  expect_equal(iel0$global, "IEL-0")
  expect_equal(iel0$overall, "IEL-0")
  expect_true(is.list(iel0$iel_justification))
  expect_true("global" %in% names(iel0$iel_justification))
  expect_true(data.table::is.data.table(iel0$iel_justification$global))
  expect_true(is.character(iel0$iel_rule_source) || is.na(iel0$iel_rule_source))

  g_lvl1 = c(g_base, list(
    G2 = make_test_gate_result("G2", "pass"),
    G5 = make_test_gate_result("G5", "pass")
  ))
  iel1 = mlr3autoiml::iel_from_gates(g_lvl1)
  expect_equal(iel1$global, "IEL-1")
  expect_equal(iel1$overall, "IEL-1")

  g_lvl2 = g_lvl1
  g_lvl2$G2 = make_test_gate_result("G2", "pass", artifacts = list(
    recommendation = "ALE with ICE under within-support semantics",
    ale_curves = data.table::data.table(feature = "x", x = 1, ale = 0)
  ))
  g_lvl2$G5 = make_test_gate_result("G5", "pass", artifacts = list(
    perm_importance = data.table::data.table(feature = "x", importance = 0.1)
  ))
  iel2 = mlr3autoiml::iel_from_gates(g_lvl2)
  expect_equal(iel2$global, "IEL-2")
  expect_equal(iel2$overall, "IEL-2")

  g_lvl3 = g_lvl2
  g_lvl3$G6 = make_test_gate_result("G6", "pass", artifacts = list(
    rashomon_set = data.table::data.table(learner_id = "lr", mean = 0.1),
    shift_assessment = data.table::data.table(group = "a", metric = 0.1)
  ))
  iel3 = mlr3autoiml::iel_from_gates(g_lvl3)
  expect_equal(iel3$global, "IEL-3")
  expect_equal(iel3$overall, "IEL-3")
})

test_that("decision IELs do not require local-faithfulness or human factors unless scoped", {
  claim_art = list(claim = list(
    claims = list(global = TRUE, local = FALSE, decision = TRUE),
    stakes = "medium",
    purpose = "decision_support",
    audience = "technical"
  ))

  gates = list(
    G0A = make_test_gate_result("G0A", "pass", artifacts = claim_art),
    G0B = make_test_gate_result("G0B", "pass"),
    G1 = make_test_gate_result("G1", "pass"),
    G2 = make_test_gate_result("G2", "pass", artifacts = list(
      recommendation = "ALE with ICE under within-support semantics",
      ale_curves = data.table::data.table(feature = "x", x = 1, ale = 0)
    )),
    G3 = make_test_gate_result("G3", "pass", artifacts = list(
      reliability = data.table::data.table(mean_pred = 0.2, frac_pos = 0.2),
      dca = data.table::data.table(threshold = 0.2, net_benefit = 0.05),
      utility_curve = data.table::data.table(threshold = 0.2, utility = 0.05),
      utility_spec = list(threshold_range = c(0.1, 0.3))
    )),
    G5 = make_test_gate_result("G5", "pass", artifacts = list(
      perm_importance = data.table::data.table(feature = "x", importance = 0.1)
    )),
    G7A = make_test_gate_result("G7A", "pass", artifacts = list(
      subgroup = data.table::data.table(group = "all", auc = 0.75)
    ))
  )

  iel = mlr3autoiml::iel_from_gates(gates)
  expect_equal(iel$decision, "IEL-2")
  expect_equal(iel$local, "IEL-0")
})

test_that("user-facing high-stakes decision IEL-3 requires human-factors evidence", {
  claim_user = list(claim = list(
    claims = list(global = TRUE, local = FALSE, decision = TRUE),
    stakes = "high",
    purpose = "deployment",
    audience = "clinician"
  ))

  g_base = list(
    G0A = make_test_gate_result("G0A", "pass", artifacts = claim_user),
    G0B = make_test_gate_result("G0B", "pass"),
    G1 = make_test_gate_result("G1", "pass"),
    G2 = make_test_gate_result("G2", "pass", artifacts = list(
      recommendation = "ALE with ICE under within-support semantics",
      ale_curves = data.table::data.table(feature = "x", x = 1, ale = 0)
    )),
    G3 = make_test_gate_result("G3", "pass", artifacts = list(
      reliability = data.table::data.table(mean_pred = 0.2, frac_pos = 0.2),
      dca = data.table::data.table(threshold = 0.2, net_benefit = 0.05),
      utility_curve = data.table::data.table(threshold = 0.2, utility = 0.05),
      utility_spec = list(threshold_range = c(0.1, 0.3))
    )),
    G5 = make_test_gate_result("G5", "pass", artifacts = list(
      perm_importance = data.table::data.table(feature = "x", importance = 0.1)
    )),
    G6 = make_test_gate_result("G6", "pass", artifacts = list(
      rashomon_set = data.table::data.table(learner_id = "lr", mean = 0.1),
      shift_assessment = data.table::data.table(group = "a", metric = 0.1)
    )),
    G7A = make_test_gate_result("G7A", "pass", artifacts = list(
      subgroup = data.table::data.table(group = "all", auc = 0.75)
    ))
  )

  iel_no_hf = mlr3autoiml::iel_from_gates(g_base)
  expect_equal(iel_no_hf$decision, "IEL-2")
  expect_true(any(grepl("user_facing_condition_not_met|missing_gates:G7B|status_mismatch:G7B", iel_no_hf$iel_justification$decision$reason)))

  g_base$G7B = make_test_gate_result("G7B", "pass", artifacts = list(
    human_factors_evidence = list(task_study = "completed")
  ))
  iel_hf = mlr3autoiml::iel_from_gates(g_base)
  expect_equal(iel_hf$decision, "IEL-3")
})

test_that("iel_from_gates enforces artifact evidence for upgraded levels", {
  claim_art = list(claim = list(
    claims = list(global = TRUE, local = FALSE, decision = FALSE),
    stakes = "medium",
    purpose = "global_insight",
    audience = "technical"
  ))

  gates = list(
    G0A = make_test_gate_result("G0A", "pass", artifacts = claim_art),
    G0B = make_test_gate_result("G0B", "pass"),
    G1 = make_test_gate_result("G1", "pass"),
    G2 = make_test_gate_result("G2", "pass"),
    G5 = make_test_gate_result("G5", "pass")
  )

  iel = mlr3autoiml::iel_from_gates(gates)
  expect_equal(iel$global, "IEL-1")
  expect_true(any(grepl("missing_artifact_keys", iel$iel_justification$global$reason, fixed = TRUE)))
})

test_that("claim_scope_from_iel expects claim-scoped IEL list", {
  expect_error(mlr3autoiml::claim_scope_from_iel("IEL-1", "global_insight"))

  iel = list(overall = "IEL-1", global = "IEL-1", local = "IEL-0", decision = "IEL-0", requested = "global")
  scope = mlr3autoiml::claim_scope_from_iel(iel, "global_insight")
  expect_true(is.list(scope))
  expect_match(scope$overall, "Overall evidence", ignore.case = TRUE)
  expect_match(scope$global, "global", ignore.case = TRUE)
})

test_that("claim_scope_from_iel returns paper-aligned scope statements", {
  iel0 = list(overall = "IEL-0", global = "IEL-0", local = "IEL-0", decision = "IEL-0", requested = "global")
  scope0 = mlr3autoiml::claim_scope_from_iel(iel0, "exploratory")
  expect_match(scope0$global, "Exploratory only")
  expect_match(scope0$local, "Avoid case-level")
  expect_match(scope0$decision, "Avoid decision")

  iel2 = list(overall = "IEL-2", global = "IEL-2", local = "IEL-2", decision = "IEL-2", requested = c("global", "local"))
  scope2 = mlr3autoiml::claim_scope_from_iel(iel2, "global_insight")
  expect_match(scope2$global, "Controlled global/regional reporting")
  expect_match(scope2$local, "Controlled local/regional support")

  iel3 = list(overall = "IEL-3", global = "IEL-3", local = "IEL-3", decision = "IEL-3", requested = "decision")
  scope3 = mlr3autoiml::claim_scope_from_iel(iel3, "deployment")
  expect_match(scope3$global, "Robust global insight", ignore.case = TRUE)
  expect_match(scope3$decision, "Applied decision support", ignore.case = TRUE)
})
