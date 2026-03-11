# FILE: tests/testthat/test-iel.R

test_that("iel_from_gates returns claim-scoped IEL list", {
  make_gr = function(id, status, artifacts = NULL) {
    mlr3autoiml::GateResult$new(
      gate_id = id,
      gate_name = id,
      pdr = "P",
      status = status,
      summary = "",
      artifacts = artifacts
    )
  }

  # Minimal claim artifact (Gate 0A) so requested scopes are explicit
  claim_art = list(claim = list(
    claims = list(global = TRUE, local = FALSE, decision = FALSE),
    stakes = "medium",
    purpose = "global_insight"
  ))

  g_base = list(
    G0A = make_gr("G0A", "pass", artifacts = claim_art),
    G0B = make_gr("G0B", "pass"),
    G1  = make_gr("G1", "pass")
  )

  iel1 = mlr3autoiml::iel_from_gates(g_base)
  expect_true(is.list(iel1))
  expect_equal(iel1$requested, "global")
  expect_equal(iel1$global, "IEL-0")
  expect_equal(iel1$overall, "IEL-0")
  expect_true(is.list(iel1$iel_justification))
  expect_true("global" %in% names(iel1$iel_justification))
  expect_true(data.table::is.data.table(iel1$iel_justification$global))
  expect_true(is.character(iel1$iel_rule_source) || is.na(iel1$iel_rule_source))

  g_lvl1 = c(g_base, list(
    G2  = make_gr("G2", "pass"),
    G5  = make_gr("G5", "pass"),
    G7A = make_gr("G7A", "pass")
  ))
  iel1 = mlr3autoiml::iel_from_gates(g_lvl1)
  expect_equal(iel1$global, "IEL-1")
  expect_equal(iel1$overall, "IEL-1")

  g_lvl2 = c(g_lvl1, list(
    G6 = make_gr("G6", "pass")
  ))
  iel2 = mlr3autoiml::iel_from_gates(g_lvl2)
  expect_equal(iel2$global, "IEL-2")
  expect_equal(iel2$overall, "IEL-2")

  g_lvl3 = c(g_lvl2, list(
    G7B = make_gr("G7B", "pass")
  ))
  iel3 = mlr3autoiml::iel_from_gates(g_lvl3)
  expect_equal(iel3$global, "IEL-2")
  expect_equal(iel3$overall, "IEL-2")

  claim_high = list(claim = list(
    claims = list(global = TRUE, local = FALSE, decision = FALSE),
    stakes = "high",
    purpose = "deployment"
  ))
  g_high = g_lvl3
  g_high$G0A = make_gr("G0A", "pass", artifacts = claim_high)
  iel_high = mlr3autoiml::iel_from_gates(g_high)
  expect_equal(iel_high$global, "IEL-3")
  expect_equal(iel_high$overall, "IEL-3")

  g_fail = g_lvl3
  g_fail$G1 = make_gr("G1", "fail")
  iel0 = mlr3autoiml::iel_from_gates(g_fail)
  expect_equal(iel0$overall, "IEL-0")
})

test_that("claim_scope_from_iel expects claim-scoped IEL list", {
  expect_error(mlr3autoiml::claim_scope_from_iel("IEL-1", "global_insight"))

  iel = list(overall = "IEL-1", global = "IEL-1", local = "IEL-0", decision = "IEL-0", requested = "global")
  scope = mlr3autoiml::claim_scope_from_iel(iel, "global_insight")
  expect_true(is.list(scope))
  expect_match(scope$overall, "Overall evidence", ignore.case = TRUE)
  expect_match(scope$global, "global", ignore.case = TRUE)
})

test_that("claim_scope_from_iel returns appropriate scope statements for different IEL levels", {
  # IEL-0 case
  iel0 = list(overall = "IEL-0", global = "IEL-0", local = "IEL-0", decision = "IEL-0", requested = "global")
  scope0 = mlr3autoiml::claim_scope_from_iel(iel0, "exploratory")
  expect_match(scope0$global, "Exploratory only")
  expect_match(scope0$local, "Avoid case-level")
  expect_match(scope0$decision, "Avoid decision")

  # IEL-2 case
  iel2 = list(overall = "IEL-2", global = "IEL-2", local = "IEL-2", decision = "IEL-2", requested = c("global", "local"))
  scope2 = mlr3autoiml::claim_scope_from_iel(iel2, "global_insight")
  expect_match(scope2$global, "publication-grade")
  expect_match(scope2$local, "multiplicity")

  # IEL-3 case
  iel3 = list(overall = "IEL-3", global = "IEL-3", local = "IEL-3", decision = "IEL-3", requested = "decision")
  scope3 = mlr3autoiml::claim_scope_from_iel(iel3, "deployment")
  expect_match(scope3$global, "deployment-grade", ignore.case = TRUE)
  expect_match(scope3$decision, "deployment decision", ignore.case = TRUE)
})
