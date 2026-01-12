# FILE: tests/testthat/test-irl.R

test_that("irl_from_gates returns claim-scoped IRL list", {
  make_gr = function(id, status) {
    mlr3autoiml::GateResult$new(
      gate_id = id,
      gate_name = id,
      pdr = "P",
      status = status,
      summary = ""
    )
  }

  g_pass = list(
    G0A = make_gr("G0A", "pass"),
    G0B = make_gr("G0B", "pass"),
    G1 = make_gr("G1", "pass"),
    G2 = make_gr("G2", "pass"),
    G5 = make_gr("G5", "pass"),
    G7A = make_gr("G7A", "pass")
  )

  irl = mlr3autoiml::irl_from_gates(g_pass)
  expect_true(is.list(irl))
  expect_equal(irl$requested, "global")
  expect_equal(irl$global, "IRL-1")
  expect_equal(irl$local, "IRL-0")
  expect_equal(irl$decision, "IRL-0")
  expect_equal(irl$overall, "IRL-1")

  g_warn = g_pass
  g_warn$G2 = make_gr("G2", "warn")
  irl2 = mlr3autoiml::irl_from_gates(g_warn)
  expect_equal(irl2$overall, "IRL-1")

  g_skip = g_pass
  g_skip$G7A = make_gr("G7A", "skip")
  irl3 = mlr3autoiml::irl_from_gates(g_skip)
  expect_equal(irl3$overall, "IRL-0")

  g_fail = g_pass
  g_fail$G1 = make_gr("G1", "fail")
  irl4 = mlr3autoiml::irl_from_gates(g_fail)
  expect_equal(irl4$overall, "IRL-0")

  g_error = g_pass
  g_error$G5 = make_gr("G5", "error")
  irl5 = mlr3autoiml::irl_from_gates(g_error)
  expect_equal(irl5$overall, "IRL-0")
})

test_that("claim_scope_from_irl expects claim-scoped IRL", {
  expect_error(mlr3autoiml::claim_scope_from_irl("IRL-1", "global_insight"))

  irl = list(overall = "IRL-1", global = "IRL-1", local = "IRL-0", decision = "IRL-0", requested = "global")
  scope = mlr3autoiml::claim_scope_from_irl(irl, "global_insight")
  expect_true(is.list(scope))
  expect_match(scope$overall, "Overall readiness")
  expect_match(scope$global, "Global")
})
