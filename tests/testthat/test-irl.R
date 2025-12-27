# FILE: tests/testthat/test-irl.R

test_that("irl_from_gates maps statuses to IRL levels", {
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
    G1 = make_gr("G1", "pass"),
    G2 = make_gr("G2", "pass"),
    G3 = make_gr("G3", "pass")
  )
  expect_identical(mlr3autoiml::irl_from_gates(g_pass), "IRL-2")

  g_warn = g_pass
  g_warn$G2 = make_gr("G2", "warn")
  expect_identical(mlr3autoiml::irl_from_gates(g_warn), "IRL-1")

  g_skip = g_pass
  g_skip$G3 = make_gr("G3", "skip")
  expect_identical(mlr3autoiml::irl_from_gates(g_skip), "IRL-1")

  g_fail = g_pass
  g_fail$G1 = make_gr("G1", "fail")
  expect_identical(mlr3autoiml::irl_from_gates(g_fail), "IRL-0")

  g_error = g_pass
  g_error$G3 = make_gr("G3", "error")
  expect_identical(mlr3autoiml::irl_from_gates(g_error), "IRL-0")
})
