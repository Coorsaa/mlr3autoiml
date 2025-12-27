# FILE: tests/testthat/test-gate2-structure.R

test_that("Gate 2 produces expected artifacts and recommendations", {
  auto = get_auto_iris(quick_start = FALSE)
  result = auto$result

  g2 = result$gate_results[["G2"]]
  expect_true(inherits(g2, "GateResult"))

  m = g2$metrics
  expect_true(data.table::is.data.table(m))
  expect_true(all(c("max_abs_cor", "dependence_flag", "recommended_effect_method") %in% names(m)))
  # iris has strong correlation between Petal.Length and Petal.Width
  expect_true(m$max_abs_cor[1] > 0.8)
  expect_true(isTRUE(m$dependence_flag[1]))
  expect_true(m$recommended_effect_method[1] %in% c("ale", "pd"))

  a = g2$artifacts
  expect_true(is.list(a))
  expect_true(all(c("pd_curves", "ice_curves", "ale_curves", "ice_spread", "hstats", "max_cor_pair") %in% names(a)))

  # tables helper should include a non-empty recommendation table if artifacts$recommendation exists
  tabs = mlr3autoiml::gate2_tables(result)
  expect_true(is.list(tabs))
  expect_true("metrics" %in% names(tabs))
})
