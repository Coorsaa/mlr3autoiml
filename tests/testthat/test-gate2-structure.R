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
  expect_true(all(c("pd_curves", "ice_curves", "ale_curves", "ice_spread", "hstats", "max_cor_pair", "pint", "gadget_multi", "gadget_regions", "gadget_splits", "gadget_feature_metrics") %in% names(a)))
  if (!is.null(a$pd_curves) && data.table::is.data.table(a$pd_curves) && nrow(a$pd_curves) > 0L) {
    expect_true("semantics_label" %in% names(a$pd_curves))
  }
  if (!is.null(a$ale_curves) && data.table::is.data.table(a$ale_curves) && nrow(a$ale_curves) > 0L) {
    expect_true("semantics_label" %in% names(a$ale_curves))
  }
  if (!is.null(a$ale2d) && length(a$ale2d) > 0L) {
    ale2d_one = data.table::as.data.table(a$ale2d[[1L]])
    expect_true(all(c("x1_left", "x1_right", "x2_bottom", "x2_top") %in% names(ale2d_one)))
    expect_true(all(ale2d_one$x1_right > ale2d_one$x1_left))
    expect_true(all(ale2d_one$x2_top > ale2d_one$x2_bottom))
  }
  if (!is.null(a$support_check) && data.table::is.data.table(a$support_check) && nrow(a$support_check) > 0L) {
    expect_true(all(c("semantics_label", "support_ratio_threshold", "support_diag_available") %in% names(a$support_check)))
  }

  # tables helper should include a non-empty recommendation table if artifacts$recommendation exists
  tabs = mlr3autoiml::gate2_tables(result)
  expect_true(is.list(tabs))
  expect_true("metrics" %in% names(tabs))
})
