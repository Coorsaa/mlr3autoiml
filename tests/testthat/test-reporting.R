# FILE: tests/testthat/test-reporting.R

test_that("report_card works for AutoIML and AutoIMLResult", {
  auto = get_auto_iris(quick_start = FALSE)
  result = auto$result

  rc1 = mlr3autoiml::report_card(auto)
  rc2 = mlr3autoiml::report_card(result)

  expect_true(data.table::is.data.table(rc1))
  expect_true(data.table::is.data.table(rc2))
  expect_equal(rc1, rc2)
})

test_that("AutoIML$tables and AutoIML$overview work", {
  auto = get_auto_iris(quick_start = FALSE)

  tabs_g2 = auto$tables("g2")
  expect_true(is.list(tabs_g2))
  expect_true(all(c("metrics", "max_cor_pair", "ice_spread_top", "hstats_top") %in% names(tabs_g2)))

  out = auto$overview()
  expect_true(inherits(out, "AutoIMLResult"))
})
