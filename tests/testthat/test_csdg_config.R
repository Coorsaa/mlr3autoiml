

test_that("csdg_config rejects unknown top-level fields", {
  expect_error(
    csdg_config(legacy_option = TRUE),
    "Unknown top-level CSDG configuration field"
  )
})

test_that("csdg_config validates integer and shareable export defaults", {
  expect_error(csdg_config(seed = 1.5), "integer")
  expect_error(csdg_config(resampling = list(folds = 2.5)), "integer")

  config = csdg_config()
  expect_false(config$export$include_models)
  expect_false(config$export$include_predictions)
})

test_that("csdg_config validates claim-relevant criteria", {
  expect_error(
    csdg_config(subgroup = list(metric = "auc")),
    "configured together"
  )
  expect_error(
    csdg_config(calibration = list(calibration_slope_range = c(1, 0))),
    "sorted"
  )
  expect_error(
    csdg_config(stability = list(typo = 1L)),
    "Unknown `stability` configuration"
  )
  expect_error(
    csdg_config(faithfulness = list(kernel_width = 0)),
    "not >=",
    fixed = TRUE
  )
})

test_that("shipped templates match verification and privacy defaults", {
  protocol_path = system.file("templates", "analysis_protocol.yml", package = "mlr3autoiml")
  review_path = system.file("templates", "preprocessing_review.md", package = "mlr3autoiml")
  protocol = yaml::read_yaml(protocol_path)
  review = readLines(review_path, warn = FALSE)

  expect_false(protocol$outputs$include_predictions)
  expect_false(protocol$outputs$include_models)
  expect_equal(protocol$measurement$verification$status, "not_checked")
  expect_true(any(grepl("`not_checked`", review, fixed = TRUE)))
  expect_true(any(grepl("until the checks below have been completed", review, fixed = TRUE)))
})
