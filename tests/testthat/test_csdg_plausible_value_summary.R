test_that("plausible-value summaries aggregate values before ranks", {
  data = data.frame(
    feature_group = rep(c("a", "b"), each = 3L),
    plausible_value = rep(paste0("pv", 1:3), 2L),
    estimate = c(3, 1, 2, 1, 0, 2)
  )
  result = csdg_plausible_value_summary(data, by = "feature_group")

  expect_equal(result$summary[feature_group == "a", mean_estimate], 2)
  expect_equal(result$summary[feature_group == "b", mean_estimate], 1)
  expect_equal(result$summary[feature_group == "a", rank_after_value_scale_aggregation], 1)
  expect_match(result$estimand$uncertainty, "not, by themselves, a sampling interval")
})

test_that("plausible-value summaries require a complete aligned grid", {
  data = data.frame(
    group = c("a", "a", "b"),
    pv = c("pv1", "pv2", "pv1"),
    value = c(1, 2, 3)
  )
  expect_error(
    csdg_plausible_value_summary(data, "pv", "value", by = "group"),
    "same complete plausible-value set"
  )
})
