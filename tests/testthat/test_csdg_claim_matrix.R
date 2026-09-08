test_that("claim matrices retain explicit judgments without an overall score", {
  matrix = csdg_claim_matrix(
    claim = c("A scoped performance claim", "A transport claim"),
    evidence_needed = c("Repeated out-of-fold estimates", "External validation"),
    conclusion = c("Supported for the analytic sample", "Not established outside the sample"),
    status = c("supported_scoped", "not_established"),
    claim_id = c("performance", "transport")
  )

  expect_s3_class(matrix, "CSDGClaimMatrix")
  expect_s3_class(matrix, "data.table")
  expect_identical(matrix$claim_order, 1:2)
  expect_identical(matrix$status_label, c("Supported (scoped)", "Not established"))
  expect_identical(
    names(matrix),
    c(
      "claim_order", "claim_id", "claim_version", "parent_claim_id", "revision_relation", "status",
      "status_label", "claim", "evidence_needed", "conclusion", "derivation_scope"
    )
  )
  expect_false(any(grepl("score|total|count", names(matrix), ignore.case = TRUE)))
})

test_that("claim matrices reject ambiguous schemas", {
  expect_error(
    csdg_claim_matrix(
      claim = c("Claim one", "Claim two"),
      evidence_needed = "Evidence",
      conclusion = c("Conclusion one", "Conclusion two"),
      status = c("unresolved", "not_evaluated")
    ),
    "equal lengths"
  )
  expect_error(
    csdg_claim_matrix(
      claim = "Claim",
      evidence_needed = "Evidence",
      conclusion = "Conclusion",
      status = "passed"
    ),
    "status"
  )
  expect_error(
    csdg_claim_matrix(
      claim = c("Claim one", "Claim two"),
      evidence_needed = c("Evidence one", "Evidence two"),
      conclusion = c("Conclusion one", "Conclusion two"),
      status = c("not_established", "unresolved"),
      claim_id = c("duplicate", "duplicate")
    ),
    "duplicated|unique"
  )
})

test_that("claim matrix plots use explicit status labels and scale to the supplied rows", {
  statuses = c(
    "supported_scoped", "unresolved", "not_supported", "not_established", "not_evaluated"
  )
  matrix = csdg_claim_matrix(
    claim = paste("Claim", seq_along(statuses)),
    evidence_needed = paste("Evidence", seq_along(statuses)),
    conclusion = paste("Conclusion", seq_along(statuses)),
    status = statuses
  )
  matrix$status_label[[4L]] = "Supported"
  plot = csdg_plot_claim_matrix(matrix)
  built = ggplot2::ggplot_build(plot)

  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(built$data[[1L]]), length(statuses))
  expect_equal(nrow(built$data[[2L]]), length(statuses))
  expect_equal(nrow(built$data[[3L]]), length(statuses))
  expect_setequal(
    gsub("\n", " ", built$data[[3L]]$label),
    c("Supported (scoped)", "Unresolved", "Not supported", "Not established", "Not evaluated")
  )
  expect_identical(plot$labels$title, "Claim-by-evidence matrix")
  expect_match(plot$labels$subtitle, "not combined into an overall score")
  expect_identical(plot$theme$text$family, "Arial")

  monochrome_plot = csdg_plot_claim_matrix(matrix, style = "monochrome")
  monochrome_built = ggplot2::ggplot_build(monochrome_plot)
  fill_channels = grDevices::col2rgb(unique(monochrome_built$data[[2L]]$fill))
  expect_true(all(fill_channels[1L, ] == fill_channels[2L, ] & fill_channels[2L, ] == fill_channels[3L, ]))
  expect_setequal(monochrome_built$data[[3L]]$colour, c("#1A1A1A", "#FFFFFF"))
  expect_true(all(monochrome_built$data[[2L]]$colour == "grey55"))
  expect_error(csdg_plot_claim_matrix(matrix, style = "sepia"), "should be one of")

  matrix$claim_order[[2L]] = matrix$claim_order[[1L]]
  expect_error(csdg_plot_claim_matrix(matrix), "duplicated")
})
