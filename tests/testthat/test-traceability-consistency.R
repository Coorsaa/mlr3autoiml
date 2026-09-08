test_that("framework requirements remain internally consistent", {
  req = .autoiml_framework_requirements()
  req_val = .autoiml_validate_framework_requirements(req)
  expect_true(isTRUE(req_val$ok))

  req_ids = vapply(req$requirements, function(r) as.character(r$id), character(1L))
  expect_equal(length(unique(req_ids)), length(req_ids))
})


test_that("report_card_extended covers requirement IDs and expected columns", {
  auto = get_auto_iris(quick_start = FALSE)
  rcx = mlr3autoiml::report_card_extended(auto$result)

  expect_true(data.table::is.data.table(rcx))
  expect_true(all(c(
    "requirement_id", "gate", "evidence_type", "severity_if_missing",
    "applicable", "gate_present", "gate_status", "artifact_keys_ok", "missing_artifact_keys", "evidence_status"
  ) %in% names(rcx)))

  req = .autoiml_framework_requirements()
  req_ids = vapply(req$requirements, function(r) as.character(r$id), character(1L))
  expect_setequal(unique(rcx$requirement_id), req_ids)
})


test_that("export_audit_bundle emits reproducibility artifacts", {
  auto = get_auto_iris(quick_start = FALSE)

  out_dir = file.path(tempdir(), paste0("autoiml_audit_bundle_", as.integer(stats::runif(1L, 1, 1e6))))
  files = mlr3autoiml::export_audit_bundle(auto$result, dir = out_dir, prefix = "audit")

  expect_true(is.list(files))
  expect_true(all(c(
    "report_card", "report_card_extended", "gate_results",
    "traceability_status", "session_info"
  ) %in% names(files)))
  expect_true(all(file.exists(unlist(files, use.names = FALSE))))
})
