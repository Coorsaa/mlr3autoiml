test_that("framework requirements and IEL rules remain internally consistent", {
  req = .autoiml_framework_requirements()
  req_val = .autoiml_validate_framework_requirements(req)
  expect_true(isTRUE(req_val$ok))

  rules = .autoiml_iel_rules()
  rules_val = .autoiml_validate_iel_rules(rules)
  expect_true(isTRUE(rules_val$ok))

  req_ids = vapply(req$requirements, function(r) as.character(r$id), character(1L))
  expect_equal(length(unique(req_ids)), length(req_ids))

  rule_ids = vapply(rules$rules, function(r) as.character(r$id), character(1L))
  expect_equal(length(unique(rule_ids)), length(rule_ids))

  rule_levels = vapply(rules$rules, function(r) as.character(r$level), character(1L))
  expect_true(all(rule_levels %in% c("IEL-0", "IEL-1", "IEL-2", "IEL-3")))

  rule_scopes = vapply(rules$rules, function(r) as.character(r$scope), character(1L))
  expect_true(all(rule_scopes %in% c("global", "local", "decision")))
})


test_that("report_card_extended covers requirement IDs and expected columns", {
  auto = get_auto_iris(quick_start = FALSE)
  rcx = mlr3autoiml::report_card_extended(auto$result)

  expect_true(data.table::is.data.table(rcx))
  expect_true(all(c(
    "requirement_id", "gate", "evidence_type", "severity_if_missing",
    "gate_present", "gate_status", "artifact_keys_ok", "missing_artifact_keys"
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
    "report_card", "report_card_extended", "gate_results", "iel",
    "claim_scope", "traceability_status", "session_info"
  ) %in% names(files)))
  expect_true(all(file.exists(unlist(files, use.names = FALSE))))
})
