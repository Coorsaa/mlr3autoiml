test_that("source resolution separates public files from protected and external declarations", {
  root = withr::local_tempdir()
  writeLines("aggregate evidence", file.path(root, "Summary.csv"))
  registry = list(
    list(id = "aggregate", access = "public", path = "Summary.csv", dependencies = "private",
      rationale = "Aggregate summary, not paired individual records."),
    list(id = "private", access = "protected", path = "not_distributed/cases.csv",
      available_aggregate_ids = "aggregate", rationale = "Protected paired cases are not included."),
    list(id = "reference", access = "external", uri = "https://example.org/article",
      rationale = "External reference is declared, not fetched.")
  )
  result = csdg_resolve_sources(registry, root)

  expect_identical(result$status, c("verified_public", "declared_protected", "declared_external"))
  expect_match(result$sha256[[1L]], "^[0-9a-f]{64}$")
  expect_true(all(is.na(result$sha256[2:3])))
  expect_false(file.exists(file.path(root, registry[[2L]]$path)))
  expect_identical(result$available_aggregate_ids[[2L]], "aggregate")
  registry[[1L]]$sha256 = result$sha256[[1L]]
  expect_identical(csdg_resolve_sources(registry, root)$sha256, result$sha256)
  registry[[1L]]$sha256 = strrep("0", 64L)
  expect_error(csdg_resolve_sources(registry, root), "SHA-256 mismatch")
})

test_that("public source paths match exact case on every filesystem", {
  root = withr::local_tempdir()
  dir.create(file.path(root, "Tables"))
  writeLines("evidence", file.path(root, "Tables", "Summary.csv"))
  source = list(id = "table", access = "public", path = "Tables/Summary.csv", rationale = "Registered summary.")

  expect_identical(csdg_resolve_sources(list(source), root)$status, "verified_public")
  for (path in c("tables/Summary.csv", "Tables/summary.csv", "Tables/Missing.csv")) {
    source$path = path
    expect_error(csdg_resolve_sources(list(source), root), "case-sensitive")
  }
  for (path in c("../Summary.csv", "Tables/../Tables/Summary.csv", "/Summary.csv", "~/Summary.csv")) {
    source$path = path
    expect_error(csdg_resolve_sources(list(source), root), "relative|components")
  }
  source$path = "Tables"
  expect_error(csdg_resolve_sources(list(source), root), "not a directory")
})

test_that("public sources cannot escape the root through symbolic links", {
  skip_on_os("windows")
  root = withr::local_tempdir()
  external = withr::local_tempfile()
  writeLines("outside root", external)
  skip_if_not(file.symlink(external, file.path(root, "escaped.csv")))
  source = list(id = "escaped", access = "public", path = "escaped.csv", rationale = "Invalid escape fixture.")

  expect_error(csdg_resolve_sources(list(source), root), "outside the declared root")
})

test_that("source identifiers and graph references must resolve without cycles", {
  root = withr::local_tempdir()
  writeLines("aggregate", file.path(root, "table.csv"))
  registry = lapply(c("first", "second"), function(id) {
    list(id = id, access = "public", path = "table.csv", rationale = "Synthetic graph fixture.")
  })
  duplicate = registry
  duplicate[[2L]]$id = "first"
  expect_error(csdg_resolve_sources(duplicate, root), "unique")
  missing = registry
  missing[[1L]]$dependencies = "unknown"
  expect_error(csdg_resolve_sources(missing, root), "unregistered")
  for (field in c("dependencies", "available_aggregate_ids")) {
    cycle = registry
    cycle[[1L]][[field]] = "second"
    cycle[[2L]][[field]] = "first"
    expect_error(csdg_resolve_sources(cycle, root), "cycle")
    cycle[[1L]][[field]] = "first"
    expect_error(csdg_resolve_sources(cycle, root), "cycle")
  }
})

test_that("protected and external sources require explicit locators and public substitutes", {
  root = withr::local_tempdir()
  protected = list(id = "protected", access = "protected", rationale = "Access restriction.")
  expect_error(csdg_resolve_sources(list(protected), root), "locator")
  external = list(id = "external", access = "external", rationale = "External article.")
  expect_error(csdg_resolve_sources(list(external), root), "uri")
  protected$uri = "urn:protected:diagnostics"
  external$uri = "https://example.org/article"
  protected$available_aggregate_ids = "external"
  expect_error(csdg_resolve_sources(list(protected, external), root), "must be public")
})
