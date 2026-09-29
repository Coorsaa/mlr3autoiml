test_that("the claim record template has the fields of the article", {
  record = csdg_claim_record_template()
  expect_true(data.table::is.data.table(record))
  expect_identical(dim(record), c(15L, 5L))
  expect_named(record, c("field", "section", "enter", "package_field", "entry"))
  expect_true(all(vapply(record, is.character, logical(1L))))
  expect_identical(record$field[1:9],
    c("Claim", "Quantity", "Model", "Procedure", "Data", "Meaning", "Use", "Origin", "Gates"))
  expect_identical(record[section == "scope", tolower(field)], .csdg_scope_elements)
  expect_true(all(record$section %in% c("claim", "scope", "origin", "gates", "evidence", "other_evidence",
    "assessment", "reported_claim", "revision")))
  expect_true(all(record$entry == ""))
  expect_false(anyNA(record))
})

test_that("the claim record template names exported functions and round-trips through CSV", {
  record = csdg_claim_record_template()
  functions = unique(unlist(regmatches(record$package_field, gregexpr("csdg_[a-z_]+", record$package_field))))
  expect_true(all(functions %in% getNamespaceExports("mlr3autoiml")))
  path = withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(record, path)
  expect_equal(data.table::fread(path, colClasses = "character"), record, ignore_attr = TRUE)
})
