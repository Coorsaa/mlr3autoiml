test_that("traceability scaffold assets exist", {
  p1 = system.file("extdata", "framework_requirements.yaml", package = "mlr3autoiml")
  p2 = system.file("extdata", "iel_rules.yaml", package = "mlr3autoiml")

  expect_true(nzchar(p1) && file.exists(p1))
  expect_true(nzchar(p2) && file.exists(p2))
})


test_that("traceability status checks run", {
  st = .autoiml_traceability_status()

  expect_true(is.list(st))
  expect_true(all(c("framework_loaded", "iel_rules_loaded", "framework_validation", "iel_rules_validation", "ok") %in% names(st)))

  expect_true(isTRUE(st$framework_loaded))
  expect_true(isTRUE(st$iel_rules_loaded))
  expect_true(isTRUE(st$framework_validation$ok))
  expect_true(isTRUE(st$iel_rules_validation$ok))
  expect_true(isTRUE(st$ok))
})
