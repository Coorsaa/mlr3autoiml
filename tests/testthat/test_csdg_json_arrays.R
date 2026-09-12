test_that("new typed metadata remain arrays in JSON exports, including singleton vectors", {
  provenance = list(
    origin = "retrospective_exploratory", date = "2026-09-10", time_basis = "Dated analysis record.",
    selection_basis = "Selected after evidence inspection.", evidence_ids = "E1"
  )
  claim = csdg_claim(provenance = provenance)
  record = csdg_evidence_record(
    "G5", TRUE, "descriptive_context", rationale = "Different reference distributions answer different questions.",
    varied_component = "reference_distribution", held_constant = "fitted_model", same_estimand = FALSE,
    same_estimand_rationale = "The reference distribution defines the estimand."
  )
  path = tempfile(fileext = ".json")
  on.exit(unlink(path))
  .write_json(list(claim = .card_to_list(claim), evidence = unclass(record)), path)
  restored = jsonlite::read_json(path, simplifyVector = FALSE)

  expect_identical(restored$claim$provenance$evidence_ids, list("E1"))
  expect_identical(restored$evidence$varied_component, list("reference_distribution"))
  expect_identical(restored$evidence$held_constant, list("fitted_model"))
  expect_identical(restored$evidence$same_estimand, FALSE)
  expect_null(restored$evidence$invariance_claimed)
  expect_identical(restored$claim$statement, claim$statement)
  expect_identical(claim$provenance, provenance)
  claim$provenance$evidence_ids = character()
  .write_json(.card_to_list(claim), path)
  expect_identical(jsonlite::read_json(path)$provenance$evidence_ids, list())
})

test_that("the claim-card schema admits exactly its required context coordinates", {
  path = system.file("schema", "csdg-card.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(path, simplifyVector = FALSE)
  coordinates = schema[["$defs"]]$claim$properties$coordinates
  expect_setequal(names(coordinates$properties), unlist(coordinates$required))
  expect_false(coordinates$additionalProperties)
  expect_true(all(vapply(coordinates$properties, function(rule) {
    all(c("string", "array", "null") %in% unlist(rule$type))
  }, logical(1L))))
})

test_that("adjudication gate identifiers round trip as arrays and scope remains explicit", {
  record = csdg_evidence_record("G2", TRUE, "necessary_requirement", result_direction = "supports",
    claim_consequence = "unresolved", rationale = "A necessary property is supported but the claim remains unresolved.")
  result = csdg_adjudicate_claim(list(record), claim_applicable = TRUE)
  path = tempfile(fileext = ".json")
  on.exit(unlink(path))
  .write_json(list(evidence = unclass(record), adjudication = unclass(result)), path)
  restored = jsonlite::read_json(path, simplifyVector = FALSE)
  expect_identical(restored$adjudication$unresolved_gate_ids, list("G2"))
  expect_identical(restored$adjudication$blocking_gate_ids, list())
  expect_identical(restored$adjudication$claim_applicable, TRUE)
  expect_identical(restored$adjudication$applicability_source, "explicit")
  arguments = restored$evidence[intersect(names(restored$evidence), names(formals(csdg_evidence_record)))]
  recreated = do.call(csdg_evidence_record, arguments)
  expect_identical(recreated, record)
  expect_identical(csdg_adjudicate_claim(list(recreated), claim_applicable = TRUE), result)
})
