test_that("evidence roles and fields remain orthogonal", {
  record = csdg_evidence_record(
    gate_id = "G4",
    applicable = TRUE,
    role = "potential_defeater",
    availability = "complete",
    result_direction = "challenges",
    criterion = list(value = 0.02, direction = "maximum"),
    criterion_source = "Prospective use protocol",
    criterion_rationale = "Maximum error linked to the declared individual-level use.",
    materiality = "materialized",
    adjudication_basis = "prespecified_claim_specific_criterion",
    claim_consequence = "exact_claim_not_retained",
    rationale = "Observed error exceeded the prespecified use-linked maximum."
  )

  expect_s3_class(record, "CSDGEvidenceRecord")
  expect_named(
    record,
    c(
      "gate_id", "applicable", "role", "availability", "result_direction", "criterion",
      "criterion_source", "criterion_rationale", "materiality", "adjudication_basis",
      "claim_consequence", "rationale"
    )
  )
  expect_setequal(mlr3autoiml:::.csdg_evidence_roles, c(
    "necessary_requirement", "potential_defeater", "graded_support", "descriptive_context"
  ))

  schema_path = system.file("schema", "csdg-evidence-record.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(schema_path, simplifyVector = FALSE)
  expect_identical(unlist(schema$properties$role$enum, use.names = FALSE), mlr3autoiml:::.csdg_evidence_roles)
  expect_true(all(
    c("criterion", "criterion_source", "criterion_rationale") %in% unlist(schema$required, use.names = FALSE)
  ))
  expect_identical(schema$allOf[[1L]]$then$properties$criterion_source$type, "string")
  expect_identical(schema$allOf[[1L]]$then$properties$criterion_rationale$type, "string")

  condition_const = function(rule, field) {
    node = rule[["if"]]$properties[[field]]
    if (is.null(node) || is.null(node$const)) NA_character_ else as.character(node$const)
  }
  consequences = vapply(schema$allOf, condition_const, character(1L), field = "claim_consequence")
  expect_setequal(
    consequences[!is.na(consequences)],
    c("retain_exact_claim", "exact_claim_not_retained", "revise_claim")
  )
  descriptive = which(vapply(schema$allOf, condition_const, character(1L), field = "role") ==
    "descriptive_context")
  expect_length(descriptive, 1L)
  expect_identical(schema$allOf[[descriptive]]$then$properties$claim_consequence$const, "none")

  not_materialized = which(vapply(schema$allOf, function(rule) {
    identical(rule[["if"]]$properties$role$const, "potential_defeater") &&
      identical(rule[["if"]]$properties$materiality$const, "not_materialized")
  }, logical(1L)))
  expect_length(not_materialized, 1L)
  expect_setequal(
    unlist(schema$allOf[[not_materialized]]$then$properties$claim_consequence$enum, use.names = FALSE),
    c("none", "unresolved")
  )
})

test_that("conditional non-compensation blocks the exact claim", {
  necessary = csdg_evidence_record(
    gate_id = "G1",
    applicable = TRUE,
    role = "necessary_requirement",
    result_direction = "challenges",
    claim_consequence = "exact_claim_not_retained",
    rationale = "The claim-specific predictive requirement was not met."
  )
  favorable = csdg_evidence_record(
    gate_id = "G5",
    applicable = TRUE,
    role = "graded_support",
    result_direction = "supports",
    claim_consequence = "retain_exact_claim",
    rationale = "The descriptive ranking was stable."
  )

  result = csdg_adjudicate_claim(list(necessary, favorable))

  expect_identical(result$decision, "not_met")
  expect_identical(result$blocking_gate_ids, "G1")
  expect_match(result$rationale, "does not compensate")
})

test_that("potential defeaters require transparent materialization", {
  expect_error(
    csdg_evidence_record(
      gate_id = "G2",
      applicable = TRUE,
      role = "potential_defeater",
      result_direction = "challenges",
      materiality = "materialized",
      rationale = "Observed support was limited."
    ),
    "adjudication_basis"
  )
  expect_error(
    csdg_evidence_record(
      gate_id = "G2",
      applicable = TRUE,
      role = "potential_defeater",
      result_direction = "challenges",
      criterion = list(value = 0.10, direction = "maximum"),
      materiality = "materialized",
      adjudication_basis = "prespecified_claim_specific_criterion",
      rationale = "Observed support was limited."
    ),
    "criterion_source"
  )
  expect_error(
    csdg_evidence_record(
      gate_id = "G2",
      applicable = TRUE,
      role = "potential_defeater",
      availability = "incomplete",
      result_direction = "challenges",
      materiality = "materialized",
      adjudication_basis = "substantive_adjudication",
      rationale = "The incomplete evidence cannot materialize a defeater."
    ),
    "complete evidence"
  )
  expect_error(
    csdg_evidence_record(
      gate_id = "G2",
      applicable = TRUE,
      role = "potential_defeater",
      result_direction = "supports",
      materiality = "materialized",
      adjudication_basis = "substantive_adjudication",
      rationale = "Supporting evidence cannot be a materialized defeater."
    ),
    "challenge"
  )
})

test_that("a materialized defeater cannot be compensated by favorable evidence", {
  defeater = csdg_evidence_record(
    gate_id = "G2",
    applicable = TRUE,
    role = "potential_defeater",
    result_direction = "challenges",
    materiality = "materialized",
    adjudication_basis = "substantive_adjudication",
    claim_consequence = "exact_claim_not_retained",
    rationale = "The declared perturbation leaves the support required by the exact claim."
  )
  favorable = csdg_evidence_record(
    gate_id = "G5",
    applicable = TRUE,
    role = "graded_support",
    result_direction = "supports",
    claim_consequence = "retain_exact_claim",
    rationale = "The ranking was stable across perturbation repetitions."
  )

  result = csdg_adjudicate_claim(list(defeater, favorable))

  expect_identical(result$decision, "not_met")
  expect_identical(result$blocking_gate_ids, "G2")
  expect_match(result$rationale, "does not compensate")
})

test_that("descriptive context cannot fail a claim", {
  expect_error(
    csdg_evidence_record(
      gate_id = "G7a",
      applicable = TRUE,
      role = "descriptive_context",
      result_direction = "challenges",
      claim_consequence = "exact_claim_not_retained",
      rationale = "Descriptive subgroup variation was observed."
    ),
    "Descriptive context"
  )
})

test_that("evidence records reject contradictory consequences", {
  expect_error(
    csdg_evidence_record(
      gate_id = "G7a",
      applicable = TRUE,
      role = "descriptive_context",
      result_direction = "descriptive",
      claim_consequence = "retain_exact_claim",
      rationale = "Descriptive evidence cannot retain the exact claim."
    ),
    "Descriptive context"
  )
  expect_error(
    csdg_evidence_record(
      gate_id = "G2",
      applicable = TRUE,
      role = "potential_defeater",
      result_direction = "challenges",
      materiality = "not_materialized",
      claim_consequence = "exact_claim_not_retained",
      rationale = "The defeater was not materialized."
    ),
    "nonmaterialized"
  )
  expect_error(
    csdg_evidence_record(
      gate_id = "G1",
      applicable = TRUE,
      role = "necessary_requirement",
      result_direction = "supports",
      claim_consequence = "exact_claim_not_retained",
      rationale = "Supporting evidence cannot reject the exact claim."
    ),
    "challenging necessary requirement"
  )
})

test_that("new CSDG objects expose no score or IEL fields", {
  record = csdg_evidence_record(
    gate_id = "G5",
    applicable = TRUE,
    role = "graded_support",
    result_direction = "descriptive",
    rationale = "Permutation variation is reported descriptively."
  )
  adjudication = csdg_adjudicate_claim(list(record))
  names_all = c(names(record), names(adjudication))

  expect_false(any(grepl("IEL|score|grade|readiness", names_all, ignore.case = TRUE)))
  expect_false(adjudication$decision %in% c("partial", "partly_supported", "partly supported"))
})
