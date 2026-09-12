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
      "claim_consequence", "rationale", "varied_component", "held_constant", "same_estimand",
      "same_estimand_rationale", "invariance_claimed", "required_property", "observation",
      "relevance_to_proposition", "variation_status", "proposition_linkage"
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
    c("retain_exact_claim", "exact_claim_not_retained", "revise_claim", "unresolved")
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

contract_evidence = function(...) {
  arguments = list(gate_id = "G2", applicable = TRUE, role = "necessary_requirement",
    result_direction = "supports", rationale = "The recorded observation bears on the stated necessary property.")
  do.call(csdg_evidence_record, modifyList(arguments, list(...), keep.null = TRUE))
}

test_that("claim scope is distinct from module applicability and is explicit in results", {
  irrelevant = contract_evidence(applicable = FALSE, result_direction = "not_evaluated")
  in_scope = csdg_adjudicate_claim(list(irrelevant), claim_applicable = TRUE)
  legacy = csdg_adjudicate_claim(list(irrelevant))
  outside = csdg_adjudicate_claim(list(irrelevant), claim_applicable = FALSE,
    applicability_rationale = "The proposition is outside the declared evaluation scope.")
  expect_identical(in_scope$decision, "unresolved")
  expect_true(in_scope$claim_applicable)
  expect_identical(in_scope$applicability_source, "explicit")
  expect_identical(legacy$decision, "unresolved")
  expect_identical(legacy$applicability_source, "legacy_default")
  expect_match(legacy$applicability_rationale, "backward-compatible")
  expect_identical(outside$decision, "not_applicable")
  expect_false(outside$claim_applicable)
  expect_identical(outside$applicability_source, "explicit")
  expect_error(csdg_adjudicate_claim(list(), claim_applicable = FALSE), "applicability_rationale")
  expect_error(csdg_adjudicate_claim(list(contract_evidence()), claim_applicable = FALSE,
    applicability_rationale = "Outside scope."), "out-of-scope")
  expect_error(csdg_adjudicate_claim(list(), claim_applicable = NA), "claim_applicable")
})

test_that("empty, missing, and incomplete evidence cannot produce met", {
  expect_identical(csdg_adjudicate_claim()$decision, "unresolved")
  expect_identical(csdg_adjudicate_claim(list(), claim_applicable = TRUE)$decision, "unresolved")
  expect_identical(csdg_adjudicate_claim()$proposition_linkage, "not_recorded_for_all_evidence")
  expect_identical(csdg_adjudicate_claim(list(), claim_applicable = FALSE,
    applicability_rationale = "Out of scope.")$decision, "not_applicable")
  expect_error(csdg_adjudicate_claim(NULL), "evidence")
  expect_error(csdg_adjudicate_claim(list(list())), "CSDGEvidenceRecord")
  unexpected = contract_evidence()
  unexpected$unexpected_field = "No silently ignored metadata."
  expect_error(csdg_adjudicate_claim(list(unexpected)), "unsupported field")
  for (field in c("role", "availability", "claim_consequence", "criterion")) {
    damaged = contract_evidence()
    damaged[[field]] = NULL
    expect_error(csdg_adjudicate_claim(list(damaged)), "missing required field")
  }
  for (availability in c("incomplete", "unavailable")) {
    record = contract_evidence(availability = availability)
    result = csdg_adjudicate_claim(list(record))
    expect_identical(result$decision, "unresolved")
    expect_identical(result$unresolved_gate_ids, "G2")
  }
  for (direction in c("mixed", "descriptive", "not_evaluated")) {
    record = contract_evidence(result_direction = direction)
    expect_identical(csdg_adjudicate_claim(list(record))$decision, "unresolved")
  }
})

test_that("binding consequences and necessary evidence have coherent precedence", {
  support = contract_evidence(claim_consequence = "retain_exact_claim")
  pending = contract_evidence(gate_id = "G4", claim_consequence = "unresolved")
  challenge = contract_evidence(gate_id = "G1", result_direction = "challenges",
    claim_consequence = "exact_claim_not_retained")
  revision = contract_evidence(gate_id = "G3a", result_direction = "mixed", claim_consequence = "revise_claim")
  expect_identical(csdg_adjudicate_claim(list(support))$decision, "met")
  expect_match(csdg_adjudicate_claim(list(support))$rationale, "conditional on the supplied")
  expect_identical(csdg_adjudicate_claim(list(pending))$decision, "unresolved")
  expect_identical(csdg_adjudicate_claim(list(challenge))$decision, "not_met")
  expect_identical(csdg_adjudicate_claim(list(revision))$decision, "not_met")
  expect_length(csdg_adjudicate_claim(list(revision))$unresolved_gate_ids, 0L)
  for (records in list(list(support, pending), list(pending, support))) {
    result = csdg_adjudicate_claim(records)
    expect_identical(result$decision, "unresolved")
    expect_identical(result$unresolved_gate_ids, "G4")
  }
  for (records in list(list(support, pending, challenge), list(challenge, pending, support))) {
    result = csdg_adjudicate_claim(records)
    expect_identical(result$decision, "not_met")
    expect_identical(result$blocking_gate_ids, "G1")
    expect_identical(result$unresolved_gate_ids, "G4")
  }
  expect_identical(csdg_adjudicate_claim(list(contract_evidence(result_direction = "challenges")))$decision, "not_met")
  expect_error(contract_evidence(result_direction = "challenges", claim_consequence = "unresolved"),
    "cannot record an unresolved")
  expect_error(contract_evidence(applicable = FALSE, result_direction = "not_evaluated",
    claim_consequence = "unresolved"), "nonapplicable module")
})

test_that("unrelated support cannot satisfy or compensate for required evidence", {
  support = contract_evidence(role = "graded_support", claim_consequence = "retain_exact_claim")
  context = contract_evidence(role = "descriptive_context", result_direction = "descriptive")
  irrelevant = contract_evidence(applicable = FALSE, result_direction = "not_evaluated")
  needed = contract_evidence(gate_id = "G1", availability = "unavailable", result_direction = "not_evaluated")
  blocker = contract_evidence(gate_id = "G4", role = "potential_defeater", result_direction = "mixed",
    materiality = "materialized", adjudication_basis = "substantive_adjudication", claim_consequence = "revise_claim")
  expect_identical(csdg_adjudicate_claim(list(support, context, irrelevant))$decision, "unresolved")
  expect_identical(csdg_adjudicate_claim(list(support, context, needed))$decision, "unresolved")
  expect_identical(csdg_adjudicate_claim(list(support, context, blocker))$decision, "not_met")
  expect_identical(csdg_adjudicate_claim(list(contract_evidence(), context, irrelevant))$decision, "met")
  pending = contract_evidence(role = "graded_support", claim_consequence = "unresolved")
  expect_identical(csdg_adjudicate_claim(list(contract_evidence(), pending))$decision, "unresolved")
  expect_error(contract_evidence(role = "descriptive_context", claim_consequence = "unresolved"), "Descriptive context")
})
