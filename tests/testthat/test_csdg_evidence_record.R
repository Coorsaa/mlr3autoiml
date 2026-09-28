capture_deprecations = function(expr) {
  messages = character()
  value = withCallingHandlers(expr, mlr3autoiml_deprecated = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, messages = messages)
}

property = function(gate_id = "G2", status = "supported", ...) {
  csdg_evidence_record(gate_id, TRUE, "required_property", status = status,
    rationale = "The recorded observation bears on the stated required property.", ...)
}

test_that("evidence records store role, status, and observation fields", {
  record = property("G6a", "contradicted",
    criterion = list(value = "strict ordering in both models", direction = "qualitative"),
    criterion_source = "Translated from the claim",
    criterion_rationale = "The claim asserts the ordering in each model.",
    required_property = "Item 1 has the larger marginal PFI in A and in B.",
    observation = "2 versus 0 in A, 0 versus 2 in B.",
    relevance_to_proposition = "Only the model differs, and the claim covers both models.")

  expect_s3_class(record, "CSDGEvidenceRecord")
  expect_named(
    record,
    c(
      "gate_id", "applicable", "role", "availability", "result_direction", "criterion",
      "criterion_source", "criterion_rationale", "materiality", "adjudication_basis",
      "claim_consequence", "rationale", "varied_component", "held_constant", "same_estimand",
      "same_estimand_rationale", "invariance_claimed", "required_property", "observation",
      "relevance_to_proposition", "status", "legacy_role", "variation_status", "proposition_linkage"
    )
  )
  expect_identical(record$status, "contradicted")
  expect_identical(record$result_direction, "challenges")
  expect_identical(record$availability, "complete")
  expect_null(record$legacy_role)
  expect_identical(record$proposition_linkage, "documented_not_verified")
  expect_identical(mlr3autoiml:::.csdg_evidence_roles, c(
    "required_property", "established_counterevidence", "unresolved_threat", "context"
  ))

  schema_path = system.file("schema", "csdg-evidence-record.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(schema_path, simplifyVector = FALSE)
  roles = unlist(schema$properties$role$enum, use.names = FALSE)
  expect_true(all(mlr3autoiml:::.csdg_evidence_roles %in% roles))
  expect_identical(unlist(schema$properties$status$enum[1:3], use.names = FALSE), c("supported", "contradicted", "open"))
  expect_true("CD" %in% unlist(schema$properties$gate_id$enum, use.names = FALSE))
  expect_true(all(
    c("criterion", "criterion_source", "criterion_rationale") %in% unlist(schema$required, use.names = FALSE)
  ))
  expect_identical(schema$allOf[[1L]]$then$properties$criterion_source$type, "string")
  expect_identical(schema$allOf[[1L]]$then$properties$criterion_rationale$type, "string")
})

test_that("status defaults and agreement with the observation fields", {
  expect_identical(property(status = "open")$result_direction, "not_evaluated")
  expect_identical(property(status = "supported")$result_direction, "supports")
  derived = csdg_evidence_record("G2", TRUE, "required_property", result_direction = "supports",
    rationale = "Complete supporting evidence.")
  expect_identical(derived$status, "supported")
  expect_identical(csdg_evidence_record("G2", TRUE, "required_property", result_direction = "challenges",
    rationale = "Complete challenging evidence.")$status, "contradicted")
  expect_identical(csdg_evidence_record("G2", TRUE, "required_property", availability = "incomplete",
    result_direction = "supports", rationale = "Incomplete evidence.")$status, "open")
  expect_error(property(status = "supported", availability = "incomplete"), "supported property")
  expect_error(property(status = "supported", result_direction = "challenges"), "supported property")
  expect_error(property(status = "contradicted", result_direction = "supports"), "contradicted property")
  expect_error(csdg_evidence_record("G2", TRUE, "context", status = "supported", rationale = "Context."),
    "Only a required property")
  expect_error(csdg_evidence_record("G2", FALSE, "required_property", status = "supported",
    rationale = "Not applicable."), "no property status")
  expect_error(csdg_evidence_record("G9", TRUE, "required_property", rationale = "Unknown gate."), "gate_id")
  expect_identical(property("CD")$gate_id, "CD")
})

test_that("a contradicted property is not offset by favorable evidence", {
  result = csdg_adjudicate_claim(list(property("G1", "contradicted"), property("G5")), claim_applicable = TRUE)
  expect_identical(result$assessment, "not_met")
  # `decision` is a deprecated alias of the assessment; the decision itself is one of `decision_options`.
  expect_identical(result$decision, result$assessment)
  expect_identical(names(result)[[1L]], "assessment")
  expect_identical(result$contradicted_gate_ids, "G1")
  expect_identical(result$blocking_gate_ids, "G1")
  expect_identical(result$decision_options, c("revise", "withhold"))
  expect_match(result$rationale, "never offset")
})

test_that("established counterevidence contradicts the property it bears on", {
  counterevidence = csdg_evidence_record("G2", TRUE, "established_counterevidence",
    rationale = "A verified coding error changes the reported values.",
    required_property = "The procedure computes the named quantity.",
    observation = "Two items were swapped before the computation.",
    relevance_to_proposition = "The swap changes which item each value refers to.")
  expect_null(counterevidence$status)
  expect_identical(counterevidence$result_direction, "challenges")
  result = csdg_adjudicate_claim(list(property("G0a"), property("G2"), counterevidence), claim_applicable = TRUE)
  expect_identical(result$assessment, "not_met")
  expect_identical(result$counterevidence_gate_ids, "G2")
  expect_identical(result$properties[gate_id == "G2", source], "counterevidence")
  alone = csdg_adjudicate_claim(list(counterevidence), claim_applicable = TRUE)
  expect_identical(alone$assessment, "not_met")
  expect_error(csdg_evidence_record("G2", TRUE, "established_counterevidence",
    rationale = "No chain."), "requires required_property")
  expect_error(csdg_evidence_record("G2", TRUE, "established_counterevidence", availability = "incomplete",
    rationale = "Incomplete.", required_property = "p", observation = "o", relevance_to_proposition = "r"),
    "complete evidence")
})

test_that("unresolved threats leave the threatened property open unless it is contradicted", {
  threat = csdg_evidence_record("G1", TRUE, "unresolved_threat",
    rationale = "The optimism was not quantified; a nested analysis would resolve it.",
    required_property = "Held-out performance is not inflated by predictor selection.",
    observation = "Predictors were selected on the same data outside the cross-validation.")
  expect_identical(threat$availability, "incomplete")
  result = csdg_adjudicate_claim(list(property("G1"), threat), claim_applicable = TRUE)
  expect_identical(result$assessment, "unresolved")
  expect_identical(result$open_gate_ids, "G1")
  expect_identical(result$threat_gate_ids, "G1")
  expect_identical(result$properties[gate_id == "G1", source], "threat")
  contradicted = csdg_adjudicate_claim(list(property("G1", "contradicted"), threat), claim_applicable = TRUE)
  expect_identical(contradicted$assessment, "not_met")
  expect_identical(contradicted$properties[gate_id == "G1", status], "contradicted")

  claim = csdg_claim(statement = "A model description.", claim_type = "global_explanation",
    quantity = "marginal PFI", model = "fitted_model")
  plan = csdg_gate_plan(claim, csdg_measurement(), csdg_explanation(method_ids = "pfi"))
  records = lapply(plan$gate_id[plan$required], property)
  expect_warning(
    with_plan <- csdg_adjudicate_claim(c(records, list(threat)), claim_applicable = TRUE, plan = plan),
    "not required"
  )
  expect_identical(with_plan$assessment, "met")
  expect_identical(with_plan$context_gate_ids, "G1")
  expect_warning(
    without_plan <- csdg_adjudicate_claim(list(property("G2"), threat), claim_applicable = TRUE),
    "added as an open required property"
  )
  expect_identical(without_plan$assessment, "unresolved")
  expect_error(csdg_evidence_record("G1", TRUE, "unresolved_threat", rationale = "No property."),
    "threatened property")
})

test_that("context never changes the assessment", {
  context = csdg_evidence_record("G7a", TRUE, "context", result_direction = "challenges",
    rationale = "Descriptive subgroup variation was observed.")
  expect_identical(csdg_adjudicate_claim(list(property(), context), claim_applicable = TRUE)$assessment, "met")
  expect_error(suppressWarnings(csdg_evidence_record("G7a", TRUE, "context", claim_consequence = "unresolved",
    rationale = "Context.")), "Context never changes the assessment")
  expect_identical(csdg_adjudicate_claim(list(context), claim_applicable = TRUE)$assessment, "unresolved")
})

test_that("the plan adds required gates without evidence and the causal design", {
  claim = csdg_claim(statement = "Both models rely more on item 1.", claim_type = "global_explanation",
    quantity = "marginal PFI", model = "several_models")
  plan = csdg_gate_plan(claim, csdg_measurement(), csdg_explanation(method_ids = "pfi"))
  result = csdg_adjudicate_claim(lapply(c("G0a", "G0b", "G2", "G5"), property), claim_applicable = TRUE, plan = plan)
  expect_identical(result$assessment, "unresolved")
  expect_identical(result$open_gate_ids, "G6a")
  expect_identical(result$properties[gate_id == "G6a", source], "plan")

  causal = csdg_claim(statement = "Improving item 1 would increase the outcome.", claim_type = "global_explanation",
    quantity = "marginal PFI", meaning = "causal_claim")
  causal_plan = csdg_gate_plan(causal, csdg_measurement(), csdg_explanation(method_ids = "pfi"))
  expect_true(attr(causal_plan, "causal_design_required"))
  gates = causal_plan$gate_id[causal_plan$required]
  without = csdg_adjudicate_claim(lapply(gates, property), claim_applicable = TRUE, plan = causal_plan)
  expect_identical(without$assessment, "unresolved")
  expect_identical(without$open_gate_ids, "CD")
  with_design = csdg_adjudicate_claim(lapply(c(gates, "CD"), property), claim_applicable = TRUE, plan = causal_plan)
  expect_identical(with_design$assessment, "met")
})

test_that("new CSDG objects expose no score or IEL fields", {
  adjudication = csdg_adjudicate_claim(list(property()))
  names_all = c(names(property()), names(adjudication))
  expect_false(any(grepl("IEL|score|grade|readiness", names_all, ignore.case = TRUE)))
  expect_false(adjudication$assessment %in% c("partial", "partly_supported", "partly supported"))
})

test_that("claim scope is distinct from gate applicability and is explicit in results", {
  irrelevant = csdg_evidence_record("G2", FALSE, "context", rationale = "The gate does not apply to the claim.")
  in_scope = csdg_adjudicate_claim(list(irrelevant), claim_applicable = TRUE)
  legacy = csdg_adjudicate_claim(list(irrelevant))
  outside = csdg_adjudicate_claim(list(irrelevant), claim_applicable = FALSE,
    applicability_rationale = "The claim is outside the evaluation.")
  expect_identical(in_scope$assessment, "unresolved")
  expect_true(in_scope$claim_applicable)
  expect_identical(in_scope$applicability_source, "explicit")
  expect_identical(legacy$assessment, "unresolved")
  expect_identical(legacy$applicability_source, "legacy_default")
  expect_match(legacy$applicability_rationale, "backward-compatible")
  expect_identical(outside$assessment, "not_applicable")
  expect_length(outside$decision_options, 0L)
  expect_false(outside$claim_applicable)
  expect_error(csdg_adjudicate_claim(list(), claim_applicable = FALSE), "applicability_rationale")
  expect_error(csdg_adjudicate_claim(list(property()), claim_applicable = FALSE,
    applicability_rationale = "Outside scope."), "out-of-scope")
  expect_error(csdg_adjudicate_claim(list(), claim_applicable = NA), "claim_applicable")
})

test_that("empty, missing, and incomplete evidence cannot produce met", {
  expect_identical(csdg_adjudicate_claim()$assessment, "unresolved")
  expect_match(csdg_adjudicate_claim()$rationale, "No required property")
  expect_identical(csdg_adjudicate_claim()$proposition_linkage, "not_recorded_for_all_evidence")
  expect_error(csdg_adjudicate_claim(NULL), "evidence")
  expect_error(csdg_adjudicate_claim(list(list())), "CSDGEvidenceRecord")
  unexpected = property()
  unexpected$unexpected_field = "No silently ignored metadata."
  expect_error(csdg_adjudicate_claim(list(unexpected)), "unsupported field")
  for (field in c("role", "availability", "claim_consequence", "criterion")) {
    damaged = property()
    damaged[[field]] = NULL
    expect_error(csdg_adjudicate_claim(list(damaged)), "missing required field")
  }
  for (availability in c("incomplete", "unavailable")) {
    result = csdg_adjudicate_claim(list(property(status = "open", availability = availability)))
    expect_identical(result$assessment, "unresolved")
    expect_identical(result$open_gate_ids, "G2")
  }
  expect_identical(csdg_adjudicate_claim(list(property(), property(status = "open")))$assessment, "unresolved")
  expect_identical(csdg_adjudicate_claim(list(property()))$assessment, "met")
  expect_match(csdg_adjudicate_claim(list(property()))$rationale, "relative to the listed required properties")
})

test_that("legacy roles and consequences are mapped with deprecation warnings", {
  mlr3autoiml:::.csdg_reset_deprecations()
  captured = capture_deprecations(csdg_evidence_record("G5", TRUE, "necessary_requirement",
    result_direction = "supports", claim_consequence = "retain_exact_claim", rationale = "Legacy supporting record."))
  necessary = captured$value
  expect_true(any(grepl("necessary_requirement", captured$messages)))
  expect_true(any(grepl("retain_exact_claim", captured$messages)))
  expect_identical(necessary$role, "required_property")
  expect_identical(necessary$legacy_role, "necessary_requirement")
  expect_identical(necessary$status, "supported")
  suppressWarnings({
    defeater = csdg_evidence_record("G2", TRUE, "potential_defeater", result_direction = "challenges",
      materiality = "materialized", adjudication_basis = "substantive_adjudication",
      claim_consequence = "exact_claim_not_retained", rationale = "A verified column swap.")
    pending = csdg_evidence_record("G2", TRUE, "potential_defeater", result_direction = "descriptive",
      claim_consequence = "unresolved", rationale = "A plausible but unquantified problem.")
    objection = csdg_evidence_record("G2", TRUE, "potential_defeater", result_direction = "descriptive",
      rationale = "A possible objection.")
    challenge = csdg_evidence_record("G1", TRUE, "necessary_requirement", result_direction = "challenges",
      claim_consequence = "exact_claim_not_retained", rationale = "Legacy challenging record.")
    revision = csdg_evidence_record("G3a", TRUE, "necessary_requirement", result_direction = "mixed",
      claim_consequence = "revise_claim", rationale = "Legacy revision.")
  })
  expect_identical(defeater$role, "established_counterevidence")
  expect_identical(pending$role, "unresolved_threat")
  expect_identical(objection$role, "context")
  expect_identical(challenge$status, "contradicted")
  expect_identical(revision$status, "contradicted")
  expect_identical(csdg_adjudicate_claim(list(necessary, defeater))$assessment, "not_met")
  expect_identical(suppressWarnings(csdg_adjudicate_claim(list(necessary, pending)))$assessment, "unresolved")
  expect_identical(csdg_adjudicate_claim(list(necessary, objection))$assessment, "met")
  expect_identical(csdg_adjudicate_claim(list(revision))$assessment, "not_met")
  expect_error(suppressWarnings(csdg_evidence_record("G2", TRUE, "potential_defeater", result_direction = "challenges",
    materiality = "materialized", rationale = "No basis.")), "adjudication_basis")
  expect_error(suppressWarnings(csdg_evidence_record("G2", TRUE, "necessary_requirement",
    result_direction = "challenges", claim_consequence = "unresolved", rationale = "Contradictory.")),
    "cannot record an unresolved")
  expect_error(suppressWarnings(csdg_evidence_record("G7a", TRUE, "descriptive_context",
    claim_consequence = "retain_exact_claim", rationale = "Context.")), "Context never changes")
  expect_error(suppressWarnings(property(status = "supported", claim_consequence = "unresolved")), "disagrees")
})

test_that("a consequence on a legacy context record no longer changes the assessment (0.1.5 behavior fixed)", {
  mlr3autoiml:::.csdg_reset_deprecations()
  captured = capture_deprecations(csdg_evidence_record("G5", TRUE, "graded_support", result_direction = "supports",
    claim_consequence = "unresolved", rationale = "Legacy qualification."))
  pending = captured$value
  expect_true(any(grepl("ignored for context", captured$messages)))
  expect_identical(pending$role, "context")
  expect_identical(csdg_adjudicate_claim(list(property(), pending))$assessment, "met")
  support = suppressWarnings(csdg_evidence_record("G5", TRUE, "graded_support", result_direction = "supports",
    claim_consequence = "retain_exact_claim", rationale = "Legacy favorable context."))
  expect_identical(csdg_adjudicate_claim(list(support))$assessment, "unresolved")
})

test_that("counterevidence on a gate that the plan does not require warns and still contradicts it", {
  claim = csdg_claim(id = "plan_ce", statement = "The learner relies on x1.", claim_type = "global_explanation",
    quantity = "held-out PFI", model = "learner", procedure = "3-fold CV", data = "synthetic",
    meaning = "model_description", use = "description")
  plan = csdg_gate_plan(claim, csdg_measurement(), csdg_explanation(method_ids = "pfi"))
  expect_false(plan[gate_id == "G4", required])
  counterevidence = csdg_evidence_record("G4", TRUE, "established_counterevidence",
    rationale = "A verified defect of the local surrogate.",
    required_property = "The local surrogate reproduces the model.",
    observation = "The surrogate was fitted to the wrong model.",
    relevance_to_proposition = "Illustration of a misattached record.")
  expect_warning(
    result <- csdg_adjudicate_claim(list(counterevidence), claim_applicable = TRUE, plan = plan),
    "does not require"
  )
  expect_identical(result$assessment, "not_met")
  expect_identical(result$counterevidence_gate_ids, "G4")
})
