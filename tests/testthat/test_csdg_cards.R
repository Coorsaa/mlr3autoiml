test_that("cards validate and plan claim-specific gates", {
  fx = make_classif_fixture()
  cards = make_cards(fx$task)
  expect_s3_class(cards$claim, "CSDGClaim")
  expect_s3_class(cards$measurement, "CSDGMeasurement")
  expect_s3_class(cards$explanation, "CSDGExplanation")

  plan = csdg_gate_plan(
    cards$claim, cards$measurement, cards$explanation
  )
  expect_s3_class(plan, "CSDGGatePlan")
  expect_equal(plan$gate_id, c("G0a", "G0b", "G1", "G2", "G3a", "G3b", "G4", "G5", "G6a", "G6b", "G7a", "G7b"))
  expect_true(plan[gate_id == "G5", required])
  expect_false(plan[gate_id == "G6a", required])
  expect_identical(plan[gate_id == "G1", evidence_role], "necessary_requirement")
  expect_setequal(unique(plan$evidence_role), mlr3autoiml:::.csdg_evidence_roles)
})

test_that("use adequacy is separate from the three inference levels", {
  expect_error(csdg_claim(claim_level = "use"), "claim_level")
  for (level in c("functional", "predictive", "substantive")) {
    expect_identical(csdg_claim(claim_level = level)$claim_level, level)
  }

  fx = make_classif_fixture()
  measurement = make_cards(fx$task)$measurement
  use_claim = csdg_claim(
    statement = "The evaluated explanation is adequate for a declared research workflow.",
    claim_type = "local_explanation",
    target = "individual fitted prediction",
    population = "synthetic population",
    analytic_distribution = "synthetic analytic sample",
    scientific_use = "research workflow",
    explanation_design = "local surrogate",
    claim_level = "substantive",
    use_claim = TRUE
  )
  plan = csdg_gate_plan(use_claim, measurement, csdg_explanation(scope = "local"))

  expect_true(use_claim$use_claim)
  expect_true(plan[gate_id == "G7b", applicable])
  expect_identical(plan[gate_id == "G7b", evidence_role], "necessary_requirement")
})

test_that("claim revisions retain an explicit version relationship", {
  original = csdg_claim(
    id = "claim_original",
    statement = "An explanation generalizes across models.",
    target = "predictions",
    population = "analytic sample",
    analytic_distribution = "observed rows",
    scientific_use = "Model interpretation",
    explanation_design = "Held-out PFI",
    claim_type = "model_generalization",
    model_scope = "model_class"
  )
  revised = csdg_claim_revision(
    original,
    id = "claim_revised",
    statement = "The explanation describes the selected fitted model.",
    claim_version = "C1",
    revision_relation = "narrower",
    model_scope = "selected_model"
  )

  expect_identical(revised$parent_claim_id, original$id)
  expect_identical(revised$claim_version, "C1")
  expect_identical(revised$revision_relation, "narrower")
  expect_identical(revised$model_scope, "selected_model")
  expect_named(revised$coordinates, mlr3autoiml:::.csdg_claim_coordinates)
})

test_that("claim relations cover all six coordinates and do not force an order", {
  original = csdg_claim(
    id = "SHILD-C0",
    statement = "Person-specific explanations support follow-up use.",
    target = "person-specific prediction",
    population = "analytic sample",
    analytic_distribution = "two-extremes analytic sample",
    scientific_use = "individual follow-up",
    explanation_design = "local additive ridge surrogate"
  )
  alternative = csdg_claim_revision(
    original,
    id = "SHILD-C3",
    statement = "Held-out marginal PFI describes the selected model.",
    claim_version = "C3",
    revision_relation = "alternative_or_incomparable",
    target = "global loss increase",
    scientific_use = "global selected-model description",
    explanation_design = "held-out marginal PFI"
  )
  relation = csdg_claim_relation(
    original,
    alternative,
    coordinate_relations = c(
      target = "alternative_or_incomparable",
      model_scope = "same",
      semantics = "same",
      analytic_distribution = "same",
      scientific_use = "alternative_or_incomparable",
      explanation_design = "alternative_or_incomparable"
    ),
    rationale = "The target, use, and explanation design change the scientific question."
  )

  expect_identical(relation$relation, "unchecked")
  expect_identical(relation$context_relation, "alternative_or_incomparable")
  expect_identical(relation$coordinates$coordinate, mlr3autoiml:::.csdg_claim_coordinates)
  narrower = csdg_claim_relation(
    original,
    alternative,
    coordinate_relations = c(
      target = "narrower",
      model_scope = "same",
      semantics = "same",
      analytic_distribution = "same",
      scientific_use = "narrower",
      explanation_design = "narrower"
    ),
    rationale = "Every declared change is a restriction for this comparison."
  )
  mixed = csdg_claim_relation(
    original,
    alternative,
    coordinate_relations = c(
      target = "narrower",
      model_scope = "broader",
      semantics = "same",
      analytic_distribution = "same",
      scientific_use = "same",
      explanation_design = "same"
    ),
    rationale = "Opposing coordinate changes do not define one order."
  )
  expect_identical(narrower$relation, "unchecked")
  expect_identical(narrower$context_relation, "narrower")
  expect_identical(mixed$relation, "unchecked")
  expect_identical(mixed$context_relation, "alternative_or_incomparable")
  expect_error(
    csdg_claim_relation(
      original,
      alternative,
      coordinate_relations = c(target = "same"),
      rationale = "Incomplete declaration"
    ),
    "every claim coordinate"
  )
})

test_that("claim-relation schema requires every formal coordinate exactly once", {
  schema_path = system.file("schema", "csdg-claim-relation.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(schema_path, simplifyVector = FALSE)
  coordinate_rules = schema$properties$coordinates$allOf
  required_coordinates = vapply(
    coordinate_rules,
    function(rule) rule$contains$properties$coordinate$const,
    character(1L)
  )

  expect_setequal(required_coordinates, mlr3autoiml:::.csdg_claim_coordinates)
  expect_true(all(vapply(coordinate_rules, function(rule) identical(rule$minContains, 1L), logical(1L))))
  expect_true(all(vapply(coordinate_rules, function(rule) identical(rule$maxContains, 1L), logical(1L))))
  expect_identical(schema$properties$coordinates$minItems, 6L)
  expect_identical(schema$properties$coordinates$maxItems, 6L)
  expect_false(schema$additionalProperties)
  expect_false(schema$properties$coordinates$items$additionalProperties)
  expect_identical(schema$properties$parent_claim_id$type, "string")
  expect_identical(schema$properties$claim_id$type, "string")
  expect_identical(schema$properties$rationale$type, "string")
})

test_that("claim-relation schema keeps context and coordinate relations consistent", {
  schema_path = system.file("schema", "csdg-claim-relation.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(schema_path, simplifyVector = FALSE)
  validate_fragment = function(value, fragment) {
    if (!is.null(fragment$const) && !identical(value, fragment$const)) {
      return(FALSE)
    }
    if (!is.null(fragment$enum) && !value %in% unlist(fragment$enum, use.names = FALSE)) {
      return(FALSE)
    }
    if (!is.null(fragment$required) &&
        (!is.list(value) || !all(unlist(fragment$required, use.names = FALSE) %in% names(value)))) {
      return(FALSE)
    }
    if (!is.null(fragment$properties)) {
      if (!is.list(value) || is.null(names(value))) {
        return(FALSE)
      }
      for (field in intersect(names(fragment$properties), names(value))) {
        if (!validate_fragment(value[[field]], fragment$properties[[field]])) {
          return(FALSE)
        }
      }
    }
    if (!is.null(fragment$items) &&
        !all(vapply(value, function(item) validate_fragment(item, fragment$items), logical(1L)))) {
      return(FALSE)
    }
    if (!is.null(fragment$contains)) {
      matches = vapply(value, function(item) validate_fragment(item, fragment$contains), logical(1L))
      minimum = if (is.null(fragment$minContains)) 1L else fragment$minContains
      maximum = if (is.null(fragment$maxContains)) Inf else fragment$maxContains
      if (sum(matches) < minimum || sum(matches) > maximum) {
        return(FALSE)
      }
    }
    if (!is.null(fragment$allOf) &&
        !all(vapply(fragment$allOf, function(rule) validate_fragment(value, rule), logical(1L)))) {
      return(FALSE)
    }
    if (!is.null(fragment$anyOf) &&
        !any(vapply(fragment$anyOf, function(rule) validate_fragment(value, rule), logical(1L)))) {
      return(FALSE)
    }
    TRUE
  }
  schema_accepts_relation = function(instance) {
    all(vapply(schema$allOf, function(rule) {
      if (!validate_fragment(instance, rule[["if"]])) {
        return(TRUE)
      }
      validate_fragment(instance, rule$then)
    }, logical(1L)))
  }
  make_instance = function(relation, coordinate_relations) {
    stopifnot(length(coordinate_relations) == length(mlr3autoiml:::.csdg_claim_coordinates))
    list(
      parent_claim_id = "C0",
      claim_id = "C1",
      relation = "unchecked",
      context_relation = relation,
      proposition_relation = "unchecked",
      proposition_rationale = NULL,
      parent_statement = "Parent proposition.",
      claim_statement = "Revised proposition.",
      revision_kind = "unspecified",
      semantic_status = "unchecked",
      coordinates = lapply(seq_along(coordinate_relations), function(index) {
        list(
          coordinate = mlr3autoiml:::.csdg_claim_coordinates[[index]],
          parent_value = "parent",
          claim_value = "claim",
          relation = coordinate_relations[[index]]
        )
      }),
      rationale = "Declared comparison."
    )
  }
  repeated = function(value) rep(value, length(mlr3autoiml:::.csdg_claim_coordinates))

  valid = list(
    make_instance("same", repeated("same")),
    make_instance("narrower", c("narrower", repeated("same")[-1L])),
    make_instance("broader", c("broader", repeated("same")[-1L])),
    make_instance("alternative_or_incomparable", c("alternative_or_incomparable", repeated("same")[-1L])),
    make_instance("alternative_or_incomparable", c("narrower", "broader", repeated("same")[-c(1L, 2L)]))
  )
  invalid = list(
    make_instance("same", c("narrower", repeated("same")[-1L])),
    make_instance("narrower", repeated("same")),
    make_instance("narrower", c("narrower", "broader", repeated("same")[-c(1L, 2L)])),
    make_instance("broader", repeated("same")),
    make_instance("broader", c("broader", "narrower", repeated("same")[-c(1L, 2L)])),
    make_instance("alternative_or_incomparable", repeated("same")),
    make_instance("alternative_or_incomparable", c("narrower", repeated("same")[-1L])),
    make_instance("alternative_or_incomparable", c("broader", repeated("same")[-1L]))
  )

  expect_true(all(vapply(valid, schema_accepts_relation, logical(1L))))
  expect_false(any(vapply(invalid, schema_accepts_relation, logical(1L))))
  root_relations = vapply(
    Filter(function(rule) !is.null(rule[["if"]]$properties$context_relation), schema$allOf),
    function(rule) rule[["if"]]$properties$context_relation$const,
    character(1L)
  )
  expect_setequal(root_relations, mlr3autoiml:::.csdg_claim_relations)
})

test_that("decision cards require decision semantics at G0a", {
  fx = make_classif_fixture()
  cards = make_cards(fx$task)
  decision_claim = csdg_claim(
    statement = "Predictions should guide an action.",
    claim_type = "decision",
    target = "y",
    population = "synthetic",
    analytic_distribution = "synthetic",
    model_scope = "selected_model",
    setting_scope = "analytic_sample"
  )
  result = csdg_audit(
    fx$task, fx$learner, decision_claim, cards$measurement,
    config = csdg_config(resampling = list(folds = 3L, repeats = 1L)),
    run_gates = "G0a"
  )
  expect_equal(result$gates$G0a$status, "not_met")
})

test_that("preprocessing review requires an artifact", {
  expect_error(
    csdg_measurement(
      verification = list(status = "author_reviewed")
    ),
    "artifact"
  )
})

test_that("coauthor verification is distinct from independent reproduction", {
  measurement = csdg_measurement(
    verification = list(
      status = "verified_by_responsible_coauthors",
      artifact = "Author confirmation dated 2026-09-09",
      notes = "No independent reconstruction of protected source data is claimed."
    )
  )

  expect_identical(measurement$verification$status, "verified_by_responsible_coauthors")
  expect_null(measurement$verification$reviewer)
})

test_that("local claims do not trigger global PFI stability", {
  fx = make_classif_fixture()
  measurement = make_cards(fx$task)$measurement
  claim = csdg_claim(
    id = "local_only",
    statement = "A local surrogate approximates the held-out model near a declared case.",
    claim_type = "local_explanation",
    target = "binary probability",
    unit = "row",
    population = "synthetic population",
    analytic_distribution = "synthetic sample",
    model_scope = "cross_fitted_pipeline",
    setting_scope = "analytic_sample",
    scientific_use = "Local fitted-model description",
    explanation_design = "Held-out cross-fitted local surrogate"
  )
  plan = csdg_gate_plan(claim, measurement, csdg_explanation(scope = "local"))

  expect_true(plan[gate_id == "G4", required])
  expect_false(plan[gate_id == "G5", required])
})

test_that("card serialization preserves field names and removes other attributes", {
  card = structure(
    list(first = 1L, second = "two"),
    class = c("SyntheticCard", "list"),
    custom_attribute = "remove me"
  )

  serialized = .card_to_list(card)

  expect_identical(serialized, list(first = 1L, second = "two"))
  expect_identical(names(serialized), c("first", "second"))
  expect_null(attr(serialized, "class"))
  expect_null(attr(serialized, "custom_attribute"))
})

test_that("card serialization rejects unnamed or ambiguously named lists", {
  expect_error(.card_to_list(list(1L)), "uniquely named list")
  expect_error(.card_to_list(structure(list(1L), names = "")), "uniquely named list")
  expect_error(.card_to_list(structure(list(1L, 2L), names = c("field", "field"))), "uniquely named list")
})
