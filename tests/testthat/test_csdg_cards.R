required_gates = function(claim, explanation = csdg_explanation(method_ids = "pfi")) {
  plan = csdg_gate_plan(claim, csdg_measurement(), explanation)
  plan$gate_id[plan$required]
}

example_claim = function(claim_type = "global_explanation", ...) {
  csdg_claim(
    id = "example", statement = "An example claim.", claim_type = claim_type,
    quantity = "marginal PFI", procedure = "held-out permutation", data = "analytic sample",
    use = "scientific description", ...
  )
}

test_that("cards validate and plan claim-specific gates", {
  fx = make_classif_fixture()
  cards = make_cards(fx$task)
  expect_s3_class(cards$claim, "CSDGClaim")
  expect_s3_class(cards$measurement, "CSDGMeasurement")
  expect_s3_class(cards$explanation, "CSDGExplanation")

  plan = csdg_gate_plan(cards$claim, cards$measurement, cards$explanation)
  expect_s3_class(plan, "CSDGGatePlan")
  expect_equal(plan$gate_id, c("G0a", "G0b", "G1", "G2", "G3a", "G3b", "G4", "G5", "G6a", "G6b", "G7a", "G7b"))
  expect_true(plan[gate_id == "G5", required])
  expect_false(plan[gate_id == "G6a", required])
  expect_identical(plan[gate_id == "G1", evidence_role], "required_property")
  expect_setequal(unique(plan$evidence_role), c("required_property", "context"))
  expect_identical(plan$gate_name, csdg_gate_registry()$gate_name)
  expect_identical(plan$applicable, plan$required)
  expect_false(isTRUE(attr(plan, "causal_design_required")))
})

test_that("the gate registry reproduces Table 3 of the article", {
  registry = csdg_gate_registry()
  expect_identical(registry$gate_id, mlr3autoiml:::.csdg_gate_ids)
  expect_identical(registry$gate_name, c(
    "Specification", "Measurement and data", "Predictive performance", "Procedure", "Calibration", "Decisions",
    "Local fidelity", "Stability", "Models", "Settings", "Subgroups", "Users"
  ))
  expect_identical(registry[area == "Foundation of the claim", gate_id], c("G0a", "G0b", "G1"))
  expect_identical(registry[area == "Predictions, explanations, and decisions", gate_id],
    c("G2", "G3a", "G3b", "G4", "G5"))
  expect_identical(registry[area == "Extensions", gate_id], c("G6a", "G6b", "G7a", "G7b"))
  expect_identical(registry[gate_id == "G6a", required_if], "covers several models or learners, or is a population claim")
  expect_identical(registry[gate_id == "G4", evidence_question],
    "Does a local surrogate reproduce the model near the case, on points not used to fit it?")
  expect_false("CD" %in% registry$gate_id)
  with_design = csdg_gate_registry(include_causal_design = TRUE)
  expect_identical(with_design[gate_id == "CD", gate_name], "Causal design")
  expect_identical(with_design[gate_id == "CD", required_if], "is causal")
})

test_that("gate plans reproduce the claim types of the article", {
  # Numerical example: original claim about models A and B, and the revised claim about model A.
  both = example_claim(model = "several_models")
  expect_identical(required_gates(both), c("G0a", "G0b", "G2", "G5", "G6a"))
  plan = csdg_gate_plan(both, csdg_measurement(), csdg_explanation(method_ids = "pfi"))
  expect_identical(plan[gate_id == "G1", plan_role], "context")
  expect_identical(required_gates(example_claim(model = "fitted_model")), c("G0a", "G0b", "G2", "G5"))
  # PISA model agreement: three learners, model description.
  expect_identical(required_gates(example_claim(model = "several_models", claim_type = "global_explanation")),
    c("G0a", "G0b", "G2", "G5", "G6a"))
  # Importance in the population.
  expect_identical(required_gates(example_claim(model = "learner", meaning = "population_claim")),
    c("G0a", "G0b", "G1", "G2", "G5", "G6a"))
  # Individual explanations with a local surrogate; G1 is context.
  local = example_claim(claim_type = "local_explanation", model = "fitted_model")
  local_plan = csdg_gate_plan(local, csdg_measurement(), csdg_explanation(method_ids = "local_surrogate",
    scope = "local"))
  expect_identical(local_plan$gate_id[local_plan$required], c("G0a", "G0b", "G2", "G4", "G5"))
  expect_identical(local_plan[gate_id == "G1", plan_role], "context")
  expect_identical(required_gates(local, csdg_explanation(method_ids = "shapley", scope = "local")),
    c("G0a", "G0b", "G2", "G5"))
  expect_true("G4" %in% required_gates(local, NULL))
  # Explanations used for decisions about people (statement d).
  decision = example_claim(claim_type = c("local_explanation", "decision"), model = "learner",
    action = "invite", thresholds = 0.1, consequences = "burden", intended_users = "counselors")
  expect_identical(required_gates(decision, csdg_explanation(method_ids = "local_surrogate", scope = "local")),
    c("G0a", "G0b", "G1", "G2", "G3b", "G4", "G5", "G7b"))
  calibrated = example_claim(claim_type = c("local_explanation", "decision", "calibration"), model = "learner",
    action = "invite", thresholds = 0.1, consequences = "burden", intended_users = "counselors")
  expect_true("G3a" %in% required_gates(calibrated))
  # Comparison across models, subgroup claim, and causal claim.
  expect_identical(required_gates(example_claim(claim_type = c("global_explanation", "model_generalization"))),
    c("G0a", "G0b", "G2", "G5", "G6a"))
  expect_identical(required_gates(example_claim(claim_type = c("global_explanation", "subgroup"))),
    c("G0a", "G0b", "G2", "G5", "G7a"))
  expect_false("G7a" %in% required_gates(example_claim(subgroup_variables = "age_group")))
  causal = example_claim(meaning = "causal_claim")
  causal_plan = csdg_gate_plan(causal, csdg_measurement(), csdg_explanation(method_ids = "pfi"))
  expect_identical(causal_plan$gate_id[causal_plan$required], c("G0a", "G0b", "G1", "G2", "G5", "G6a"))
  expect_true(attr(causal_plan, "causal_design_required"))
  # A claim that extends beyond the sampled setting adds G6b; prediction-only claims need no G2 or G5.
  expect_true("G6b" %in% required_gates(example_claim(setting_scope = "other countries")))
  expect_identical(required_gates(csdg_claim(claim_type = "predictive_performance")), c("G0a", "G0b", "G1"))
})

test_that("claim cards accept the scope elements under their article names", {
  claim = csdg_claim(
    statement = "Under marginal permutation, both models rely more on item 1 than on item 2.",
    claim_type = "global_explanation", quantity = "marginal PFI", model = "several_models",
    procedure = "independent permutation", data = "Z standard normal", meaning = "model_description",
    use = "scientific description"
  )
  expect_named(claim$scope, mlr3autoiml:::.csdg_scope_elements)
  expect_identical(claim$scope$model, "several_models")
  expect_identical(claim$target, "marginal PFI")
  expect_identical(claim$explanation_design, "independent permutation")
  expect_identical(claim$analytic_distribution, "Z standard normal")
  expect_identical(claim$scientific_use, "scientific description")
  expect_identical(claim$meaning, "model_description")
  expect_identical(claim$semantics, "fitted_model_description")
  expect_named(claim$coordinates, mlr3autoiml:::.csdg_claim_coordinates)
  expect_identical(csdg_claim()$meaning, "model_description")
  expect_identical(csdg_claim()$model_scope, "fitted_model")
  expect_identical(csdg_claim(meaning = "causal_claim")$semantics, "causal")
  expect_error(csdg_claim(quantity = "a", target = "b"), "not both")
  expect_error(csdg_claim(model = "learner", model_scope = "fitted_model"), "not both")
  expect_error(csdg_claim(meaning = "population"), "meaning")
  expect_identical(csdg_claim(quantity = "a", target = "a")$target, "a")
  # claim_level is documentary and does not make a claim a population claim.
  expect_identical(csdg_claim(claim_level = "substantive")$meaning, "model_description")
  expect_error(csdg_claim(claim_level = "use"), "claim_level")
})

test_that("legacy claim values are accepted with deprecation warnings", {
  mlr3autoiml:::.csdg_reset_deprecations()
  expect_warning(legacy_model <- csdg_claim(model_scope = "cross_fitted_pipeline"), class = "mlr3autoiml_deprecated")
  expect_identical(legacy_model$model_scope, "learner")
  expect_warning(legacy_semantics <- csdg_claim(semantics = "causal"), class = "mlr3autoiml_deprecated")
  expect_identical(legacy_semantics$meaning, "causal_claim")
  expect_identical(legacy_semantics$semantics, "causal")
  expect_error(suppressWarnings(csdg_claim(semantics = "causal", meaning = "model_description")), "contradicts")
  expect_identical(suppressWarnings(csdg_claim(model_scope = "near_equivalent_models"))$model_scope,
    "several_models")
  expect_identical(suppressWarnings(csdg_claim(model_scope = "selected_model"))$model_scope, "fitted_model")
  expect_identical(suppressWarnings(csdg_claim(model_scope = "model_class"))$model_scope, "several_models")
  original = csdg_claim(id = "C0", statement = "Original.")
  expect_warning(
    revised <- csdg_claim_revision(original, "C1", "Revised.", "C1", "alternative_or_incomparable"),
    class = "mlr3autoiml_deprecated"
  )
  expect_identical(revised$revision_relation, "incomparable")
})

test_that("use claims require evidence with the intended users", {
  fx = make_classif_fixture()
  measurement = make_cards(fx$task)$measurement
  use_claim = csdg_claim(
    statement = "The evaluated explanation helps the intended users.",
    claim_type = "local_explanation",
    quantity = "individual fitted prediction",
    population = "synthetic population",
    data = "synthetic analytic sample",
    use = "research workflow",
    procedure = "local surrogate",
    use_claim = TRUE
  )
  plan = csdg_gate_plan(use_claim, measurement, csdg_explanation(scope = "local"))

  expect_true(use_claim$use_claim)
  expect_true(plan[gate_id == "G7b", required])
  expect_identical(plan[gate_id == "G7b", evidence_role], "required_property")
})

test_that("claim revisions retain an explicit version relationship", {
  original = csdg_claim(
    id = "claim_original",
    statement = "An explanation generalizes across models.",
    quantity = "predictions",
    population = "analytic sample",
    data = "observed rows",
    use = "Model interpretation",
    procedure = "Held-out PFI",
    claim_type = "model_generalization",
    model = "several_models"
  )
  revised = csdg_claim_revision(
    original,
    id = "claim_revised",
    statement = "The explanation describes the fitted model.",
    claim_version = "C1",
    revision_relation = "narrower",
    model = "fitted_model"
  )

  expect_identical(revised$parent_claim_id, original$id)
  expect_identical(revised$claim_version, "C1")
  expect_identical(revised$revision_relation, "narrower")
  expect_identical(revised$model_scope, "fitted_model")
  expect_identical(revised$scope$quantity, "predictions")
  expect_named(revised$coordinates, mlr3autoiml:::.csdg_claim_coordinates)
  requantified = csdg_claim_revision(original, "claim_quantity", "New quantity.", "C2", "incomparable",
    quantity = "grouped PFI")
  expect_identical(requantified$target, "grouped PFI")
  population = csdg_claim_revision(original, "claim_population", "Population claim.", "C3", "incomparable",
    meaning = "population_claim")
  expect_identical(population$meaning, "population_claim")
})

test_that("claim relations cover all six scope elements and do not force an order", {
  original = csdg_claim(
    id = "SHILD-C0",
    statement = "Person-specific explanations support follow-up use.",
    quantity = "person-specific prediction",
    population = "analytic sample",
    data = "two-extremes analytic sample",
    use = "individual follow-up",
    procedure = "local additive ridge surrogate"
  )
  alternative = csdg_claim_revision(
    original,
    id = "SHILD-C3",
    statement = "Held-out marginal PFI describes the fitted model.",
    claim_version = "C3",
    revision_relation = "incomparable",
    quantity = "global loss increase",
    use = "global description",
    procedure = "held-out marginal PFI"
  )
  relations = function(...) {
    out = setNames(rep("same", 6L), mlr3autoiml:::.csdg_scope_elements)
    changes = c(...)
    out[names(changes)] = changes
    out
  }
  relation = csdg_claim_relation(
    original, alternative,
    coordinate_relations = relations(quantity = "incomparable", use = "incomparable", procedure = "incomparable"),
    rationale = "The quantity, use, and procedure change the scientific question."
  )

  expect_identical(relation$relation, "unchecked")
  expect_identical(relation$scope_relation, "incomparable")
  expect_identical(relation$scope$element, mlr3autoiml:::.csdg_scope_elements)
  expect_identical(relation$coordinates$coordinate, mlr3autoiml:::.csdg_claim_coordinates)
  narrower = csdg_claim_relation(original, alternative,
    coordinate_relations = relations(quantity = "narrower", use = "narrower", procedure = "narrower"),
    rationale = "Every declared change is a restriction for this comparison.")
  mixed = csdg_claim_relation(original, alternative,
    coordinate_relations = relations(quantity = "narrower", model = "broader"),
    rationale = "Opposing changes do not define one order.")
  expect_identical(narrower$scope_relation, "narrower")
  expect_identical(mixed$scope_relation, "incomparable")
  expect_error(
    csdg_claim_relation(original, alternative, coordinate_relations = c(quantity = "same"),
      rationale = "Incomplete declaration"),
    "every scope element"
  )
})

test_that("claim-relation schema requires every scope element exactly once", {
  schema_path = system.file("schema", "csdg-claim-relation.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(schema_path, simplifyVector = FALSE)
  element_rules = schema$properties$scope$allOf
  required_elements = vapply(element_rules, function(rule) rule$contains$properties$element$const, character(1L))
  expect_setequal(required_elements, mlr3autoiml:::.csdg_scope_elements)
  coordinate_rules = schema$properties$coordinates$allOf
  required_coordinates = vapply(coordinate_rules, function(rule) {
    rule$contains$properties$coordinate$const
  }, character(1L))
  expect_setequal(required_coordinates, mlr3autoiml:::.csdg_claim_coordinates)
  expect_true(all(vapply(element_rules, function(rule) identical(rule$minContains, 1L), logical(1L))))
  expect_true(all(vapply(element_rules, function(rule) identical(rule$maxContains, 1L), logical(1L))))
  expect_identical(schema$properties$scope$minItems, 6L)
  expect_identical(schema$properties$scope$maxItems, 6L)
  expect_false(schema$additionalProperties)
  expect_false(schema$properties$scope$items$additionalProperties)
  expect_identical(schema$properties$parent_claim_id$type, "string")
  expect_identical(schema$properties$rationale$type, "string")
})

test_that("claim-relation schema keeps the scope relation and the element relations consistent", {
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
      scope_relation = relation,
      proposition_relation = "unchecked",
      proposition_rationale = NULL,
      parent_statement = "Parent proposition.",
      claim_statement = "Revised proposition.",
      revision_kind = "unspecified",
      semantic_status = "unchecked",
      scope = lapply(seq_along(coordinate_relations), function(index) {
        list(
          element = mlr3autoiml:::.csdg_scope_elements[[index]],
          parent_value = "parent",
          claim_value = "claim",
          relation = coordinate_relations[[index]]
        )
      }),
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
    make_instance("incomparable", c("incomparable", repeated("same")[-1L])),
    make_instance("incomparable", c("narrower", "broader", repeated("same")[-c(1L, 2L)]))
  )
  invalid = list(
    make_instance("same", c("narrower", repeated("same")[-1L])),
    make_instance("narrower", repeated("same")),
    make_instance("narrower", c("narrower", "broader", repeated("same")[-c(1L, 2L)])),
    make_instance("broader", repeated("same")),
    make_instance("broader", c("broader", "narrower", repeated("same")[-c(1L, 2L)])),
    make_instance("incomparable", repeated("same")),
    make_instance("incomparable", c("narrower", repeated("same")[-1L])),
    make_instance("incomparable", c("broader", repeated("same")[-1L]))
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

test_that("a decision claim that names no decision leaves G0a open", {
  fx = make_classif_fixture()
  cards = make_cards(fx$task)
  decision_claim = csdg_claim(
    statement = "Predictions should guide an action.",
    claim_type = "decision",
    quantity = "y",
    population = "synthetic",
    data = "synthetic",
    model = "fitted_model",
    setting_scope = "analytic_sample"
  )
  result = csdg_audit(
    fx$task, fx$learner, decision_claim, cards$measurement,
    config = csdg_config(resampling = list(folds = 3L, repeats = 1L)),
    run_gates = "G0a"
  )
  expect_equal(result$gates$G0a$status, "open")
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

test_that("local claims require stability and, with a surrogate, local fidelity", {
  fx = make_classif_fixture()
  measurement = make_cards(fx$task)$measurement
  claim = csdg_claim(
    id = "local_only",
    statement = "A local surrogate approximates the held-out model near a declared case.",
    claim_type = "local_explanation",
    quantity = "binary probability",
    unit = "row",
    population = "synthetic population",
    data = "synthetic sample",
    model = "learner",
    setting_scope = "analytic_sample",
    use = "Local model description",
    procedure = "Held-out cross-fitted local surrogate"
  )
  plan = csdg_gate_plan(claim, measurement, csdg_explanation(scope = "local"))

  expect_true(plan[gate_id == "G4", required])
  expect_true(plan[gate_id == "G5", required])
  expect_false(plan[gate_id == "G1", required])
  expect_match(plan[gate_id == "G4", trigger], "declare method_ids")
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
