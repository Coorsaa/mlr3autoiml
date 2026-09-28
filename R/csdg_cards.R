
#' @rdname csdg_cards
#' @export
csdg_claim = function(
    id = "primary_claim",
    statement = NULL,
    claim_type = "predictive_performance",
    target = NULL,
    semantics = NULL,
    unit = NULL,
    population = NULL,
    analytic_distribution = NULL,
    model_scope = "fitted_model",
    setting_scope = "analytic_sample",
    scientific_use = NULL,
    explanation_design = NULL,
    claim_level = "substantive",
    use_claim = FALSE,
    claim_version = "C0",
    parent_claim_id = NULL,
    revision_relation = "original",
    intended_users = NULL,
    action = NULL,
    thresholds = NULL,
    consequences = NULL,
    subgroup_variables = NULL,
    confirmatory = FALSE,
    notes = NULL,
    provenance = NULL,
    quantity = NULL,
    procedure = NULL,
    data = NULL,
    meaning = NULL,
    use = NULL,
    ...) {
  dots = list(...)
  .assert_named_dots(dots)
  statement = statement %||% dots$claim %||% dots$text %||% dots$description
  if (is.null(statement)) {
    statement = "Claim statement not yet supplied."
  }
  if (!is.null(dots$type)) claim_type = dots$type
  if (!is.null(dots$types)) claim_type = dots$types
  if (!is.null(dots$model)) {
    if (!missing(model_scope) && !identical(model_scope, dots$model)) {
      .csdg_stop("Supply either `model` or its legacy alias `model_scope`, not both with different values.")
    }
    model_scope = dots$model
  }
  if (!is.null(dots$setting)) setting_scope = dots$setting
  if (!is.null(dots$subgroups)) subgroup_variables = dots$subgroups

  merge_alias = function(new, old, new_name, old_name) {
    if (!is.null(new) && !is.null(old) && !identical(new, old)) {
      .csdg_stop(
        "Supply either `%s` or its legacy alias `%s`, not both with different values.",
        new_name, old_name
      )
    }
    new %||% old
  }
  target = merge_alias(quantity, target, "quantity", "target")
  explanation_design = merge_alias(procedure, explanation_design, "procedure", "explanation_design")
  analytic_distribution = merge_alias(data, analytic_distribution, "data", "analytic_distribution")
  scientific_use = merge_alias(use, scientific_use, "use", "scientific_use")

  allowed = c(
    "predictive_performance",
    "calibration",
    "global_explanation",
    "local_explanation",
    "subgroup",
    "model_generalization",
    "setting_generalization",
    "decision"
  )
  checkmate::assert_character(claim_type, any.missing = FALSE, min.len = 1L)
  claim_type = unique(claim_type)
  .assert_scalar_string(id, "id")
  .assert_scalar_string(statement, "statement")
  .assert_choice(claim_type, allowed, "claim_type", multiple = TRUE)
  .assert_scalar_string(model_scope, "model_scope")
  model_scope = .csdg_map_legacy(model_scope, .csdg_legacy_model_scopes, "Model value")
  .assert_choice(model_scope, .csdg_model_scopes, "model")
  if (!is.null(semantics)) {
    .assert_choice(
      semantics,
      c("fitted_model_description", "hypothetical_model_query", "causal", "recourse"),
      "semantics"
    )
    semantic_meaning = unname(.csdg_legacy_semantics[[semantics]])
    .csdg_deprecate(semantics, paste0("meaning = ", semantic_meaning), "semantics")
  }
  if (!is.null(meaning)) {
    .assert_choice(meaning, .csdg_meanings, "meaning")
    if (!is.null(semantics) &&
        xor(identical(meaning, "causal_claim"), identical(semantic_meaning, "causal_claim"))) {
      .csdg_stop("`meaning = \"%s\"` contradicts the legacy `semantics = \"%s\"`.", meaning, semantics)
    }
  } else {
    meaning = if (is.null(semantics)) "model_description" else semantic_meaning
  }
  semantics = semantics %||% if (identical(meaning, "causal_claim")) "causal" else "fitted_model_description"
  .assert_choice(claim_level, c("functional", "predictive", "substantive"), "claim_level")
  assert_flag(use_claim, .var.name = "use_claim")
  .assert_scalar_string(revision_relation, "revision_relation")
  revision_relation = .csdg_map_legacy(revision_relation, .csdg_legacy_relations, "Revision relation")
  .assert_choice(
    revision_relation,
    c("original", .csdg_scope_relations),
    "revision_relation"
  )
  .assert_scalar_string(claim_version, "claim_version")
  .assert_scalar_string(parent_claim_id, "parent_claim_id", allow_null = TRUE)
  if (identical(revision_relation, "original") && !is.null(parent_claim_id)) {
    .csdg_stop("An original claim cannot have a `parent_claim_id`.")
  }
  if (!identical(revision_relation, "original") && is.null(parent_claim_id)) {
    .csdg_stop("`parent_claim_id` is required for a revised or excluded claim.")
  }
  .assert_scalar_string(setting_scope, "setting_scope")
  .assert_optional_character(target, "quantity")
  .assert_optional_character(unit, "unit")
  .assert_optional_character(population, "population")
  .assert_optional_character(analytic_distribution, "data")
  .assert_optional_character(scientific_use, "use")
  .assert_optional_character(explanation_design, "procedure")
  .assert_optional_character(intended_users, "intended_users")
  .assert_optional_character(action, "action")
  .assert_optional_character(consequences, "consequences")
  .assert_optional_character(subgroup_variables, "subgroup_variables")
  .assert_optional_character(notes, "notes")
  checkmate::assert_flag(confirmatory)
  provenance = .csdg_validate_claim_provenance(provenance)
  checkmate::assert_true(
    is.null(thresholds) || is.atomic(thresholds) || is.list(thresholds),
    .var.name = "thresholds"
  )

  if ("decision" %in% claim_type) {
    missing_decision = c(
      action = is.null(action),
      thresholds = is.null(thresholds),
      consequences = is.null(consequences),
      intended_users = is.null(intended_users)
    )
  } else {
    missing_decision = setNames(logical(0), character(0))
  }

  structure(
    list(
      id = id,
      statement = statement,
      claim_type = claim_type,
      target = target,
      semantics = semantics,
      meaning = meaning,
      unit = unit,
      population = population,
      analytic_distribution = analytic_distribution,
      model_scope = model_scope,
      setting_scope = setting_scope,
      scientific_use = scientific_use,
      explanation_design = explanation_design,
      claim_level = claim_level,
      use_claim = use_claim,
      claim_version = claim_version,
      parent_claim_id = parent_claim_id,
      revision_relation = revision_relation,
      intended_users = intended_users,
      action = action,
      thresholds = thresholds,
      consequences = consequences,
      subgroup_variables = subgroup_variables,
      confirmatory = isTRUE(confirmatory),
      notes = notes,
      provenance = provenance,
      scope = list(
        quantity = target,
        model = model_scope,
        procedure = explanation_design,
        data = analytic_distribution,
        meaning = meaning,
        use = scientific_use
      ),
      coordinates = list(
        target = target,
        model_scope = model_scope,
        semantics = semantics,
        analytic_distribution = analytic_distribution,
        scientific_use = scientific_use,
        explanation_design = explanation_design
      ),
      completeness = list(
        decision_fields_missing = names(missing_decision)[missing_decision]
      ),
      additional = dots[setdiff(
        names(dots),
        c("claim", "text", "description", "type", "types",
          "model", "setting", "subgroups")
      )]
    ),
    class = c("CSDGClaim", "list")
  )
}

#' Revise a claim explicitly
#'
#' Creates a new claim card linked to an earlier claim.
#' A revised claim is a new claim with its own record and assessment, linked to its predecessor.
#' The function records the declared relation of the scope and does not infer that the revision is supported.
#' A revision requires a new identifier.
#' Provenance and the legacy `confirmatory` flag are not inherited: provide new documentary metadata explicitly;
#' a claim written after the results were seen is exploratory.
#' Recording a revision does not remove the inferential consequences of outcome-dependent selection.
#'
#' @param claim A `CSDGClaim` to revise.
#' @param id Unique identifier for the revised claim.
#' @param statement Complete revised claim statement.
#' @param claim_version Version label for the revised claim.
#' @param revision_relation Relation of the revised scope to the predecessor's scope: `"same"`, `"narrower"`,
#'   `"broader"`, or `"incomparable"`. The legacy value `"alternative_or_incomparable"` is accepted with a
#'   deprecation warning.
#' @param ... Named claim-card fields that replace fields inherited from `claim`, for example `model =
#'   "fitted_model"` or `quantity = "..."`.
#'
#' @return A `CSDGClaim` linked to `claim` through `parent_claim_id`.
#' @examples
#' original = csdg_claim(
#'   id = "both_models",
#'   statement = "Under marginal permutation, both models rely more on item 1 than on item 2.",
#'   claim_type = "global_explanation",
#'   quantity = "marginal PFI with squared error",
#'   model = "several_models",
#'   procedure = "each item permuted independently of the other item and the outcome",
#'   data = "Z standard normal; item 1 = item 2 = outcome = Z",
#'   meaning = "model_description",
#'   use = "scientific description"
#' )
#' revised = csdg_claim_revision(
#'   original,
#'   id = "model_a",
#'   claim_version = "C1",
#'   revision_relation = "narrower",
#'   statement = "Under marginal permutation, model A relies more on item 1 than on item 2.",
#'   model = "fitted_model"
#' )
#' revised
#' @export
csdg_claim_revision = function(
    claim,
    id,
    statement,
    claim_version,
    revision_relation,
    ...) {
  assert_class(claim, "CSDGClaim", .var.name = "claim")
  assert_string(id, min.chars = 1L, .var.name = "id")
  assert_string(statement, min.chars = 1L, .var.name = "statement")
  assert_string(claim_version, min.chars = 1L, .var.name = "claim_version")
  if (identical(id, claim$id)) .csdg_stop("A revised claim requires a new `id`; earlier claims are preserved.")
  assert_string(revision_relation, min.chars = 1L, .var.name = "revision_relation")
  revision_relation = .csdg_map_legacy(revision_relation, .csdg_legacy_relations, "Revision relation")
  assert_choice(
    revision_relation,
    .csdg_scope_relations,
    .var.name = "revision_relation"
  )
  replacements = list(...)
  .assert_named_dots(replacements)
  if (!is.null(replacements$model)) {
    replacements$model_scope = replacements$model
    replacements$model = NULL
  }
  if (!is.null(replacements$model_scope)) {
    replacements$model_scope = .csdg_map_legacy(replacements$model_scope, .csdg_legacy_model_scopes, "Model value")
  }
  if (!is.null(replacements$semantics)) {
    .assert_choice(
      replacements$semantics,
      c("fitted_model_description", "hypothetical_model_query", "causal", "recourse"),
      "semantics"
    )
    .csdg_deprecate(
      replacements$semantics,
      paste0("meaning = ", .csdg_legacy_semantics[[replacements$semantics]]),
      "semantics"
    )
  }
  inherited = unclass(claim)
  inherited$completeness = NULL
  inherited$coordinates = NULL
  inherited$scope = NULL
  inherited$additional = NULL
  # A replacement under either name replaces both the scope element and its legacy alias.
  aliases = c(quantity = "target", procedure = "explanation_design", data = "analytic_distribution",
    use = "scientific_use", meaning = "semantics")
  for (new_name in names(aliases)) {
    old_name = aliases[[new_name]]
    if (new_name %in% names(replacements) || old_name %in% names(replacements)) {
      inherited[[new_name]] = NULL
      inherited[[old_name]] = NULL
    }
  }
  if (is.null(inherited$meaning) && is.null(replacements$meaning) && is.null(replacements$semantics)) {
    inherited$meaning = claim$meaning %||% "model_description"
  }
  if (!is.null(replacements$meaning) && is.null(replacements$semantics)) {
    inherited$semantics = NULL
  }
  inherited$id = id
  inherited$statement = statement
  inherited$claim_version = claim_version
  inherited$parent_claim_id = claim$id
  inherited$revision_relation = revision_relation
  inherited$provenance = NULL
  inherited$confirmatory = FALSE
  inherited = .recursive_modify(inherited, replacements)
  # Legacy values supplied as replacements were reported above; inherited legacy fields are not reported again.
  withCallingHandlers(
    do.call(csdg_claim, inherited),
    mlr3autoiml_deprecated = function(w) invokeRestart("muffleWarning")
  )
}

.csdg_validate_claim_provenance = function(provenance) {
  if (is.null(provenance)) return(NULL)
  .assert_named_list(provenance, "provenance")
  required = c("origin", "date", "time_basis", "selection_basis", "evidence_ids")
  if (!setequal(names(provenance), required)) {
    .csdg_stop("`provenance` must contain origin, date, time_basis, selection_basis, and evidence_ids.")
  }
  assert_choice(provenance$origin, c(
    "specified_before_results", "retrospective_exploratory", "independently_confirmed"
  ), .var.name = "provenance$origin")
  for (field in c("date", "time_basis", "selection_basis")) {
    assert_string(provenance[[field]], min.chars = 1L, .var.name = paste0("provenance$", field))
  }
  date = suppressWarnings(as.Date(provenance$date, format = "%Y-%m-%d"))
  if (is.na(date) || !identical(format(date, "%Y-%m-%d"), provenance$date)) {
    .csdg_stop("`provenance$date` must be a valid YYYY-MM-DD date.")
  }
  assert_character(provenance$evidence_ids, any.missing = FALSE, unique = TRUE,
    min.len = if (provenance$origin == "independently_confirmed") 1L else 0L,
    .var.name = "provenance$evidence_ids")
  if (any(!nzchar(provenance$evidence_ids))) .csdg_stop("`provenance$evidence_ids` cannot contain empty identifiers.")
  provenance
}

.normalize_verification = function(verification) {
  verification = verification %||% list(status = "not_checked")
  if (is.character(verification) && length(verification) == 1L) {
    verification = list(status = verification)
  }
  .assert_named_list(verification, "verification")
  verification = .recursive_modify(
    list(
      status = "not_checked",
      artifact = NULL,
      reviewer = NULL,
      notes = NULL
    ),
    verification
  )
  .assert_choice(
    verification$status,
    c(
      "not_checked",
      "author_reviewed",
      "author_confirmed_verified",
      "verified_by_responsible_coauthors",
      "independently_reviewed"
    ),
    "verification$status"
  )
  if (verification$status %in% c(
      "author_reviewed",
      "author_confirmed_verified",
      "verified_by_responsible_coauthors",
      "independently_reviewed"
    ) &&
      is.null(verification$artifact)) {
    .csdg_stop(
      "`verification$artifact` is required when preprocessing is reported as reviewed."
    )
  }
  if (identical(verification$status, "independently_reviewed") &&
      is.null(verification$reviewer)) {
    .csdg_stop(
      "`verification$reviewer` is required for independently reviewed preprocessing."
    )
  }
  verification
}

#' @rdname csdg_cards
#' @export
csdg_measurement = function(
    outcome = NULL,
    predictors = NULL,
    data_source = NULL,
    sample_definition = NULL,
    unit = NULL,
    time_index = NULL,
    outcome_scale = NULL,
    missingness = NULL,
    preprocessing = NULL,
    verification = list(status = "not_checked"),
    audit_variables = NULL,
    weights = NULL,
    clusters = NULL,
    notes = NULL,
    ...) {
  dots = list(...)
  .assert_named_dots(dots)
  outcome = outcome %||% dots$target %||% dots$outcome_name
  predictors = predictors %||% dots$features %||% dots$feature_names
  sample_definition = sample_definition %||% dots$sample
  audit_variables = audit_variables %||% dots$subgroups
  verification = .normalize_verification(verification)

  .assert_optional_character(outcome, "outcome")
  .assert_optional_character(predictors, "predictors")
  .assert_optional_character(data_source, "data_source")
  .assert_optional_character(sample_definition, "sample_definition")
  .assert_optional_character(unit, "unit")
  .assert_optional_character(time_index, "time_index")
  .assert_optional_character(outcome_scale, "outcome_scale")
  .assert_optional_character(audit_variables, "audit_variables")
  .assert_optional_character(notes, "notes")
  checkmate::assert_true(is.null(weights) || is.atomic(weights), .var.name = "weights")
  checkmate::assert_true(is.null(clusters) || is.atomic(clusters), .var.name = "clusters")

  structure(
    list(
      outcome = outcome,
      predictors = unique(predictors %||% character()),
      data_source = data_source,
      sample_definition = sample_definition,
      unit = unit,
      time_index = time_index,
      outcome_scale = outcome_scale,
      missingness = missingness,
      preprocessing = preprocessing,
      verification = verification,
      audit_variables = audit_variables,
      weights = weights,
      clusters = clusters,
      notes = notes,
      additional = dots[setdiff(
        names(dots),
        c("target", "outcome_name", "features", "feature_names", "sample", "subgroups")
      )]
    ),
    class = c("CSDGMeasurement", "list")
  )
}

#' @rdname csdg_cards
#' @export
csdg_explanation = function(
    method_ids = character(),
    scope = "global",
    target = "prediction",
    feature_groups = NULL,
    perturbation = list(type = "marginal", distribution = "analytic_sample"),
    background = NULL,
    local_cases = NULL,
    case_selection = c("prespecified", "post_hoc_communication"),
    aggregation = NULL,
    notes = NULL,
    ...) {
  dots = list(...)
  .assert_named_dots(dots)
  if (!length(method_ids)) {
    method_ids = dots$methods %||% dots$method %||% character()
  }
  scope = dots$explanation_scope %||% scope
  checkmate::assert_character(case_selection, any.missing = FALSE, min.len = 1L)
  case_selection = match.arg(case_selection)
  allowed_methods = c(
    "ale", "coefficients", "counterfactual", "decision_curve", "ice", "local_surrogate",
    "marginal_shap", "other", "path_attribution", "pdp", "pfi", "shapley"
  )
  checkmate::assert_character(method_ids, any.missing = FALSE)
  method_ids = unique(method_ids)
  if (length(method_ids)) {
    .assert_choice(method_ids, allowed_methods, "method_ids", multiple = TRUE)
  }
  checkmate::assert_character(scope, any.missing = FALSE, min.len = 1L)
  scope = unique(scope)
  .assert_choice(scope, c("global", "local"), "scope", multiple = TRUE)
  .assert_scalar_string(target, "target")
  if (!is.null(feature_groups)) checkmate::assert_list(feature_groups, .var.name = "feature_groups")
  if (!is.null(feature_groups) &&
      (is.null(names(feature_groups)) || any(!nzchar(names(feature_groups))))) {
    .csdg_stop("`feature_groups` must be named.")
  }
  if (!is.null(feature_groups)) {
    checkmate::assert_true(
      all(vapply(feature_groups, function(x) {
        checkmate::test_character(x, any.missing = FALSE, min.len = 1L)
      }, logical(1L))),
      .var.name = "feature_groups"
    )
  }
  .assert_named_list(perturbation, "perturbation")
  if (!is.null(background)) checkmate::assert_data_frame(background, min.rows = 1L)
  checkmate::assert_true(
    is.null(local_cases) || is.atomic(local_cases) || is.data.frame(local_cases),
    .var.name = "local_cases"
  )
  .assert_optional_character(aggregation, "aggregation")
  .assert_optional_character(notes, "notes")

  structure(
    list(
      method_ids = method_ids,
      scope = scope,
      target = target,
      feature_groups = feature_groups,
      perturbation = perturbation,
      background = background,
      local_cases = local_cases,
      case_selection = case_selection,
      aggregation = aggregation,
      notes = notes,
      additional = dots[setdiff(
        names(dots), c("methods", "method", "explanation_scope")
      )]
    ),
    class = c("CSDGExplanation", "list")
  )
}
