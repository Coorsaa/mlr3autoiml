
#' @rdname csdg_cards
#' @export
csdg_claim = function(
    id = "primary_claim",
    statement = NULL,
    claim_type = "predictive_performance",
    target = NULL,
    semantics = "fitted_model_description",
    unit = NULL,
    population = NULL,
    analytic_distribution = NULL,
    model_scope = "selected_model",
    setting_scope = "analytic_sample",
    scientific_use = NULL,
    explanation_design = NULL,
    claim_level = "substantive",
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
    ...) {
  dots = list(...)
  .assert_named_dots(dots)
  statement = statement %||% dots$claim %||% dots$text %||% dots$description
  if (is.null(statement)) {
    statement = "Claim statement not yet supplied."
  }
  if (!is.null(dots$type)) claim_type = dots$type
  if (!is.null(dots$types)) claim_type = dots$types
  if (!is.null(dots$model)) model_scope = dots$model
  if (!is.null(dots$setting)) setting_scope = dots$setting
  if (!is.null(dots$subgroups)) subgroup_variables = dots$subgroups

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
  .assert_choice(
    model_scope,
    c("selected_model", "cross_fitted_pipeline", "near_equivalent_models", "model_class", "unspecified"),
    "model_scope"
  )
  .assert_choice(
    semantics,
    c("fitted_model_description", "hypothetical_model_query", "causal", "recourse"),
    "semantics"
  )
  .assert_choice(claim_level, c("functional", "predictive", "substantive", "use"), "claim_level")
  .assert_choice(
    revision_relation,
    c("original", "narrower", "stronger_excluded", "alternative"),
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
  .assert_optional_character(target, "target")
  .assert_optional_character(unit, "unit")
  .assert_optional_character(population, "population")
  .assert_optional_character(analytic_distribution, "analytic_distribution")
  .assert_optional_character(scientific_use, "scientific_use")
  .assert_optional_character(explanation_design, "explanation_design")
  .assert_optional_character(intended_users, "intended_users")
  .assert_optional_character(action, "action")
  .assert_optional_character(consequences, "consequences")
  .assert_optional_character(subgroup_variables, "subgroup_variables")
  .assert_optional_character(notes, "notes")
  checkmate::assert_flag(confirmatory)
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
      unit = unit,
      population = population,
      analytic_distribution = analytic_distribution,
      model_scope = model_scope,
      setting_scope = setting_scope,
      scientific_use = scientific_use,
      explanation_design = explanation_design,
      claim_level = claim_level,
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

#' Revise an interpretation claim explicitly
#'
#' Creates a new claim card linked to an earlier claim.
#' The function records the analyst's declared relation and does not infer that the revision is supported or maximal.
#'
#' @param claim A `CSDGClaim` to revise.
#' @param id Unique identifier for the revised claim.
#' @param statement Complete revised claim statement.
#' @param claim_version Version label for the revised claim.
#' @param revision_relation One of `"narrower"`, `"stronger_excluded"`, or `"alternative"`.
#' @param ... Named claim-card fields that replace fields inherited from `claim`.
#'
#' @return A `CSDGClaim` linked to `claim` through `parent_claim_id`.
#' @examples
#' original = csdg_claim(
#'   id = "claim_original",
#'   statement = "The explanation applies to every model and setting.",
#'   target = "prediction",
#'   population = "analytic sample",
#'   analytic_distribution = "observed rows"
#' )
#' revised = csdg_claim_revision(
#'   original,
#'   id = "claim_revised",
#'   claim_version = "C1",
#'   statement = "The explanation describes the selected model in the analytic sample."
#' )
#' revised
#' @export
csdg_claim_revision = function(
    claim,
    id,
    statement,
    claim_version,
    revision_relation = "narrower",
    ...) {
  assert_class(claim, "CSDGClaim", .var.name = "claim")
  assert_string(id, min.chars = 1L, .var.name = "id")
  assert_string(statement, min.chars = 1L, .var.name = "statement")
  assert_string(claim_version, min.chars = 1L, .var.name = "claim_version")
  assert_choice(
    revision_relation,
    c("narrower", "stronger_excluded", "alternative"),
    .var.name = "revision_relation"
  )
  replacements = list(...)
  .assert_named_dots(replacements)
  inherited = unclass(claim)
  inherited$completeness = NULL
  inherited$additional = NULL
  inherited$id = id
  inherited$statement = statement
  inherited$claim_version = claim_version
  inherited$parent_claim_id = claim$id
  inherited$revision_relation = revision_relation
  inherited = .recursive_modify(inherited, replacements)
  do.call(csdg_claim, inherited)
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
    c("not_checked", "author_reviewed", "independently_reviewed"),
    "verification$status"
  )
  if (verification$status %in% c("author_reviewed", "independently_reviewed") &&
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
