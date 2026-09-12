.csdg_evidence_availability = c("complete", "incomplete", "unavailable", "not_applicable")
.csdg_result_directions = c("supports", "challenges", "mixed", "descriptive", "not_evaluated")
.csdg_materiality = c("not_materialized", "materialized", "not_applicable")
.csdg_adjudication_bases = c(
  "prespecified_claim_specific_criterion",
  "substantive_adjudication",
  "claim_relevant_sensitivity",
  "other_transparent_rule"
)
.csdg_claim_consequences = c(
  "none",
  "retain_exact_claim",
  "revise_claim",
  "exact_claim_not_retained",
  "unresolved"
)

.validate_evidence_criterion = function(criterion, source, rationale) {
  if (is.null(criterion)) {
    if (!is.null(source) || !is.null(rationale)) {
      .csdg_stop("`criterion_source` and `criterion_rationale` require a non-NULL `criterion`.")
    }
    return(invisible(TRUE))
  }
  .assert_named_list(criterion, "criterion")
  required = c("value", "direction")
  missing = setdiff(required, names(criterion))
  if (length(missing)) {
    .csdg_stop("`criterion` is missing required field(s): %s.", paste(missing, collapse = ", "))
  }
  assert_choice(
    criterion$direction,
    c("minimum", "maximum", "range", "qualitative"),
    .var.name = "criterion$direction"
  )
  assert_true(length(criterion$value) > 0L, .var.name = "criterion$value")
  assert_string(source, min.chars = 1L, .var.name = "criterion_source")
  assert_string(rationale, min.chars = 1L, .var.name = "criterion_rationale")
  invisible(TRUE)
}

#' Create an orthogonal CSDG evidence record
#'
#' @description
#' Separates module applicability, evidence role, availability, observed result direction, criterion metadata,
#' materiality, adjudication basis, and claim consequence.
#' It records an analyst's evidence characterization and does not apply a universal threshold or aggregate score.
#' Cross-field validation prevents an evidence role or observed direction from being paired with a contradictory
#' claim consequence.
#' The consequence is a binding constraint in [csdg_adjudicate_claim()], not an independent overall verdict.
#' `"unresolved"` prevents a `"met"` decision, and a justified revision prevents retaining the unchanged claim.
#' `"retain_exact_claim"` cannot override an unmet requirement, a materialized defeater,
#' or unresolved evidence elsewhere.
#' `"none"` adds no constraint beyond the evidence role, availability, and observed direction.
#' Sensitivity is relative to the proposition: changing an estimand is not automatically counterevidence.
#' New variation records that constrain a claim require a property-observation-relevance chain.
#' Changed or uncertain estimands additionally require an explicit invariance claim before they constrain a claim.
#' Legacy records without these optional fields remain usable with `proposition_linkage = "not_recorded"`.
#' A documented chain is not independently validated merely because its fields are complete.
#'
#' @param gate_id CSDG module identifier.
#' @param applicable Whether the module is applicable to the exact claim.
#' @param role One of the four CSDG evidence roles.
#' @param availability Completion or availability state.
#' @param result_direction Direction of the observed evidence relative to the exact claim.
#' @param criterion Optional named list with `value` and `direction`.
#' @param criterion_source Source of a supplied criterion.
#' @param criterion_rationale Rationale linking a supplied criterion to the claim and intended use.
#' @param materiality Whether a potential defeater has been materialized.
#' @param adjudication_basis Transparent basis used to materialize a potential defeater.
#' @param claim_consequence Binding record-level constraint for the exact claim.
#' @param rationale Evidence-specific rationale.
#' @param varied_component Optional nonempty character vector naming the varied components.
#' @param held_constant Optional nonempty character vector naming what the comparison holds fixed.
#' @param same_estimand Whether the comparison retains the same estimand; `NA` or JSON `null` denotes missing metadata.
#' @param same_estimand_rationale Reason for the estimand classification, required when variation is documented.
#' @param invariance_claimed Whether the proposition requires invariance over this variation.
#'   `NA` or `NULL` means not recorded.
#' @param required_property Property that the exact proposition requires.
#' @param observation Diagnostic observation bearing on that property.
#' @param relevance_to_proposition Substantive explanation linking that observation to the required property.
#'
#' @return A `CSDGEvidenceRecord` list.
#' @export
csdg_evidence_record = function(
    gate_id,
    applicable,
    role,
    availability = if (applicable) "complete" else "not_applicable",
    result_direction = if (applicable) "descriptive" else "not_evaluated",
    criterion = NULL,
    criterion_source = NULL,
    criterion_rationale = NULL,
    materiality = if (role == "potential_defeater" && applicable) "not_materialized" else "not_applicable",
    adjudication_basis = NULL,
    claim_consequence = "none",
    rationale,
    varied_component = NULL,
    held_constant = NULL,
    same_estimand = NA,
    same_estimand_rationale = NULL,
    invariance_claimed = NA,
    required_property = NULL,
    observation = NULL,
    relevance_to_proposition = NULL) {
  assert_choice(gate_id, .csdg_gate_ids, .var.name = "gate_id")
  assert_flag(applicable, .var.name = "applicable")
  assert_choice(role, .csdg_evidence_roles, .var.name = "role")
  assert_choice(availability, .csdg_evidence_availability, .var.name = "availability")
  assert_choice(result_direction, .csdg_result_directions, .var.name = "result_direction")
  assert_choice(materiality, .csdg_materiality, .var.name = "materiality")
  assert_choice(claim_consequence, .csdg_claim_consequences, .var.name = "claim_consequence")
  assert_string(rationale, min.chars = 1L, .var.name = "rationale")
  if (!is.null(adjudication_basis)) {
    assert_choice(adjudication_basis, .csdg_adjudication_bases, .var.name = "adjudication_basis")
  }
  .validate_evidence_criterion(criterion, criterion_source, criterion_rationale)
  assert_character(varied_component, any.missing = FALSE, min.len = 1L, null.ok = TRUE,
    .var.name = "varied_component")
  assert_character(held_constant, any.missing = FALSE, min.len = 1L, null.ok = TRUE, .var.name = "held_constant")
  same_estimand = same_estimand %??% NA
  invariance_claimed = invariance_claimed %??% NA
  assert_logical(same_estimand, len = 1L, any.missing = TRUE, .var.name = "same_estimand")
  assert_logical(invariance_claimed, len = 1L, any.missing = TRUE, .var.name = "invariance_claimed")
  chain = list(
    required_property = required_property,
    observation = observation,
    relevance_to_proposition = relevance_to_proposition
  )
  for (name in names(chain)) {
    assert_string(chain[[name]], min.chars = 1L, null.ok = TRUE, .var.name = name)
  }
  assert_string(same_estimand_rationale, min.chars = 1L, null.ok = TRUE, .var.name = "same_estimand_rationale")
  variation_recorded = !is.null(varied_component) || !is.null(held_constant) ||
    !is.null(same_estimand_rationale) || !is.na(same_estimand) || !is.na(invariance_claimed)
  chain_complete = all(!vapply(chain, is.null, logical(1L)))
  chain_recorded = any(!vapply(chain, is.null, logical(1L)))
  if (chain_recorded && !chain_complete) {
    .csdg_stop("Supply the full required_property, observation, and relevance_to_proposition chain.")
  }
  if (variation_recorded && (is.null(varied_component) || is.null(held_constant) ||
      is.null(same_estimand_rationale))) {
    .csdg_stop("Variation requires varied_component, held_constant, and same_estimand_rationale.")
  }
  if (any(!nzchar(c(varied_component, held_constant)))) {
    .csdg_stop("Variation components cannot contain empty strings.")
  }
  constrains_claim = identical(materiality, "materialized") ||
    claim_consequence %in% c("revise_claim", "exact_claim_not_retained") ||
    (identical(role, "necessary_requirement") && identical(result_direction, "challenges"))
  if (variation_recorded && constrains_claim && !chain_complete) {
    .csdg_stop("Claim-constraining variation requires the property-observation-relevance chain.")
  }
  if (variation_recorded && constrains_claim && !isTRUE(same_estimand) && !isTRUE(invariance_claimed)) {
    .csdg_stop("A changed or uncertain estimand cannot constrain the claim without explicit claimed invariance.")
  }

  if (!applicable && !identical(availability, "not_applicable")) {
    .csdg_stop("A nonapplicable module must use `availability = \"not_applicable\"`.")
  }
  if (!applicable && !identical(result_direction, "not_evaluated")) {
    .csdg_stop("A nonapplicable module must use `result_direction = \"not_evaluated\"`.")
  }
  if (!applicable && !identical(materiality, "not_applicable")) {
    .csdg_stop("A nonapplicable module must use `materiality = \"not_applicable\"`.")
  }
  if (!applicable && !identical(claim_consequence, "none")) {
    .csdg_stop("A nonapplicable module cannot impose a claim consequence; use `claim_consequence = \"none\"`.")
  }
  if (applicable && identical(availability, "not_applicable")) {
    .csdg_stop("An applicable module cannot use `availability = \"not_applicable\"`.")
  }
  if (!identical(role, "potential_defeater") && !identical(materiality, "not_applicable")) {
    .csdg_stop("Only a `potential_defeater` can have a materiality state.")
  }
  if (identical(role, "potential_defeater") && applicable && identical(materiality, "not_applicable")) {
    .csdg_stop("An applicable potential defeater must be `materialized` or `not_materialized`.")
  }
  if (identical(materiality, "materialized") && is.null(adjudication_basis)) {
    .csdg_stop("A materialized potential defeater requires `adjudication_basis`.")
  }
  if (identical(materiality, "materialized") && !identical(availability, "complete")) {
    .csdg_stop("A materialized potential defeater requires complete evidence.")
  }
  if (identical(materiality, "materialized") && !result_direction %in% c("challenges", "mixed")) {
    .csdg_stop("A materialized potential defeater must challenge the claim or provide mixed evidence.")
  }
  if (identical(adjudication_basis, "prespecified_claim_specific_criterion") && is.null(criterion)) {
    .csdg_stop("Criterion-based adjudication requires a complete `criterion`.")
  }
  if (identical(role, "descriptive_context") &&
      !identical(claim_consequence, "none")) {
    .csdg_stop("Descriptive context cannot by itself retain, revise, reject, or leave a claim unresolved.")
  }
  if (identical(role, "graded_support") && identical(claim_consequence, "exact_claim_not_retained")) {
    .csdg_stop("Graded support cannot by itself reject a claim.")
  }
  if (identical(materiality, "materialized") &&
      !claim_consequence %in% c("revise_claim", "exact_claim_not_retained")) {
    .csdg_stop("A materialized potential defeater must revise the claim or prevent retention of the exact claim.")
  }
  if (identical(role, "potential_defeater") && identical(materiality, "not_materialized") &&
      claim_consequence %in% c("revise_claim", "exact_claim_not_retained")) {
    .csdg_stop("A nonmaterialized potential defeater cannot revise or reject the exact claim.")
  }
  if (identical(claim_consequence, "retain_exact_claim") &&
      (!applicable || !identical(availability, "complete") || !identical(result_direction, "supports") ||
        !role %in% c("necessary_requirement", "graded_support"))) {
    .csdg_stop(
      "`retain_exact_claim` requires complete supporting evidence with a necessary-requirement or graded-support role."
    )
  }
  if (identical(claim_consequence, "exact_claim_not_retained")) {
    coherent_rejection = applicable && identical(availability, "complete") &&
      ((identical(role, "necessary_requirement") && identical(result_direction, "challenges")) ||
        (identical(role, "potential_defeater") && identical(materiality, "materialized") &&
          result_direction %in% c("challenges", "mixed")))
    if (!coherent_rejection) {
      .csdg_stop(
        "`exact_claim_not_retained` requires a challenging necessary requirement or a materialized potential defeater."
      )
    }
  }
  if (identical(claim_consequence, "revise_claim")) {
    coherent_revision = applicable && identical(availability, "complete") &&
      ((identical(role, "necessary_requirement") && result_direction %in% c("challenges", "mixed")) ||
        (identical(role, "potential_defeater") && identical(materiality, "materialized") &&
          result_direction %in% c("challenges", "mixed")))
    if (!coherent_revision) {
      .csdg_stop("`revise_claim` requires challenging or mixed complete evidence with a claim-constraining role.")
    }
  }
  if (identical(claim_consequence, "unresolved") && identical(role, "necessary_requirement") &&
      identical(availability, "complete") && identical(result_direction, "challenges")) {
    .csdg_stop("A complete challenging necessary requirement cannot record an unresolved claim consequence.")
  }

  structure(
    list(
      gate_id = gate_id,
      applicable = applicable,
      role = role,
      availability = availability,
      result_direction = result_direction,
      criterion = criterion,
      criterion_source = criterion_source,
      criterion_rationale = criterion_rationale,
      materiality = materiality,
      adjudication_basis = adjudication_basis,
      claim_consequence = claim_consequence,
      rationale = rationale,
      varied_component = varied_component,
      held_constant = held_constant,
      same_estimand = same_estimand,
      same_estimand_rationale = same_estimand_rationale,
      invariance_claimed = invariance_claimed,
      required_property = required_property,
      observation = observation,
      relevance_to_proposition = relevance_to_proposition,
      variation_status = if (variation_recorded) "documented_not_verified" else "not_recorded",
      proposition_linkage = if (chain_complete) "documented_not_verified" else "not_recorded"
    ),
    class = c("CSDGEvidenceRecord", "list")
  )
}

#' Adjudicate one exact claim without compensation or scoring
#'
#' @description
#' Applies CSDG's conditional non-compensation rule to orthogonal evidence records.
#' An unmet necessary requirement or a materialized challenging defeater prevents retention of the exact claim.
#' Graded support and descriptive context never compensate for either condition and never form an aggregate score.
#' Claim applicability is specified independently of module applicability.
#' For compatibility, an omitted `claim_applicable` means `TRUE` and is recorded as `"legacy_default"`.
#' An in-scope claim with no applicable supplied necessary evidence remains `"unresolved"`.
#' Only an explicitly out-of-scope claim with a rationale can return `"not_applicable"`.
#'
#' Validated record consequences are binding constraints, with precedence `"not_met"`, `"unresolved"`, then `"met"`.
#' A justified `"revise_claim"` or `"exact_claim_not_retained"` consequence blocks the unchanged claim.
#' An explicit `"unresolved"` consequence prevents `"met"`, even when the record supports a necessary property.
#' `"retain_exact_claim"` expresses favorable record-level evidence and cannot override another record's constraint.
#' `"none"` imposes no additional constraint but does not suppress the necessary-requirement or defeater rules.
#' Contradictory consequences within a record are rejected rather than resolved by precedence.
#' Across records, a genuine blocker takes precedence over uncertainty and support, regardless of order.
#'
#' `"met"` is conditional on the supplied applicable necessary requirements, not a completeness certificate.
#' The caller remains responsible for identifying every requirement relevant to the proposition.
#' The returned linkage status distinguishes documented chains from legacy records with missing linkage metadata.
#' Neither status independently validates the observations, their relevance, or the proposition.
#'
#' @param evidence A list of `CSDGEvidenceRecord` objects; an empty list supplies no evidence.
#' @param claim_applicable Whether the proposition itself is within the declared evaluation scope.
#' @param applicability_rationale Optional rationale for the claim-level scope decision; required when out of scope.
#'
#' @return A `CSDGClaimAdjudication` list with `decision`, `claim_applicable`, `applicability_source`,
#'   `applicability_rationale`, `blocking_gate_ids`, `unresolved_gate_ids`, `proposition_linkage`, and `rationale`.
#' @examples
#' supporting = csdg_evidence_record(
#'   "G5", TRUE, "necessary_requirement", result_direction = "supports",
#'   claim_consequence = "retain_exact_claim", rationale = "The declared ordering occurs in the supplied evidence."
#' )
#' csdg_adjudicate_claim(list(supporting), claim_applicable = TRUE)
#' csdg_adjudicate_claim(list(), claim_applicable = TRUE)
#' csdg_adjudicate_claim(
#'   list(), claim_applicable = FALSE, applicability_rationale = "This proposition is outside the declared evaluation."
#' )
#' @export
csdg_adjudicate_claim = function(evidence = list(), claim_applicable = TRUE, applicability_rationale = NULL) {
  applicability_source = if (missing(claim_applicable)) "legacy_default" else "explicit"
  assert_list(evidence, .var.name = "evidence")
  assert_flag(claim_applicable, .var.name = "claim_applicable")
  assert_string(applicability_rationale, min.chars = 1L, null.ok = claim_applicable,
    .var.name = "applicability_rationale")
  applicability_rationale = applicability_rationale %??% if (applicability_source == "legacy_default") {
    "The backward-compatible default treats the proposition as in scope; module applicability does not decide scope."
  } else {
    "The caller explicitly designated the proposition as in scope."
  }
  if (any(!vapply(evidence, inherits, logical(1L), what = "CSDGEvidenceRecord"))) {
    .csdg_stop("Every element of `evidence` must be a CSDGEvidenceRecord.")
  }
  evidence = lapply(evidence, function(record) {
    required = c("gate_id", "applicable", "role", "availability", "result_direction", "criterion",
      "criterion_source", "criterion_rationale", "materiality", "adjudication_basis", "claim_consequence", "rationale")
    missing_fields = setdiff(required, names(record))
    if (length(missing_fields)) {
      .csdg_stop("Evidence record is missing required field(s): %s.", paste(missing_fields, collapse = ", "))
    }
    allowed = c(names(formals(csdg_evidence_record)), "variation_status", "proposition_linkage")
    if (any(!names(record) %in% allowed)) {
      .csdg_stop("Evidence record contains unsupported field(s): %s.",
        paste(setdiff(names(record), allowed), collapse = ", "))
    }
    do.call(csdg_evidence_record, record[intersect(names(record), names(formals(csdg_evidence_record)))])
  })
  linkage = if (length(evidence) && all(vapply(evidence, function(record) {
    identical(record$proposition_linkage, "documented_not_verified")
  }, logical(1L)))) "documented_not_verified" else "not_recorded_for_all_evidence"

  applicable = vapply(evidence, function(record) isTRUE(record$applicable), logical(1L))
  if (!claim_applicable && any(applicable)) {
    .csdg_stop("An out-of-scope claim cannot have supplied modules marked applicable to that claim.")
  }
  if (!claim_applicable) {
    return(structure(
      list(
        decision = "not_applicable",
        claim_applicable = FALSE,
        applicability_source = applicability_source,
        applicability_rationale = applicability_rationale,
        blocking_gate_ids = character(),
        unresolved_gate_ids = character(),
        proposition_linkage = linkage,
        rationale = "The proposition was explicitly placed outside the declared evaluation scope."
      ),
      class = c("CSDGClaimAdjudication", "list")
    ))
  }

  is_blocking = vapply(evidence, function(record) {
    if (!isTRUE(record$applicable)) return(FALSE)
    necessary_challenge = identical(record$role, "necessary_requirement") &&
      identical(record$availability, "complete") &&
      identical(record$result_direction, "challenges")
    materialized_challenge = identical(record$role, "potential_defeater") &&
      identical(record$materiality, "materialized") &&
      record$result_direction %in% c("challenges", "mixed")
    binding_revision = record$claim_consequence %in% c("revise_claim", "exact_claim_not_retained")
    necessary_challenge || materialized_challenge || binding_revision
  }, logical(1L))
  is_unresolved = vapply(evidence, function(record) {
    if (!isTRUE(record$applicable)) return(FALSE)
    if (record$claim_consequence %in% c("revise_claim", "exact_claim_not_retained")) return(FALSE)
    necessary_incomplete = identical(record$role, "necessary_requirement") &&
      (record$availability %in% c("incomplete", "unavailable") ||
        record$result_direction %in% c("mixed", "descriptive", "not_evaluated"))
    binding_unresolved = identical(record$claim_consequence, "unresolved")
    necessary_incomplete || binding_unresolved
  }, logical(1L))
  has_necessary = any(vapply(evidence, function(record) {
    isTRUE(record$applicable) && identical(record$role, "necessary_requirement")
  }, logical(1L)))

  decision = if (any(is_blocking)) {
    "not_met"
  } else if (!has_necessary) {
    "unresolved"
  } else if (any(is_unresolved)) {
    "unresolved"
  } else {
    "met"
  }
  structure(
    list(
      decision = decision,
      claim_applicable = TRUE,
      applicability_source = applicability_source,
      applicability_rationale = applicability_rationale,
      proposition_linkage = linkage,
      blocking_gate_ids = unique(vapply(evidence[is_blocking], `[[`, character(1L), "gate_id")),
      unresolved_gate_ids = unique(vapply(evidence[is_unresolved], `[[`, character(1L), "gate_id")),
      rationale = switch(
        decision,
        not_met = paste(
          "The exact claim is not retained because a necessary requirement was unmet, a claim-specific defeater",
          "was materialized, or a binding revision was recorded. Favorable evidence elsewhere does not compensate."
        ),
        unresolved = if (!has_necessary) {
          "The exact claim remains unresolved because no applicable necessary requirement was supplied."
        } else {
          paste("The exact claim remains unresolved because required evidence is incomplete or a binding unresolved",
            "consequence was recorded.")
        },
        met = paste(
          "All supplied applicable necessary requirements support the exact claim, and no challenging potential",
          "defeater or unresolved consequence remains. This is conditional on the supplied requirements, not a",
          "certificate that every relevant requirement was supplied. Graded and descriptive evidence were not scored."
        )
      )
    ),
    class = c("CSDGClaimAdjudication", "list")
  )
}
