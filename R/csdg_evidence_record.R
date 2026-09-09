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
#' @param claim_consequence Recorded consequence for the exact claim.
#' @param rationale Evidence-specific rationale.
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
    rationale) {
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

  if (!applicable && !identical(availability, "not_applicable")) {
    .csdg_stop("A nonapplicable module must use `availability = \"not_applicable\"`.")
  }
  if (!applicable && !identical(result_direction, "not_evaluated")) {
    .csdg_stop("A nonapplicable module must use `result_direction = \"not_evaluated\"`.")
  }
  if (!applicable && !identical(materiality, "not_applicable")) {
    .csdg_stop("A nonapplicable module must use `materiality = \"not_applicable\"`.")
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
      rationale = rationale
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
#'
#' @param evidence A nonempty list of `CSDGEvidenceRecord` objects.
#'
#' @return A `CSDGClaimAdjudication` list with `decision`, `blocking_gate_ids`, `unresolved_gate_ids`, and `rationale`.
#' @export
csdg_adjudicate_claim = function(evidence) {
  assert_list(evidence, min.len = 1L, .var.name = "evidence")
  if (any(!vapply(evidence, inherits, logical(1L), what = "CSDGEvidenceRecord"))) {
    .csdg_stop("Every element of `evidence` must be a CSDGEvidenceRecord.")
  }

  applicable = vapply(evidence, function(record) isTRUE(record$applicable), logical(1L))
  if (!any(applicable)) {
    return(structure(
      list(
        decision = "not_applicable",
        blocking_gate_ids = character(),
        unresolved_gate_ids = character(),
        rationale = "No supplied evidence module was applicable to the exact claim."
      ),
      class = c("CSDGClaimAdjudication", "list")
    ))
  }

  is_blocking = vapply(evidence, function(record) {
    necessary_challenge = identical(record$role, "necessary_requirement") &&
      identical(record$availability, "complete") &&
      identical(record$result_direction, "challenges")
    materialized_challenge = identical(record$role, "potential_defeater") &&
      identical(record$materiality, "materialized") &&
      record$result_direction %in% c("challenges", "mixed")
    necessary_challenge || materialized_challenge
  }, logical(1L))
  is_unresolved = vapply(evidence, function(record) {
    if (!isTRUE(record$applicable)) return(FALSE)
    necessary_incomplete = identical(record$role, "necessary_requirement") &&
      (record$availability %in% c("incomplete", "unavailable") ||
        record$result_direction %in% c("mixed", "descriptive", "not_evaluated"))
    materialized_incomplete = identical(record$role, "potential_defeater") &&
      identical(record$materiality, "materialized") &&
      record$availability %in% c("incomplete", "unavailable")
    necessary_incomplete || materialized_incomplete
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
      blocking_gate_ids = unique(vapply(evidence[is_blocking], `[[`, character(1L), "gate_id")),
      unresolved_gate_ids = unique(vapply(evidence[is_unresolved], `[[`, character(1L), "gate_id")),
      rationale = switch(
        decision,
        not_met = paste(
          "The exact claim is not retained because at least one necessary requirement was unmet or a",
          "claim-specific potential defeater was materialized. Favorable evidence elsewhere does not compensate."
        ),
        unresolved = if (!has_necessary) {
          "The exact claim remains unresolved because no applicable necessary requirement was supplied."
        } else {
          "The exact claim remains unresolved because required claim-scoped evidence is incomplete."
        },
        met = paste(
          "All supplied applicable necessary requirements support the exact claim, and no challenging potential",
          "defeater was materialized. Graded support and descriptive context were not scored."
        )
      )
    ),
    class = c("CSDGClaimAdjudication", "list")
  )
}
