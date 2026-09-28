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
# Property status implied by a legacy claim consequence.
.csdg_consequence_statuses = c(
  retain_exact_claim = "supported",
  revise_claim = "contradicted",
  exact_claim_not_retained = "contradicted",
  unresolved = "open"
)
.csdg_legacy_evidence_roles = c("necessary_requirement", "potential_defeater", "graded_support", "descriptive_context")

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

#' Record evidence on a required property, counterevidence, an unresolved threat, or context
#'
#' @description
#' Creates one entry of a claim record (Step 4 of the article).
#' Each entry has one of four roles:
#' * `"required_property"`: a property that the claim requires, derived from a gate (see [csdg_gate_plan()]).
#'   Judged against its criterion, it has the `status` `"supported"`, `"contradicted"`, or `"open"`.
#'   It is contradicted if the evidence shows that it does not hold; it is open if the needed evidence is absent
#'   (for example, no criterion or no causal design), is inconclusive under the criterion, or faces an unresolved
#'   threat.
#' * `"established_counterevidence"`: a demonstrated problem, such as a coding error or leakage shown to affect the
#'   result. It bears on the required property named by `gate_id` and sets that property to contradicted.
#' * `"unresolved_threat"`: a specific and plausible but unquantified problem, such as predictor selection on the same
#'   data outside the cross-validation. It sets the threatened property (`gate_id`) to open unless that property is
#'   contradicted.
#' * `"context"`: everything else. It is reported but never changes the assessment.
#'
#' For each required property, the record states the property, the criterion that decides whether it holds, the
#' observation, and its relevance (`required_property`, `criterion`, `observation`, `relevance_to_proposition`).
#' For repeated or alternative analyses, it also states what varied, what was held fixed, and whether the quantity
#' stayed the same; a variation that contradicts a property with a changed or uncertain quantity must state that the
#' claim asserts invariance over it.
#' The package records these entries and applies the decision rule in [csdg_adjudicate_claim()]; the researcher sets
#' the criteria and states the relevance of each observation.
#' A documented entry is not independently validated merely because its fields are complete.
#'
#' Versions up to 0.1.5 used the roles `"necessary_requirement"`, `"potential_defeater"`, `"graded_support"`, and
#' `"descriptive_context"` and a `claim_consequence`. These are still accepted with a deprecation warning:
#' `"necessary_requirement"` becomes a required property; a `"potential_defeater"` with `materiality =
#' "materialized"` becomes established counterevidence, one that is not materialized but records the consequence
#' `"unresolved"` becomes an unresolved threat, and any other becomes context; `"graded_support"` and
#' `"descriptive_context"` become context. A consequence on a context entry is ignored with a warning; it no longer
#' changes the assessment (in 0.1.5, an `"unresolved"` consequence on a `"graded_support"` record left the claim
#' unresolved). See `inst/MIGRATION_0_1_6.md`.
#'
#' @param gate_id Gate of the required property: one of the 12 gate identifiers, or `"CD"` for the causal design that
#'   a causal claim requires (not a gate). For counterevidence and threats, the property they bear on.
#' @param applicable Whether the gate applies to the claim. A nonapplicable entry is reported as context.
#' @param role One of `"required_property"`, `"established_counterevidence"`, `"unresolved_threat"`, or `"context"`.
#' @param availability Whether the evidence is `"complete"`, `"incomplete"`, `"unavailable"` (for example, after a
#'   computation error), or `"not_applicable"`. Defaults to `"complete"` (`"incomplete"` for an unresolved threat;
#'   `"not_applicable"` if `applicable = FALSE`).
#' @param result_direction Descriptive direction of the observation: `"supports"`, `"challenges"`, `"mixed"`,
#'   `"descriptive"`, or `"not_evaluated"`. Defaults follow `status` (supported: supports; contradicted:
#'   challenges; open: not evaluated); it must agree with a supplied `status`.
#' @param criterion Optional named list with `value` and `direction` (`"minimum"`, `"maximum"`, `"range"`, or
#'   `"qualitative"`).
#' @param criterion_source Source of a supplied criterion.
#' @param criterion_rationale Rationale linking a supplied criterion to the claim and its use.
#' @param materiality Deprecated documentary field of versions up to 0.1.5 (`"materialized"` for counterevidence);
#'   default `"not_applicable"`.
#' @param adjudication_basis Optional documentary basis for established counterevidence.
#' @param claim_consequence Deprecated (0.1.5). If given together with `status`, it must agree with it; on an entry
#'   that is not a required property it is ignored.
#' @param rationale Rationale of the entry.
#' @param varied_component Optional nonempty character vector naming what varied.
#' @param held_constant Optional nonempty character vector naming what was held fixed.
#' @param same_estimand Whether the comparison keeps the same quantity; `NA` or JSON `null` means not recorded.
#' @param same_estimand_rationale Reason for that classification, required when variation is documented.
#' @param invariance_claimed Whether the claim asserts invariance over this variation. `NA` or `NULL` means not
#'   recorded.
#' @param required_property The property, in the words of the claim record.
#' @param observation The observation bearing on the property (for a threat: the plausible problem).
#' @param relevance_to_proposition Why the observation bears on the property: what varied, what was held fixed, and
#'   why this matches the scope.
#' @param status Status of a required property: `"supported"`, `"contradicted"`, or `"open"`. If `NULL`, it is
#'   derived from `availability` and `result_direction` (complete and supporting: supported; complete and
#'   challenging: contradicted; otherwise open). Only a required property has a status.
#'
#' @return A `CSDGEvidenceRecord` list. It stores `role`, `status`, the observation fields, and, for an entry created
#'   with a legacy role, `legacy_role`.
#' @examples
#' csdg_evidence_record(
#'   "G6a", TRUE, "required_property", status = "contradicted",
#'   criterion = list(value = "strict ordering in both models", direction = "qualitative"),
#'   criterion_source = "translated from the claim",
#'   criterion_rationale = "The claim asserts the ordering in each model.",
#'   rationale = "Model B reverses the ordering.",
#'   required_property = "Item 1 has the larger marginal PFI in A and in B.",
#'   observation = "PFI of item 1 versus item 2: 2 versus 0 in A, 0 versus 2 in B.",
#'   relevance_to_proposition = "Only the model differs, and the claim covers both models."
#' )
#' csdg_evidence_record(
#'   "G1", TRUE, "unresolved_threat",
#'   rationale = "The optimism was not quantified; a nested analysis would resolve it.",
#'   required_property = "Held-out performance is not inflated by predictor selection.",
#'   observation = "Predictors were selected on the same data outside the cross-validation."
#' )
#' @export
csdg_evidence_record = function(
    gate_id,
    applicable,
    role,
    availability = NULL,
    result_direction = NULL,
    criterion = NULL,
    criterion_source = NULL,
    criterion_rationale = NULL,
    materiality = NULL,
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
    relevance_to_proposition = NULL,
    status = NULL) {
  .csdg_evidence_record_impl(
    gate_id = gate_id, applicable = applicable, role = role, availability = availability,
    result_direction = result_direction, criterion = criterion, criterion_source = criterion_source,
    criterion_rationale = criterion_rationale, materiality = materiality, adjudication_basis = adjudication_basis,
    claim_consequence = claim_consequence, rationale = rationale, varied_component = varied_component,
    held_constant = held_constant, same_estimand = same_estimand, same_estimand_rationale = same_estimand_rationale,
    invariance_claimed = invariance_claimed, required_property = required_property, observation = observation,
    relevance_to_proposition = relevance_to_proposition, status = status
  )
}

.csdg_evidence_record_impl = function(
    gate_id,
    applicable,
    role,
    availability = NULL,
    result_direction = NULL,
    criterion = NULL,
    criterion_source = NULL,
    criterion_rationale = NULL,
    materiality = NULL,
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
    relevance_to_proposition = NULL,
    status = NULL,
    legacy_role = NULL,
    warn = TRUE) {
  assert_choice(gate_id, .csdg_requirement_ids, .var.name = "gate_id")
  assert_flag(applicable, .var.name = "applicable")
  assert_string(role, min.chars = 1L, .var.name = "role")
  assert_string(rationale, min.chars = 1L, .var.name = "rationale")
  claim_consequence = claim_consequence %||% "none"
  assert_choice(claim_consequence, .csdg_claim_consequences, .var.name = "claim_consequence")
  if (!is.null(materiality)) assert_choice(materiality, .csdg_materiality, .var.name = "materiality")
  if (!is.null(status)) assert_choice(status, .csdg_property_statuses, .var.name = "status")
  if (!is.null(legacy_role)) assert_choice(legacy_role, .csdg_legacy_evidence_roles, .var.name = "legacy_role")

  # Map the roles of versions up to 0.1.5.
  if (role %in% .csdg_legacy_evidence_roles) {
    legacy_role = role
    if (identical(role, "potential_defeater") && applicable && is.null(materiality)) {
      materiality = "not_materialized"
    }
    role = switch(
      role,
      necessary_requirement = "required_property",
      potential_defeater = if (identical(materiality, "materialized")) {
        "established_counterevidence"
      } else if (applicable && identical(claim_consequence, "unresolved")) {
        "unresolved_threat"
      } else {
        "context"
      },
      "context"
    )
    if (warn) .csdg_deprecate(legacy_role, role, "Evidence role")
  }
  assert_choice(role, .csdg_evidence_roles, .var.name = "role")
  legacy = !is.null(legacy_role)
  if (warn && !identical(claim_consequence, "none")) {
    .csdg_deprecate(claim_consequence, "status", "claim_consequence")
  }
  materiality = materiality %||% "not_applicable"

  availability = availability %||% if (!applicable) {
    "not_applicable"
  } else if (identical(role, "unresolved_threat")) {
    "incomplete"
  } else {
    "complete"
  }
  result_direction = result_direction %||% if (!applicable) {
    "not_evaluated"
  } else if (!is.null(status)) {
    switch(status, supported = "supports", contradicted = "challenges", "not_evaluated")
  } else {
    switch(role,
      established_counterevidence = "challenges",
      unresolved_threat = "not_evaluated",
      "descriptive"
    )
  }
  assert_choice(availability, .csdg_evidence_availability, .var.name = "availability")
  assert_choice(result_direction, .csdg_result_directions, .var.name = "result_direction")
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
  if (chain_recorded && !chain_complete && !identical(role, "unresolved_threat")) {
    .csdg_stop("Supply the full required_property, observation, and relevance_to_proposition chain.")
  }
  if (variation_recorded && (is.null(varied_component) || is.null(held_constant) ||
      is.null(same_estimand_rationale))) {
    .csdg_stop("Variation requires varied_component, held_constant, and same_estimand_rationale.")
  }
  if (any(!nzchar(c(varied_component, held_constant)))) {
    .csdg_stop("Variation components cannot contain empty strings.")
  }

  # Nonapplicable entries are context for the claim.
  if (!applicable) {
    if (!identical(availability, "not_applicable")) {
      .csdg_stop("A nonapplicable gate must use `availability = \"not_applicable\"`.")
    }
    if (!identical(result_direction, "not_evaluated")) {
      .csdg_stop("A nonapplicable gate must use `result_direction = \"not_evaluated\"`.")
    }
    if (!identical(materiality, "not_applicable")) {
      .csdg_stop("A nonapplicable gate must use `materiality = \"not_applicable\"`.")
    }
    if (!identical(claim_consequence, "none")) {
      .csdg_stop("A nonapplicable gate cannot change the assessment; use `claim_consequence = \"none\"`.")
    }
    if (!is.null(status)) {
      .csdg_stop("A nonapplicable gate has no property status; omit `status`.")
    }
  } else if (identical(availability, "not_applicable")) {
    .csdg_stop("An applicable gate cannot use `availability = \"not_applicable\"`.")
  }
  if (identical(adjudication_basis, "prespecified_claim_specific_criterion") && is.null(criterion)) {
    .csdg_stop("Criterion-based adjudication requires a complete `criterion`.")
  }
  if (!is.null(status) && !identical(role, "required_property")) {
    .csdg_stop("Only a required property has a `status`; counterevidence, threats, and context do not.")
  }
  if (identical(materiality, "materialized") && !identical(role, "established_counterevidence")) {
    .csdg_stop("Only established counterevidence can be `materialized`.")
  }

  if (identical(role, "required_property") && applicable) {
    if (!identical(materiality, "not_applicable")) {
      .csdg_stop("A required property has no materiality state; use `materiality = \"not_applicable\"`.")
    }
    status = .csdg_required_property_status(status, availability, result_direction, claim_consequence)
  } else if (identical(role, "established_counterevidence")) {
    if (!applicable) .csdg_stop("Established counterevidence must be applicable to the claim.")
    if (!identical(availability, "complete")) {
      .csdg_stop("Established counterevidence requires complete evidence.")
    }
    if (!result_direction %in% c("challenges", "mixed")) {
      .csdg_stop("Established counterevidence must challenge the claim or provide mixed evidence.")
    }
    if (!claim_consequence %in% c("none", "revise_claim", "exact_claim_not_retained")) {
      .csdg_stop("Established counterevidence contradicts a required property; it cannot retain the claim.")
    }
    if (legacy) {
      if (is.null(adjudication_basis)) {
        .csdg_stop("A materialized potential defeater requires `adjudication_basis`.")
      }
      if (identical(claim_consequence, "none")) {
        .csdg_stop("A materialized potential defeater must revise the claim or prevent retention of the exact claim.")
      }
    } else if (!chain_complete) {
      .csdg_stop(
        "Established counterevidence requires required_property, observation, and relevance_to_proposition."
      )
    }
  } else if (identical(role, "unresolved_threat")) {
    if (!applicable) .csdg_stop("An unresolved threat must be applicable to the claim.")
    if (identical(result_direction, "supports")) {
      .csdg_stop("An unresolved threat cannot support the claim.")
    }
    if (!claim_consequence %in% c("none", "unresolved")) {
      .csdg_stop("An unresolved threat leaves its property open; it cannot revise or retain the claim.")
    }
    if (!legacy && (is.null(required_property) || is.null(observation))) {
      .csdg_stop("An unresolved threat requires `required_property` (the threatened property) and `observation`.")
    }
  } else if (identical(role, "context") && !identical(claim_consequence, "none")) {
    ignorable = identical(legacy_role, "graded_support") &&
      claim_consequence %in% c("retain_exact_claim", "unresolved")
    if (!ignorable) {
      if (identical(legacy_role, "potential_defeater")) {
        .csdg_stop("A nonmaterialized potential defeater cannot revise or reject the exact claim.")
      }
      .csdg_stop("Context never changes the assessment; use `claim_consequence = \"none\"`.")
    }
    if (warn) {
      .csdg_warn_once(
        "context consequence",
        "claim_consequence is ignored for context and no longer changes the assessment (mlr3autoiml 0.1.6)."
      )
    }
  }

  constrains_claim = identical(status, "contradicted") || identical(role, "established_counterevidence")
  if (variation_recorded && constrains_claim && !chain_complete) {
    .csdg_stop("Claim-constraining variation requires the property-observation-relevance chain.")
  }
  if (variation_recorded && constrains_claim && !isTRUE(same_estimand) && !isTRUE(invariance_claimed)) {
    .csdg_stop("A changed or uncertain quantity cannot contradict the claim without explicit claimed invariance.")
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
      status = if (identical(role, "required_property") && applicable) status else NULL,
      legacy_role = legacy_role,
      variation_status = if (variation_recorded) "documented_not_verified" else "not_recorded",
      proposition_linkage = if (chain_complete) "documented_not_verified" else "not_recorded"
    ),
    class = c("CSDGEvidenceRecord", "list")
  )
}

.csdg_required_property_status = function(status, availability, result_direction, claim_consequence) {
  implied = .csdg_consequence_statuses[claim_consequence]
  if (!is.null(status)) {
    if (!identical(claim_consequence, "none") && !identical(unname(implied), status)) {
      .csdg_stop("`claim_consequence = \"%s\"` disagrees with `status = \"%s\"`.", claim_consequence, status)
    }
    if (identical(status, "supported") &&
        (!identical(availability, "complete") || !identical(result_direction, "supports"))) {
      .csdg_stop("A supported property requires complete evidence with `result_direction = \"supports\"`.")
    }
    if (identical(status, "contradicted") &&
        (!identical(availability, "complete") || !result_direction %in% c("challenges", "mixed"))) {
      .csdg_stop("A contradicted property requires complete evidence that challenges it (or mixed evidence).")
    }
    return(status)
  }
  complete = identical(availability, "complete")
  switch(
    claim_consequence,
    retain_exact_claim = {
      if (!complete || !identical(result_direction, "supports")) {
        .csdg_stop("`retain_exact_claim` requires complete supporting evidence on a required property.")
      }
      "supported"
    },
    exact_claim_not_retained = {
      if (!complete || !identical(result_direction, "challenges")) {
        .csdg_stop("`exact_claim_not_retained` requires complete evidence that challenges a required property.")
      }
      "contradicted"
    },
    revise_claim = {
      if (!complete || !result_direction %in% c("challenges", "mixed")) {
        .csdg_stop("`revise_claim` requires complete challenging or mixed evidence on a required property.")
      }
      "contradicted"
    },
    unresolved = {
      if (complete && identical(result_direction, "challenges")) {
        .csdg_stop("A complete challenging required property cannot record an unresolved claim consequence.")
      }
      "open"
    },
    if (complete && identical(result_direction, "supports")) {
      "supported"
    } else if (complete && identical(result_direction, "challenges")) {
      "contradicted"
    } else {
      "open"
    }
  )
}

#' Apply the decision rule to one claim
#'
#' @description
#' Applies the fixed decision rule of the article (Step 5) to the entries of one claim record:
#' a claim is `"not_met"` if at least one required property is contradicted, including by established
#' counterevidence; otherwise it is `"unresolved"` if at least one required property is open, including through an
#' unresolved threat, or if no required property was supplied; otherwise it is `"met"`.
#' Favorable results never offset a contradicted property, and context never changes the assessment.
#' A met claim is retained; a claim that is not met or unresolved is revised or withheld (`decision_options`).
#'
#' If `plan` is supplied, every gate that the plan requires but that has no entry is an open property ("no evidence
#' supplied"), and a causal claim without an entry for the causal design (`gate_id = "CD"`) is open as well.
#' An unresolved threat to a gate that the plan does not require is reported as context, with a warning.
#' Established counterevidence on such a gate still contradicts it (a demonstrated problem is never discarded), with a
#' warning to attach it to the required property it affects.
#' Without a plan, a threat to a property that has no required-property entry adds that property as open.
#'
#' `"met"` holds relative to the listed required properties; it is not a certificate that every relevant property
#' was identified. The label `"not_applicable"` is reserved for a claim that the researcher explicitly places outside
#' the evaluation, with a rationale (`claim_applicable = FALSE`). For compatibility, an omitted `claim_applicable`
#' means `TRUE` and is recorded as `"legacy_default"`.
#'
#' @param evidence A list of `CSDGEvidenceRecord` objects; an empty list supplies no evidence.
#' @param claim_applicable Whether the claim itself is within the evaluation.
#' @param applicability_rationale Optional rationale for that decision; required when the claim is out of scope.
#' @param plan Optional `CSDGGatePlan` from [csdg_gate_plan()] (or any table with columns `gate_id` and
#'   `required`), which adds the plan's required gates that have no entry as open properties.
#'
#' @return A `CSDGClaimAdjudication` list with the `assessment` (`"met"`, `"not_met"`, `"unresolved"`, or
#'   `"not_applicable"`), `decision_options` (the decisions that the assessment permits: `"retain"`, or `"revise"`
#'   and `"withhold"`; the researcher takes the decision), the
#'   table `properties` (gate, status, source, rationale), the gate identifiers of contradicted and open properties,
#'   of counterevidence, threats, and context, `claim_applicable`, `applicability_source`,
#'   `applicability_rationale`, `proposition_linkage`, and `rationale`. `blocking_gate_ids` and
#'   `unresolved_gate_ids` are kept as aliases of the contradicted and open gate identifiers.
#'   `decision` is a deprecated alias of `assessment` (versions up to 0.1.5 stored the assessment under this name);
#'   it will be removed in a later release. Use `assessment` for met, not met, or unresolved, and `decision_options`
#'   for retain, revise, or withhold.
#' @examples
#' supported = csdg_evidence_record(
#'   "G5", TRUE, "required_property", status = "supported",
#'   rationale = "The ordering recurs in every repetition."
#' )
#' csdg_adjudicate_claim(list(supported), claim_applicable = TRUE)
#' csdg_adjudicate_claim(list(), claim_applicable = TRUE)
#' csdg_adjudicate_claim(
#'   list(), claim_applicable = FALSE, applicability_rationale = "This claim is outside the evaluation."
#' )
#' @export
csdg_adjudicate_claim = function(evidence = list(), claim_applicable = TRUE, applicability_rationale = NULL,
                                 plan = NULL) {
  applicability_source = if (missing(claim_applicable)) "legacy_default" else "explicit"
  assert_list(evidence, .var.name = "evidence")
  assert_flag(claim_applicable, .var.name = "claim_applicable")
  assert_string(applicability_rationale, min.chars = 1L, null.ok = claim_applicable,
    .var.name = "applicability_rationale")
  applicability_rationale = applicability_rationale %??% if (applicability_source == "legacy_default") {
    "The backward-compatible default treats the claim as in scope; gate applicability does not decide scope."
  } else {
    "The caller explicitly designated the claim as in scope."
  }
  if (!is.null(plan)) {
    assert_data_frame(plan, .var.name = "plan")
    if (!all(c("gate_id", "required") %in% names(plan))) {
      .csdg_stop("`plan` must contain the columns `gate_id` and `required`.")
    }
  }
  if (any(!vapply(evidence, inherits, logical(1L), what = "CSDGEvidenceRecord"))) {
    .csdg_stop("Every element of `evidence` must be a CSDGEvidenceRecord.")
  }
  evidence = lapply(evidence, .csdg_revalidate_evidence_record)
  linkage = if (length(evidence) && all(vapply(evidence, function(record) {
    identical(record$proposition_linkage, "documented_not_verified")
  }, logical(1L)))) "documented_not_verified" else "not_recorded_for_all_evidence"

  applicable = vapply(evidence, function(record) isTRUE(record$applicable), logical(1L))
  if (!claim_applicable && any(applicable)) {
    .csdg_stop("An out-of-scope claim cannot have supplied gates marked applicable to that claim.")
  }
  empty_ids = character()
  if (!claim_applicable) {
    return(.csdg_adjudication_result(
      decision = "not_applicable",
      applicability_source = applicability_source,
      applicability_rationale = applicability_rationale,
      linkage = linkage,
      properties = .csdg_property_table(list()),
      counterevidence = empty_ids, threats = empty_ids,
      context = unique(vapply(evidence, `[[`, character(1L), "gate_id")),
      claim_applicable = FALSE,
      rationale = "The claim was explicitly placed outside the evaluation."
    ))
  }

  role_of = function(record) if (isTRUE(record$applicable)) record$role else "context"
  roles = vapply(evidence, role_of, character(1L))
  properties = list()
  set_property = function(gate_id, status, source, rationale) {
    properties[[gate_id]] <<- list(status = status, source = source, rationale = rationale)
  }
  rank = c(contradicted = 3L, open = 2L, supported = 1L)
  for (record in evidence[roles == "required_property"]) {
    current = properties[[record$gate_id]]
    if (is.null(current) || rank[[record$status]] > rank[[current$status]]) {
      set_property(record$gate_id, record$status, "record", record$rationale)
    }
  }
  plan_required = character()
  plan_gates = character()
  causal_required = FALSE
  if (!is.null(plan)) {
    plan_table = as.data.frame(plan)
    plan_gates = as.character(plan_table$gate_id)
    plan_required = plan_gates[as.logical(plan_table$required) %in% TRUE]
    causal_required = isTRUE(attr(plan, "causal_design_required"))
    if (causal_required) {
      plan_required = c(plan_required, "CD")
      plan_gates = c(plan_gates, "CD")
    }
    for (gate_id in setdiff(plan_required, names(properties))) {
      set_property(gate_id, "open", "plan", if (identical(gate_id, "CD")) {
        "A causal claim requires a causal design; none was supplied."
      } else {
        "No evidence was supplied for this required property."
      })
    }
  }
  counterevidence = character()
  for (record in evidence[roles == "established_counterevidence"]) {
    if (!is.null(plan) && !record$gate_id %in% plan_required) {
      .csdg_warn(paste(
        "The established counterevidence bears on %s, which the plan does not require; it is added as a",
        "contradicted required property. Attach it to the required property it affects if that is a different gate."
      ), record$gate_id)
    }
    counterevidence = c(counterevidence, record$gate_id)
    set_property(record$gate_id, "contradicted", "counterevidence", record$rationale)
  }
  threats = character()
  context = vapply(evidence[roles == "context"], `[[`, character(1L), "gate_id")
  for (record in evidence[roles == "unresolved_threat"]) {
    gate_id = record$gate_id
    required = !is.null(properties[[gate_id]])
    if (!required && !is.null(plan) && (!gate_id %in% plan_gates || !gate_id %in% plan_required)) {
      .csdg_warn(
        "The unresolved threat to %s is reported as context: the threatened property is not required by this claim.",
        gate_id
      )
      context = c(context, gate_id)
      next
    }
    if (!required) {
      .csdg_warn(
        "The threatened property %s has no required-property entry; it is added as an open required property.",
        gate_id
      )
    }
    threats = c(threats, gate_id)
    if (!identical(properties[[gate_id]]$status, "contradicted")) {
      set_property(gate_id, "open", "threat", record$rationale)
    }
  }

  table = .csdg_property_table(properties)
  decision = if (any(table$status == "contradicted") || length(counterevidence)) {
    "not_met"
  } else if (!nrow(table) || any(table$status == "open")) {
    "unresolved"
  } else {
    "met"
  }
  .csdg_adjudication_result(
    decision = decision,
    applicability_source = applicability_source,
    applicability_rationale = applicability_rationale,
    linkage = linkage,
    properties = table,
    counterevidence = counterevidence,
    threats = threats,
    context = context,
    claim_applicable = TRUE,
    rationale = switch(
      decision,
      not_met = if (length(counterevidence)) {
        "Established counterevidence contradicts a required property; favorable results never offset it."
      } else {
        "A required property is contradicted; favorable results never offset it."
      },
      unresolved = if (!nrow(table)) {
        "No required property was supplied."
      } else {
        "At least one required property is open."
      },
      met = "Every required property is supported; this holds relative to the listed required properties."
    )
  )
}

.csdg_revalidate_evidence_record = function(record) {
  required = c("gate_id", "applicable", "role", "availability", "result_direction", "criterion",
    "criterion_source", "criterion_rationale", "materiality", "adjudication_basis", "claim_consequence", "rationale")
  missing_fields = setdiff(required, names(record))
  if (length(missing_fields)) {
    .csdg_stop("Evidence record is missing required field(s): %s.", paste(missing_fields, collapse = ", "))
  }
  arguments = setdiff(names(formals(.csdg_evidence_record_impl)), "warn")
  allowed = c(arguments, "variation_status", "proposition_linkage")
  if (any(!names(record) %in% allowed)) {
    .csdg_stop("Evidence record contains unsupported field(s): %s.",
      paste(setdiff(names(record), allowed), collapse = ", "))
  }
  values = unclass(record)[intersect(names(record), arguments)]
  values = values[!vapply(values, is.null, logical(1L))]
  do.call(.csdg_evidence_record_impl, c(values, list(warn = FALSE)))
}

.csdg_property_table = function(properties) {
  if (!length(properties)) {
    return(data.table(
      gate_id = character(), status = character(), source = character(), rationale = character()
    ))
  }
  table = data.table(
    gate_id = names(properties),
    status = vapply(properties, `[[`, character(1L), "status"),
    source = vapply(properties, `[[`, character(1L), "source"),
    rationale = vapply(properties, `[[`, character(1L), "rationale")
  )
  table[order(match(gate_id, .csdg_requirement_ids))]
}

.csdg_adjudication_result = function(decision, applicability_source, applicability_rationale, linkage, properties,
                                     counterevidence, threats, context, claim_applicable, rationale) {
  sort_ids = function(x) {
    x = unique(as.character(x))
    x[order(match(x, .csdg_requirement_ids))]
  }
  contradicted = sort_ids(properties[status == "contradicted", gate_id])
  open = sort_ids(properties[status == "open", gate_id])
  structure(
    list(
      assessment = decision,
      # Deprecated alias of `assessment`, kept for compatibility with versions up to 0.1.5; the decision itself
      # (retain, revise, or withhold) is taken by the researcher from `decision_options`.
      decision = decision,
      decision_options = switch(decision, met = "retain", not_applicable = character(), c("revise", "withhold")),
      claim_applicable = claim_applicable,
      applicability_source = applicability_source,
      applicability_rationale = applicability_rationale,
      proposition_linkage = linkage,
      properties = properties,
      contradicted_gate_ids = contradicted,
      open_gate_ids = open,
      counterevidence_gate_ids = sort_ids(counterevidence),
      threat_gate_ids = sort_ids(threats),
      context_gate_ids = sort_ids(context),
      blocking_gate_ids = contradicted,
      unresolved_gate_ids = open,
      rationale = rationale
    ),
    class = c("CSDGClaimAdjudication", "list")
  )
}
