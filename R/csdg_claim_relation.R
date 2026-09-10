#' Document proposition and context relations between CSDG claims
#'
#' @description
#' Records a declared relation between two complete CSDG claim objects.
#' Every comparison covers target, model scope, explanation semantics, analytic distribution and measurement
#' context, intended use, and explanation design.
#' A claim is a proposition (`statement`) together with its six-coordinate context.
#' Equal contexts do not establish equal propositions, and a smaller population does not generally imply a weaker
#' claim about a marginal mean.
#' The function records declarations, not logical proofs or empirical validation.
#' Without an explicit proposition comparison, `relation` is `"unchecked"`; the former context aggregate remains
#' available as `context_relation`.
#'
#' @param parent A `CSDGClaim` used as the reference claim.
#' @param claim A `CSDGClaim` being compared with `parent`.
#' @param coordinate_relations A uniquely named character vector with one of `"same"`, `"narrower"`, `"broader"`,
#'   or `"alternative_or_incomparable"` for every claim coordinate.
#' @param rationale A nonempty explanation for the declared coordinate relations.
#' @param proposition_relation Declared relation of the revised proposition to the parent: `"unchecked"`, `"same"`,
#'   `"logical_weakening"`, `"logical_strengthening"`, or `"alternative_or_incomparable"`.
#'   A weakening means that the parent proposition entails the revised proposition under the stated assumptions.
#' @param proposition_rationale Explanation and assumptions for the proposition comparison.
#'   Required unless `proposition_relation = "unchecked"`.
#' @param revision_kind Documentary distinction among `"unspecified"`, `"logical_weakening"`,
#'   `"context_restriction"`, and `"estimand_change"`; it does not establish entailment.
#'
#' @return A `CSDGClaimRelation` list with separate semantic and context relations, proposition statements,
#'   declarations, and a six-row coordinate table.
#'   `semantic_status` is `"unchecked"` or `"declared_not_verified"`, not a claim-support decision.
#' @examples
#' broad = csdg_claim(
#'   id = "C0",
#'   statement = "Person-specific explanations support follow-up use.",
#'   target = "person-specific prediction",
#'   model_scope = "selected_model",
#'   semantics = "fitted_model_description",
#'   analytic_distribution = "analytic sample",
#'   scientific_use = "individual follow-up",
#'   explanation_design = "local surrogate"
#' )
#' global = csdg_claim_revision(
#'   broad,
#'   id = "C3",
#'   statement = "Held-out marginal PFI describes the selected model.",
#'   claim_version = "C3",
#'   revision_relation = "alternative_or_incomparable",
#'   target = "global loss increase",
#'   scientific_use = "global description",
#'   explanation_design = "held-out marginal PFI"
#' )
#' csdg_claim_relation(
#'   broad,
#'   global,
#'   coordinate_relations = c(
#'     target = "alternative_or_incomparable",
#'     model_scope = "same",
#'     semantics = "same",
#'     analytic_distribution = "same",
#'     scientific_use = "alternative_or_incomparable",
#'     explanation_design = "alternative_or_incomparable"
#'   ),
#'   rationale = "The target, use, and explanation design change the scientific question."
#' )
#' @export
csdg_claim_relation = function(
    parent,
    claim,
    coordinate_relations,
    rationale,
    proposition_relation = "unchecked",
    proposition_rationale = NULL,
    revision_kind = "unspecified") {
  assert_class(parent, "CSDGClaim", .var.name = "parent")
  assert_class(claim, "CSDGClaim", .var.name = "claim")
  assert_character(
    coordinate_relations,
    any.missing = FALSE,
    min.len = 1L,
    .var.name = "coordinate_relations"
  )
  assert_string(rationale, min.chars = 1L, .var.name = "rationale")
  assert_choice(proposition_relation, c(
    "unchecked", "same", "logical_weakening", "logical_strengthening", "alternative_or_incomparable"
  ), .var.name = "proposition_relation")
  assert_string(proposition_rationale, min.chars = 1L, null.ok = TRUE, .var.name = "proposition_rationale")
  assert_choice(revision_kind, c(
    "unspecified", "logical_weakening", "context_restriction", "estimand_change"
  ), .var.name = "revision_kind")
  if (proposition_relation != "unchecked" && is.null(proposition_rationale)) {
    .csdg_stop("A declared proposition comparison requires `proposition_rationale` and its assumptions.")
  }
  if (revision_kind == "logical_weakening" && proposition_relation != "logical_weakening") {
    .csdg_stop("`revision_kind = \"logical_weakening\"` requires that explicit proposition relation.")
  }
  if (revision_kind == "estimand_change" &&
      !proposition_relation %in% c("unchecked", "alternative_or_incomparable")) {
    .csdg_stop("An `estimand_change` is a different question, not automatically the same or a weaker proposition.")
  }
  if (is.null(names(coordinate_relations)) || anyDuplicated(names(coordinate_relations)) ||
      !setequal(names(coordinate_relations), .csdg_claim_coordinates)) {
    .csdg_stop(
      "`coordinate_relations` must name every claim coordinate exactly once: %s.",
      paste(.csdg_claim_coordinates, collapse = ", ")
    )
  }
  coordinate_relations = coordinate_relations[.csdg_claim_coordinates]
  assert_subset(
    coordinate_relations,
    .csdg_claim_relations,
    empty.ok = FALSE,
    .var.name = "coordinate_relations"
  )

  changed = coordinate_relations != "same"
  context_relation = if (!any(changed)) {
    "same"
  } else if (any(coordinate_relations == "alternative_or_incomparable") ||
      all(c("narrower", "broader") %in% coordinate_relations)) {
    "alternative_or_incomparable"
  } else if (all(coordinate_relations %in% c("same", "narrower"))) {
    "narrower"
  } else if (all(coordinate_relations %in% c("same", "broader"))) {
    "broader"
  } else {
    "alternative_or_incomparable"
  }
  overall_relation = switch(
    proposition_relation,
    logical_weakening = "narrower",
    logical_strengthening = "broader",
    proposition_relation
  )

  coordinates = data.table(
    coordinate = .csdg_claim_coordinates,
    parent_value = vapply(.csdg_claim_coordinates, function(name) {
      paste(parent[[name]] %||% NA_character_, collapse = " | ")
    }, character(1L)),
    claim_value = vapply(.csdg_claim_coordinates, function(name) {
      paste(claim[[name]] %||% NA_character_, collapse = " | ")
    }, character(1L)),
    relation = unname(coordinate_relations)
  )

  structure(
    list(
      parent_claim_id = parent$id,
      claim_id = claim$id,
      relation = overall_relation,
      context_relation = context_relation,
      proposition_relation = proposition_relation,
      proposition_rationale = proposition_rationale,
      parent_statement = parent$statement,
      claim_statement = claim$statement,
      revision_kind = revision_kind,
      semantic_status = if (proposition_relation == "unchecked") "unchecked" else "declared_not_verified",
      coordinates = coordinates,
      rationale = rationale
    ),
    class = c("CSDGClaimRelation", "list")
  )
}
