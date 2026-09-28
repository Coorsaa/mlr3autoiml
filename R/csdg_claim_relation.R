#' Record the relation between a revised claim and its predecessor
#'
#' @description
#' When a claim is revised, the record compares the new claim with its predecessor in two separate ways.
#' For each of the six scope elements (quantity, model, procedure, data, meaning, use), the researcher declares
#' whether the new scope is the same, narrower, broader, or incomparable; whether the revised claim follows from its
#' predecessor is declared separately, with a rationale (`proposition_relation`).
#' A narrower scope need not yield a weaker claim: restricting "the mean is positive" to a subgroup with a negative
#' mean yields a claim that does not follow from the original.
#' The record distinguishes the three kinds of revision of the article: a weakening (`"logical_weakening"`), in which
#' the revised claim follows from its predecessor; a restriction without entailment
#' (`"restriction_without_entailment"`), in which the scope narrows but the revised claim does not follow from its
#' predecessor and needs its own evidence; and a change of question (`"change_of_question"`), in which the quantity
#' or the meaning changes.
#' The function stores these declarations and rationales; it does not prove them.
#' Without an explicit comparison of the claims, `relation` is `"unchecked"`; the relation implied by the scope
#' elements is returned as `scope_relation`.
#'
#' @param parent A `CSDGClaim`, the predecessor.
#' @param claim A `CSDGClaim`, the revised claim.
#' @param coordinate_relations A uniquely named character vector with one of `"same"`, `"narrower"`, `"broader"`, or
#'   `"incomparable"` for each scope element (`quantity`, `model`, `procedure`, `data`, `meaning`, `use`).
#'   The legacy names (`target`, `model_scope`, `explanation_design`, `analytic_distribution`, `semantics`,
#'   `scientific_use`) and the value `"alternative_or_incomparable"` are accepted with a deprecation warning.
#' @param rationale A nonempty explanation of the declared scope relations.
#' @param proposition_relation Whether the revised claim follows from its predecessor: `"unchecked"`, `"same"`,
#'   `"logical_weakening"` (the predecessor implies the revised claim under the stated assumptions),
#'   `"logical_strengthening"`, or `"incomparable"`.
#' @param proposition_rationale Argument and assumptions for the comparison of the claims.
#'   Required unless `proposition_relation = "unchecked"`.
#' @param revision_kind `"unspecified"`, `"logical_weakening"` (weakening), `"restriction_without_entailment"`, or
#'   `"change_of_question"`.
#'   A weakening requires `proposition_relation = "logical_weakening"`; a restriction without entailment requires a
#'   narrower scope and cannot be declared the same claim or a weakening; a change of question cannot be declared the
#'   same claim or a weakening. The legacy values `"context_restriction"` and `"scope_restriction"` (both now
#'   `"restriction_without_entailment"`) and `"estimand_change"` (now `"change_of_question"`) are accepted with a
#'   deprecation warning.
#'
#' @return A `CSDGClaimRelation` list with `relation` (from the comparison of the claims), `scope_relation`
#'   (implied by the scope elements), the claim statements, the declarations, and the table `scope` with columns
#'   `element`, `parent_value`, `claim_value`, and `relation`. `context_relation` and `coordinates` (with legacy
#'   element names) are kept for compatibility.
#'   `semantic_status` is `"unchecked"` or `"declared_not_verified"`; it is not an assessment.
#' @examples
#' both = csdg_claim(
#'   id = "both_models",
#'   statement = "Under marginal permutation, both models rely more on item 1 than on item 2.",
#'   claim_type = "global_explanation",
#'   quantity = "marginal PFI with squared error",
#'   model = "several_models",
#'   procedure = "each item permuted independently",
#'   data = "Z standard normal; item 1 = item 2 = outcome = Z",
#'   meaning = "model_description",
#'   use = "scientific description"
#' )
#' model_a = csdg_claim_revision(
#'   both, id = "model_a", claim_version = "C1", revision_relation = "narrower",
#'   statement = "Under marginal permutation, model A relies more on item 1 than on item 2.",
#'   model = "fitted_model"
#' )
#' csdg_claim_relation(
#'   both, model_a,
#'   coordinate_relations = c(quantity = "same", model = "narrower", procedure = "same", data = "same",
#'     meaning = "same", use = "same"),
#'   rationale = "The claim about models A and B is restricted to model A.",
#'   proposition_relation = "logical_weakening",
#'   proposition_rationale = "A universal claim over {A, B} implies the claim for A.",
#'   revision_kind = "logical_weakening"
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
  assert_string(proposition_relation, min.chars = 1L, .var.name = "proposition_relation")
  proposition_relation = .csdg_map_legacy(proposition_relation, .csdg_legacy_relations, "Claim relation")
  assert_choice(proposition_relation, .csdg_proposition_relations, .var.name = "proposition_relation")
  assert_string(proposition_rationale, min.chars = 1L, null.ok = TRUE, .var.name = "proposition_rationale")
  assert_string(revision_kind, min.chars = 1L, .var.name = "revision_kind")
  revision_kind = .csdg_map_legacy(revision_kind, .csdg_legacy_revision_kinds, "Revision kind")
  assert_choice(revision_kind, .csdg_revision_kinds, .var.name = "revision_kind")
  if (proposition_relation != "unchecked" && is.null(proposition_rationale)) {
    .csdg_stop("A declared comparison of the claims requires `proposition_rationale` and its assumptions.")
  }
  if (revision_kind == "logical_weakening" && proposition_relation != "logical_weakening") {
    .csdg_stop("`revision_kind = \"logical_weakening\"` requires `proposition_relation = \"logical_weakening\"`.")
  }
  if (revision_kind == "change_of_question" &&
      !proposition_relation %in% c("unchecked", "incomparable")) {
    .csdg_stop("A `change_of_question` is a different question, not automatically the same or a weaker claim.")
  }
  if (revision_kind == "restriction_without_entailment" &&
      proposition_relation %in% c("same", "logical_weakening")) {
    .csdg_stop(paste(
      "A `restriction_without_entailment` does not follow from its predecessor;",
      "declare `revision_kind = \"logical_weakening\"` if it does."
    ))
  }
  element_names = names(coordinate_relations)
  if (!is.null(element_names)) {
    legacy_names = element_names %in% names(.csdg_legacy_coordinates)
    if (any(legacy_names)) {
      for (name in element_names[legacy_names]) {
        .csdg_deprecate(name, .csdg_legacy_coordinates[[name]], "Scope element name")
      }
      element_names[legacy_names] = unname(.csdg_legacy_coordinates[element_names[legacy_names]])
      names(coordinate_relations) = element_names
    }
  }
  if (is.null(element_names) || anyDuplicated(element_names) ||
      !setequal(element_names, .csdg_scope_elements)) {
    .csdg_stop(
      "`coordinate_relations` must name every scope element exactly once: %s.",
      paste(.csdg_scope_elements, collapse = ", ")
    )
  }
  coordinate_relations = coordinate_relations[.csdg_scope_elements]
  coordinate_relations[] = .csdg_map_legacy(unname(coordinate_relations), .csdg_legacy_relations, "Scope relation")
  assert_subset(
    unname(coordinate_relations),
    .csdg_scope_relations,
    empty.ok = FALSE,
    .var.name = "coordinate_relations"
  )

  changed = coordinate_relations != "same"
  scope_relation = if (!any(changed)) {
    "same"
  } else if (any(coordinate_relations == "incomparable") ||
      all(c("narrower", "broader") %in% coordinate_relations)) {
    "incomparable"
  } else if (all(coordinate_relations %in% c("same", "narrower"))) {
    "narrower"
  } else {
    "broader"
  }
  if (revision_kind == "restriction_without_entailment" && scope_relation != "narrower") {
    .csdg_stop("A `restriction_without_entailment` narrows the scope; the declared scope relation is \"%s\".",
      scope_relation)
  }
  overall_relation = switch(
    proposition_relation,
    logical_weakening = "narrower",
    logical_strengthening = "broader",
    proposition_relation
  )

  element_value = function(x, element) {
    value = if (identical(element, "meaning")) .csdg_claim_meaning(x) else x[[.csdg_scope_fields[[element]]]]
    paste(value %||% NA_character_, collapse = " | ")
  }
  scope = data.table(
    element = .csdg_scope_elements,
    parent_value = vapply(.csdg_scope_elements, element_value, character(1L), x = parent),
    claim_value = vapply(.csdg_scope_elements, element_value, character(1L), x = claim),
    relation = unname(coordinate_relations)
  )
  legacy_order = match(.csdg_claim_coordinates, names(.csdg_legacy_coordinates))
  coordinates = data.table(
    coordinate = .csdg_claim_coordinates,
    parent_value = scope$parent_value[match(.csdg_legacy_coordinates[legacy_order], scope$element)],
    claim_value = scope$claim_value[match(.csdg_legacy_coordinates[legacy_order], scope$element)],
    relation = scope$relation[match(.csdg_legacy_coordinates[legacy_order], scope$element)]
  )

  structure(
    list(
      parent_claim_id = parent$id,
      claim_id = claim$id,
      relation = overall_relation,
      scope_relation = scope_relation,
      context_relation = scope_relation,
      proposition_relation = proposition_relation,
      proposition_rationale = proposition_rationale,
      parent_statement = parent$statement,
      claim_statement = claim$statement,
      revision_kind = revision_kind,
      semantic_status = if (proposition_relation == "unchecked") "unchecked" else "declared_not_verified",
      scope = scope,
      coordinates = coordinates,
      rationale = rationale
    ),
    class = c("CSDGClaimRelation", "list")
  )
}
