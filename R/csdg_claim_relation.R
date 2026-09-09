#' Compare two CSDG claims coordinate by coordinate
#'
#' @description
#' Records a declared relation between two complete CSDG claim objects.
#' Every comparison covers target, model scope, explanation semantics, analytic distribution and measurement
#' context, intended use, and explanation design.
#' The function does not infer restrictions from text because a textual change can alter the scientific question.
#'
#' @param parent A `CSDGClaim` used as the reference claim.
#' @param claim A `CSDGClaim` being compared with `parent`.
#' @param coordinate_relations A uniquely named character vector with one of `"same"`, `"narrower"`, `"broader"`,
#'   or `"alternative_or_incomparable"` for every claim coordinate.
#' @param rationale A nonempty explanation for the declared coordinate relations.
#'
#' @return A `CSDGClaimRelation` list containing the overall relation and a six-row coordinate table.
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
csdg_claim_relation = function(parent, claim, coordinate_relations, rationale) {
  assert_class(parent, "CSDGClaim", .var.name = "parent")
  assert_class(claim, "CSDGClaim", .var.name = "claim")
  assert_character(
    coordinate_relations,
    any.missing = FALSE,
    min.len = 1L,
    .var.name = "coordinate_relations"
  )
  assert_string(rationale, min.chars = 1L, .var.name = "rationale")
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
  overall_relation = if (!any(changed)) {
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
      coordinates = coordinates,
      rationale = rationale
    ),
    class = c("CSDGClaimRelation", "list")
  )
}
