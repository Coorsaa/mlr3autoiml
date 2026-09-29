#' Status of a required property
#'
#' @description
#' Convenience constructors for the three statuses of a required property (Step 4 of the article).
#' Judged against its criterion, a required property is supported if the observation meets the criterion,
#' contradicted if the observation shows that the property does not hold, and open if the criterion, the evidence, or a
#' completed computation is missing, or if the observation falls into the inconclusive zone of the criterion.
#' The constructors return the strings that [csdg_evidence_record()] accepts as `status`, checked against the
#' vocabulary of the package, so a misspelled status cannot enter a claim record.
#'
#' The open status is constructed by `open_status()` rather than `open()`, so that the package does not mask
#' [base::open()].
#'
#' @return A string: `"supported"`, `"contradicted"`, or `"open"`.
#' @seealso [csdg_evidence_record()], [csdg_adjudicate_claim()], [csdg_claim_record_template()].
#' @examples
#' supported()
#' contradicted()
#' open_status()
#'
#' csdg_evidence_record(
#'   "G5", TRUE, "required_property", status = supported(),
#'   rationale = "The ordering is the same in every fold.",
#'   required_property = "The ordering persists when the quantity is estimated again.",
#'   observation = "Item 1 ranks above item 2 in all five folds.",
#'   relevance_to_proposition = "Folds vary the training data while the model and perturbation stay fixed."
#' )
#' @name csdg_property_status
NULL

#' @rdname csdg_property_status
#' @export
supported = function() {
  .csdg_property_status("supported")
}

#' @rdname csdg_property_status
#' @export
contradicted = function() {
  .csdg_property_status("contradicted")
}

#' @rdname csdg_property_status
#' @export
open_status = function() {
  .csdg_property_status("open")
}

.csdg_property_status = function(status) {
  assert_choice(status, .csdg_property_statuses, .var.name = "status")
  status
}
