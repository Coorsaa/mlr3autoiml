#' Blank claim record
#'
#' @description
#' Returns the blank claim record of the accompanying article (Supplement B) as a table with one row per field.
#' A claim record documents one claim from its first wording to the form in which it is reported:
#' Step 1 fills the claim and its origin, Step 2 the six scope elements, Step 3 the required gates, Step 4 the
#' evidence entries, and Step 5 the assessment, the reported claim, and any revision.
#' The template is stored in `inst/templates/claim_record_template.csv`; write the returned table to a file, fill the
#' column `entry`, and keep one record per claim, including claims that are not met or unresolved.
#'
#' @return A [data.table::data.table()] with 15 rows and the character columns
#'   * `field`: the field of the claim record;
#'   * `section`: `"claim"`, `"scope"`, `"origin"`, `"gates"`, `"evidence"`, `"other_evidence"`, `"assessment"`,
#'     `"reported_claim"`, or `"revision"`;
#'   * `enter`: what to enter in the field;
#'   * `package_field`: the function and argument of the package that record the field;
#'   * `entry`: empty, to be filled.
#' @seealso [csdg_claim()], [csdg_gate_plan()], [csdg_evidence_record()], [csdg_adjudicate_claim()],
#'   [csdg_property_status].
#' @examples
#' record = csdg_claim_record_template()
#' record[, c("field", "enter")]
#'
#' path = tempfile(fileext = ".csv")
#' data.table::fwrite(record, path)
#' @importFrom data.table fread
#' @export
csdg_claim_record_template = function() {
  path = system.file("templates", "claim_record_template.csv", package = "mlr3autoiml", mustWork = TRUE)
  fread(path, colClasses = "character", encoding = "UTF-8")[]
}
