# Export deterministic claim-contract fixtures; no models or empirical inputs are used.
# Usage: Rscript tests/schema/export_claim_contract.R --library LIBRARY OUTPUT_DIRECTORY
#    or: Rscript tests/schema/export_claim_contract.R --source PACKAGE_ROOT OUTPUT_DIRECTORY

arguments = commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) == 3L, arguments[[1L]] %in% c("--library", "--source"))
if (arguments[[1L]] == "--library") {
  library(mlr3autoiml, lib.loc = arguments[[2L]])
} else {
  devtools::load_all(arguments[[2L]], quiet = TRUE)
}
output = normalizePath(arguments[[3L]], mustWork = FALSE)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
script_path = sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
fixtures = file.path(dirname(normalizePath(script_path)), "fixtures")
write_json = getFromNamespace(".write_json", "mlr3autoiml")
make_record = function(...) {
  defaults = list(gate_id = "G2", applicable = TRUE, role = "necessary_requirement",
    result_direction = "supports", rationale = "The supplied observation bears on a declared necessary property.")
  do.call(csdg_evidence_record, modifyList(defaults, list(...), keep.null = TRUE))
}
read_record = function(name) {
  do.call(csdg_evidence_record, jsonlite::read_json(file.path(fixtures, paste0(name, ".json"))))
}
support = make_record(claim_consequence = "retain_exact_claim")
pending = read_record("support_with_unresolved")
irrelevant = read_record("module_inapplicable")
challenge = make_record(gate_id = "G1", result_direction = "challenges",
  claim_consequence = "exact_claim_not_retained")
revision = make_record(result_direction = "mixed", claim_consequence = "revise_claim")
graded = make_record(role = "graded_support", claim_consequence = "retain_exact_claim")
context = make_record(role = "descriptive_context", result_direction = "descriptive")
defeater = make_record(gate_id = "G4", role = "potential_defeater", result_direction = "mixed",
  materiality = "materialized", adjudication_basis = "substantive_adjudication", claim_consequence = "revise_claim")
incomplete = make_record(availability = "incomplete", result_direction = "not_evaluated")
unavailable = make_record(availability = "unavailable", result_direction = "not_evaluated")
cases = list()
add_case = function(name, records, expected, claim_applicable = TRUE, legacy = FALSE) {
  decision = if (legacy) {
    csdg_adjudicate_claim(records)
  } else {
    csdg_adjudicate_claim(records, claim_applicable = claim_applicable,
      applicability_rationale = if (claim_applicable) "The fixture proposition is within scope." else "Outside scope.")
  }
  stopifnot(identical(decision$decision, expected))
  list(name = name, expected_decision = expected, evidence = lapply(records, unclass), adjudication = unclass(decision))
}
cases = list(
  add_case("in_scope_no_applicable_modules", list(irrelevant), "unresolved"),
  add_case("legacy_no_applicable_modules", list(irrelevant), "unresolved", legacy = TRUE),
  add_case("out_of_scope", list(irrelevant), "not_applicable", claim_applicable = FALSE),
  add_case("empty_in_scope", list(), "unresolved"),
  add_case("empty_out_of_scope", list(), "not_applicable", claim_applicable = FALSE),
  add_case("support_with_unresolved", list(pending), "unresolved"),
  add_case("met", list(support), "met"),
  add_case("not_met", list(challenge), "not_met"),
  add_case("mixed_revision", list(revision), "not_met"),
  add_case("incomplete", list(incomplete), "unresolved"),
  add_case("unavailable", list(unavailable), "unresolved"),
  add_case("mixed_undecided", list(make_record(result_direction = "mixed")), "unresolved"),
  add_case("descriptive_necessary", list(make_record(result_direction = "descriptive")), "unresolved"),
  add_case("unrelated_support_only", list(graded, context, irrelevant), "unresolved"),
  add_case("unrelated_support_with_missing_requirement", list(graded, incomplete), "unresolved"),
  add_case("non_compensation", list(support, graded, context, defeater), "not_met"),
  add_case("support_and_unresolved", list(support, pending), "unresolved"),
  add_case("unresolved_and_support", list(pending, support), "unresolved"),
  add_case("blocker_and_unresolved", list(challenge, pending, support), "not_met"),
  add_case("reordered_blocker_and_unresolved", list(support, pending, challenge), "not_met"),
  add_case("met_with_irrelevant_context", list(support, context, irrelevant), "met")
)
identity = list(package_version = as.character(packageVersion("mlr3autoiml")),
  loaded_package_path = getNamespaceInfo(asNamespace("mlr3autoiml"), "path"), mode = arguments[[1L]],
  r_version = as.character(getRversion()), empirical_execution = FALSE)
write_json(list(identity = identity, cases = cases), file.path(output, "claim_contract_cases.json"))
cat("Loaded package:", identity$package_version, "at", identity$loaded_package_path, "\n")
cat("Exported", length(cases), "validated runtime cases; no empirical execution.\n")
