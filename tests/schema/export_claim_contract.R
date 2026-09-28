# Export deterministic claim-contract fixtures; no models or empirical inputs are used.
# Cases 1-21 use the record format of versions up to 0.1.5 (accepted with deprecation warnings); cases 22-27 use
# the roles and statuses of 0.1.6.
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
property = function(gate_id) csdg_evidence_record(gate_id, TRUE, "required_property", status = "supported",
  rationale = "The supplied observation supports the required property.")
threat = csdg_evidence_record("G1", TRUE, "unresolved_threat",
  rationale = "The optimism caused by predictor selection was not quantified.",
  required_property = "Held-out performance is not inflated by predictor selection.",
  observation = "Predictors were selected on the same data outside the cross-validation.")
counterevidence = csdg_evidence_record("G2", TRUE, "established_counterevidence",
  rationale = "A verified coding error changes the reported values.",
  required_property = "The procedure computes the named quantity.",
  observation = "Two items were swapped before the computation.",
  relevance_to_proposition = "The swap changes which item each reported value refers to.")
legacy_context = make_record(role = "graded_support", claim_consequence = "unresolved")
fixture_claim = function(...) csdg_claim(id = "fixture", statement = "Fixture claim.", claim_type = "global_explanation",
  quantity = "marginal PFI", procedure = "held-out permutation", data = "fixture data", use = "fixture", ...)
fixture_measurement = csdg_measurement(outcome = "y", predictors = c("x1", "x2"))
fixture_explanation = csdg_explanation(method_ids = "pfi")
description_plan = csdg_gate_plan(fixture_claim(model = "several_models"), fixture_measurement, fixture_explanation)
causal_plan = csdg_gate_plan(fixture_claim(meaning = "causal_claim"), fixture_measurement, fixture_explanation)
causal_gates = causal_plan$gate_id[causal_plan$required]
unavailable = make_record(availability = "unavailable", result_direction = "not_evaluated")
cases = list()
add_case = function(name, records, expected, claim_applicable = TRUE, legacy = FALSE, plan = NULL) {
  decision = if (legacy) {
    csdg_adjudicate_claim(records)
  } else {
    csdg_adjudicate_claim(records, claim_applicable = claim_applicable,
      applicability_rationale = if (claim_applicable) "The fixture proposition is within scope." else "Outside scope.",
      plan = plan)
  }
  stopifnot(identical(decision$assessment, expected), identical(decision$decision, decision$assessment))
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
  add_case("met_with_irrelevant_context", list(support, context, irrelevant), "met"),
  add_case("unresolved_threat_on_required_g1", list(property("G1"), threat), "unresolved"),
  add_case("counterevidence_with_supported_properties", list(property("G0a"), property("G2"), counterevidence),
    "not_met"),
  add_case("context_with_legacy_unresolved_consequence", list(property("G2"), legacy_context), "met"),
  add_case("plan_required_gate_without_record", lapply(c("G0a", "G0b", "G2", "G5"), property), "unresolved",
    plan = description_plan),
  add_case("causal_claim_without_causal_design", lapply(causal_gates, property), "unresolved", plan = causal_plan),
  add_case("causal_claim_with_causal_design", lapply(c(causal_gates, "CD"), property), "met", plan = causal_plan)
)
# Cards as csdg_export() writes them: sanitized with the default export settings (no models, no predictions).
sanitize_result = getFromNamespace(".sanitize_result_for_export", "mlr3autoiml")
card_to_list = getFromNamespace(".card_to_list", "mlr3autoiml")
export_claim = fixture_claim(model = "learner", meaning = "model_description")
sanitized = sanitize_result(
  list(claim = export_claim, measurement = fixture_measurement, explanation = fixture_explanation,
    config = csdg_config(), metadata = list(), plan = csdg_gate_plan(export_claim, fixture_measurement,
      fixture_explanation), artifacts = list(), gates = list()),
  include_models = FALSE, include_predictions = FALSE
)
exported_cards = list(
  claim = card_to_list(sanitized$claim),
  measurement = card_to_list(sanitized$measurement),
  explanation = card_to_list(sanitized$explanation),
  config = card_to_list(sanitized$config)
)
stopifnot(identical(exported_cards$claim$scope$model, "learner"))
identity = list(package_version = as.character(packageVersion("mlr3autoiml")),
  loaded_package_path = getNamespaceInfo(asNamespace("mlr3autoiml"), "path"), mode = arguments[[1L]],
  r_version = as.character(getRversion()), empirical_execution = FALSE)
write_json(list(identity = identity, cases = cases, exported_cards = exported_cards),
  file.path(output, "claim_contract_cases.json"))
cat("Loaded package:", identity$package_version, "at", identity$loaded_package_path, "\n")
cat("Exported", length(cases), "validated runtime cases; no empirical execution.\n")
