test_that("audit continues after nonrequired gates and exports cleanly", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  cards = make_cards(fx$task)
  result = csdg_audit(
    task = fx$task,
    learner = fx$learner,
    claim = cards$claim,
    measurement = cards$measurement,
    explanation = cards$explanation,
    config = csdg_config(
      seed = 12L,
      resampling = list(folds = 3L, repeats = 1L),
      stability = list(
        pfi_repetitions = 2L,
        top_k = 2L,
        min_top_k_overlap = 0
      )
    )
  )
  expect_s3_class(result, "CSDGResult")
  card = csdg_report_card(result)
  expect_equal(nrow(card), 12L)
  expect_true(all(c(
    "applicable", "evidence_role", "availability", "result_direction", "criterion",
    "criterion_source", "criterion_rationale", "materiality", "adjudication_basis", "claim_consequence",
    "rationale"
  ) %in% names(card)))
  expect_setequal(unique(card$evidence_role), mlr3autoiml:::.csdg_evidence_roles)
  expect_equal(card[gate_id == "G6a", status], "not_applicable")
  expect_identical(card[gate_id == "G2", result_direction], "descriptive")
  expect_identical(card[gate_id == "G2", materiality], "not_materialized")
  expect_true(all(nzchar(card$started_at)))
  expect_true(all(nzchar(card$completed_at)))

  parent = tempfile("csdg-export-")
  dir.create(parent)
  path = csdg_export(result, parent, prefix = "test")
  expect_true(file.exists(file.path(path, "report_card.csv")))
  expect_true(file.exists(file.path(path, "claim_report.csv")))
  expect_true(file.exists(file.path(path, "csdg_result.rds")))
  expect_true(file.exists(file.path(path, "MANIFEST.csv")))

  schema_path = system.file("schema", "csdg-card.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(schema_path, simplifyVector = FALSE)
  expect_identical(
    unlist(schema[["$defs"]]$claim$properties$claim_level$enum, use.names = FALSE),
    c("functional", "predictive", "substantive")
  )
  expect_identical(schema[["$defs"]]$claim$properties$use_claim$type, "boolean")
  card_names = c("claim", "measurement", "explanation", "config")
  card_paths = setNames(file.path(path, "cards", paste0(card_names, ".json")), card_names)
  expect_true(all(file.exists(card_paths)))
  cards = setNames(lapply(card_paths, jsonlite::read_json, simplifyVector = FALSE), card_names)
  for (card_name in card_names) {
    required_fields = unlist(schema[["$defs"]][[card_name]]$required, use.names = FALSE)
    expect_true(is.list(cards[[card_name]]), info = card_name)
    expect_true(!is.null(names(cards[[card_name]])), info = card_name)
    expect_true(all(nzchar(names(cards[[card_name]]))), info = card_name)
    expect_true(all(required_fields %in% names(cards[[card_name]])), info = card_name)
    first_character = substr(trimws(readLines(card_paths[[card_name]], warn = FALSE)[[1L]]), 1L, 1L)
    expect_identical(first_character, "{", info = card_name)
  }
  cards_path = file.path(path, "cards", "cards.json")
  expect_true(file.exists(cards_path))
  cards_document = jsonlite::read_json(cards_path, simplifyVector = FALSE)
  expect_named(cards_document, unlist(schema$required, use.names = FALSE), ignore.order = FALSE)
  expect_identical(cards_document, cards)

  writeLines("stale", file.path(path, "stale.txt"))
  csdg_export(result, parent, prefix = "test")
  expect_false(file.exists(file.path(path, "stale.txt")))
})

test_that("report-card schema matches the claim-scoped gate and evidence-state contracts", {
  schema_path = system.file("schema", "csdg-report-card.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(schema_path, simplifyVector = TRUE)
  expect_identical(schema$items$properties$gate_id$enum, mlr3autoiml:::.csdg_gate_ids)
  expect_identical(
    schema$items$properties$status$enum,
    c("met", "unresolved", "not_met", "not_applicable", "error")
  )
  expect_identical(schema$items$properties$evidence_role$enum, mlr3autoiml:::.csdg_evidence_roles)
  expect_true("criterion" %in% schema$items$required)
  expect_identical(schema$items$properties$criterion$type, "string")
  expect_true("criterion_rationale" %in% schema$items$required)
})

test_that("report-card schema rejects contradictory evidence states", {
  schema_path = system.file("schema", "csdg-report-card.schema.json", package = "mlr3autoiml")
  schema = jsonlite::read_json(schema_path, simplifyVector = FALSE)
  rules = schema$items$allOf
  condition_const = function(rule, field) {
    node = rule[["if"]]$properties[[field]]
    if (is.null(node) || is.null(node$const)) NA_character_ else as.character(node$const)
  }

  consequences = vapply(rules, condition_const, character(1L), field = "claim_consequence")
  expect_setequal(
    consequences[!is.na(consequences)],
    c("retain_exact_claim", "exact_claim_not_retained", "revise_claim")
  )
  descriptive = which(vapply(rules, condition_const, character(1L), field = "evidence_role") ==
    "descriptive_context")
  expect_length(descriptive, 1L)
  expect_identical(rules[[descriptive]]$then$properties$claim_consequence$const, "none")

  not_materialized = which(vapply(rules, function(rule) {
    identical(rule[["if"]]$properties$evidence_role$const, "potential_defeater") &&
      identical(rule[["if"]]$properties$materiality$const, "not_materialized")
  }, logical(1L)))
  expect_length(not_materialized, 1L)
  expect_setequal(
    unlist(rules[[not_materialized]]$then$properties$claim_consequence$enum, use.names = FALSE),
    c("none", "unresolved")
  )
})

test_that("legacy gate statuses are quarantined from CSDG gate results", {
  legacy = GateResult$new(
    gate_id = "G1",
    gate_name = "Legacy predictive gate",
    pdr = "P",
    status = "pass",
    summary = "A compatibility-only heuristic status."
  )

  converted = .override_gate("G1", legacy)

  expect_s3_class(converted, "CSDGGateResult")
  expect_identical(converted$status, "unresolved")
  expect_error(
    new_gate_result("G1", status = "pass", summary = "Invalid CSDG status"),
    "status"
  )
  expect_warning(print(legacy), "compatibility-only")
})

test_that("external CSDG evidence preserves criterion provenance as separate fields", {
  converted = .override_gate("G2", list(
    status = "not_met",
    summary = "The supplied claim-specific fidelity criterion was not met.",
    criterion = list(value = 0.02, direction = "maximum"),
    criterion_source = "Prospective use protocol",
    criterion_rationale = "Maximum absolute error permitted for the declared individual-level use.",
    result_direction = "challenges",
    materiality = "materialized",
    adjudication_basis = "prespecified_claim_specific_criterion",
    claim_consequence = "exact_claim_not_retained",
    rationale = "Observed error exceeded the declared maximum."
  ))

  expect_identical(converted$criterion_source, "Prospective use protocol")
  expect_identical(
    converted$criterion_rationale,
    "Maximum absolute error permitted for the declared individual-level use."
  )
  expect_false("criterion_rationale" %in% names(converted$evidence))
  expect_error(
    .override_gate("G2", list(
      status = "not_met",
      criterion = list(value = 0.02, direction = "maximum"),
      criterion_source = "Prospective use protocol"
    )),
    "criterion_rationale"
  )
})

test_that("audit retains dependent artifacts and rejects unknown arguments", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  cards = make_cards(fx$task)
  config = csdg_config(
    seed = 13L,
    resampling = list(folds = 3L, repeats = 1L),
    performance = list(minimum_primary_score = 0),
    calibration = list(maximum_brier = 1),
    stability = list(pfi_repetitions = 1L, top_k = 2L, min_top_k_overlap = 0),
    criteria = list(
      performance.minimum_primary_score = list(
        source = "Prospective test protocol",
        rationale = "Any finite score meets the synthetic-fixture criterion."
      ),
      calibration.maximum_brier = list(
        source = "Prospective test protocol",
        rationale = "The unit upper bound is used only for this synthetic fixture."
      ),
      stability.min_top_k_overlap = list(
        source = "Prospective test protocol",
        rationale = "The zero lower bound exercises criterion plumbing in this fixture."
      )
    )
  )
  result = csdg_audit(
    fx$task,
    fx$learner,
    cards$claim,
    cards$measurement,
    cards$explanation,
    config = config
  )

  expect_s3_class(result$artifacts$oof, "CSDGResample")
  expect_true(all(c("performance", "calibration", "pfi") %in% names(result$artifacts)))
  expect_equal(result$gates$G1$status, "met")
  expect_equal(result$gates$G3a$status, "met")
  expect_identical(result$gates$G1$criterion_source, "Prospective test protocol")
  expect_identical(
    result$gates$G1$criterion_rationale,
    "Any finite score meets the synthetic-fixture criterion."
  )
  expect_identical(
    csdg_report_card(result)[gate_id == "G1", criterion_rationale],
    "Any finite score meets the synthetic-fixture criterion."
  )
  expect_false(result$gates$G5$status == "error")
  expect_error(
    csdg_audit(
      fx$task,
      fx$learner,
      cards$claim,
      cards$measurement,
      cards$explanation,
      unsupported = TRUE
    ),
    "Unknown `csdg_audit\\(\\)` argument"
  )
})

test_that("claim-required gates remain unresolved without adequacy criteria", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  cards = make_cards(fx$task)
  result = csdg_audit(
    fx$task,
    fx$learner,
    cards$claim,
    cards$measurement,
    cards$explanation,
    config = csdg_config(
      seed = 14L,
      resampling = list(folds = 3L, repeats = 1L),
      stability = list(pfi_repetitions = 1L)
    )
  )
  expect_equal(result$gates$G1$status, "unresolved")
  expect_equal(result$gates$G3a$status, "unresolved")
})

test_that("an unqualified numerical cutoff cannot adjudicate a CSDG module", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 60L)
  cards = make_cards(fx$task)
  result = csdg_audit(
    fx$task,
    fx$learner,
    cards$claim,
    cards$measurement,
    cards$explanation,
    config = csdg_config(
      seed = 1401L,
      resampling = list(folds = 2L, repeats = 1L),
      performance = list(minimum_primary_score = 0)
    ),
    run_gates = c("G0a", "G0b", "G1")
  )

  expect_identical(result$gates$G1$status, "unresolved")
  expect_false(result$gates$G1$evidence$criteria$criterion_complete)
  expect_true(is.na(result$gates$G1$evidence$criteria$passed))
  expect_identical(result$gates$G2$status, "unresolved")
  expect_identical(result$gates$G2$availability, "unavailable")
  expect_identical(result$gates$G2$materiality, "not_materialized")
})

test_that("explicit gate execution does not change claim-derived applicability", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  cards = make_cards(fx$task)
  claim = csdg_claim(
    id = "prediction_only",
    statement = "The cross-fitted pipeline predicts the binary outcome in the analytic sample.",
    claim_type = "predictive_performance",
    target = "binary outcome",
    unit = "row",
    population = "synthetic population",
    analytic_distribution = "synthetic analytic sample",
    model_scope = "cross_fitted_pipeline",
    setting_scope = "analytic_sample",
    scientific_use = "Characterize held-out prediction only",
    explanation_design = "No explanation claim"
  )
  result = csdg_audit(
    fx$task,
    fx$learner,
    claim,
    cards$measurement,
    config = csdg_config(seed = 140L, resampling = list(folds = 3L, repeats = 1L)),
    run_gates = c("G0a", "G0b", "G1", "G3a")
  )

  expect_false(result$plan[gate_id == "G3a", required])
  expect_true(result$plan[gate_id == "G3a", execute])
  expect_false(result$gates$G3a$status == "not_applicable")
})

test_that("model generalization cannot pass when the focal learner is outside tolerance", {
  skip_if_not_installed("rpart")
  data = data.frame(
    x = rep(c(-1, 1), each = 60L),
    y = factor(rep(c(0, 1), each = 60L), levels = c(0, 1))
  )
  task = mlr3::as_task_classif(data, target = "y", positive = "1")
  focal = mlr3::lrn("classif.featureless", predict_type = "prob")
  candidate = mlr3::lrn(
    "classif.rpart",
    predict_type = "prob",
    cp = 0,
    minsplit = 2L
  )
  claim = csdg_claim(
    id = "focal_tolerance",
    statement = "The selected model is robust across near-equivalent model classes.",
    claim_type = "model_generalization",
    target = "binary outcome",
    unit = "row",
    population = "synthetic population",
    analytic_distribution = "balanced deterministic fixture",
    model_scope = "near_equivalent_models",
    setting_scope = "analytic_sample",
    scientific_use = "Model-multiplicity evaluation",
    explanation_design = "Held-out PFI across accepted learners"
  )
  measurement = csdg_measurement(
    outcome = "y",
    predictors = "x",
    data_source = "synthetic fixture",
    sample_definition = "all rows",
    missingness = "none",
    preprocessing = "none",
    verification = list(status = "author_reviewed", artifact = "test fixture")
  )
  result = csdg_audit(
    task = task,
    learner = focal,
    claim = claim,
    measurement = measurement,
    explanation = csdg_explanation(),
    config = csdg_config(
      seed = 141L,
      resampling = list(folds = 2L, repeats = 1L),
      generalization = list(
        rashomon_tolerance_absolute = 0,
        rashomon_tolerance_relative = 0
      ),
      criteria = list(
        generalization.rashomon_tolerance = list(
          source = "Prospective test protocol",
          rationale = "Exact equivalence is used to test focal-model exclusion."
        )
      )
    ),
    candidate_learners = list(decision_tree = candidate)
  )

  expect_equal(result$gates$G6a$status, "not_met")
  expect_false(result$gates$G6a$diagnostics$focal_accepted)
  expect_match(result$gates$G6a$summary, "focal learner was outside")
  expect_true(any(
    result$gates$G6a$evidence$model_generalization$candidates$learner_id == focal$id
  ))
})

test_that("default export removes task, learner, models, and predictions", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 60L)
  cards = make_cards(fx$task)
  cards$measurement$weights = "No analysis weights were supplied."
  cards$measurement$clusters = fx$cluster
  result = csdg_audit(
    fx$task,
    fx$learner,
    cards$claim,
    cards$measurement,
    cards$explanation,
    config = csdg_config(
      seed = 15L,
      resampling = list(folds = 2L, repeats = 1L),
      stability = list(pfi_repetitions = 1L)
    )
  )
  path = csdg_export(result, tempfile("csdg-sanitized-parent-"), prefix = "safe")
  exported = readRDS(file.path(path, "csdg_result.rds"))
  expect_null(exported$artifacts$oof[["task"]])
  expect_null(exported$artifacts$oof[["learner"]])
  expect_null(exported$artifacts$oof[["resampling"]])
  expect_null(exported$artifacts$oof[["models"]])
  expect_null(exported$artifacts$oof[["predictions"]])
  expect_null(exported$artifacts$oof[["train_sets"]])
  expect_null(exported$artifacts$oof[["test_sets"]])
  expect_null(exported$artifacts$calibration[["observation_level"]])
  expect_null(exported$gates$G3a$evidence$calibration[["observation_level"]])
  expect_equal(exported$measurement$weights, "No analysis weights were supplied.")
  expect_null(exported$measurement$clusters)
})

test_that("default export removes case-level and split-membership evidence", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 120L, seed = 22L)
  cards = make_cards(fx$task)
  cards$claim = csdg_claim(
    id = "local_privacy",
    statement = "Prespecified held-out cases have locally faithful explanations.",
    claim_type = c("predictive_performance", "local_explanation"),
    target = "binary outcome",
    unit = "row",
    population = "synthetic population",
    analytic_distribution = "synthetic analytic sample"
  )
  cards$explanation = csdg_explanation(
    method_ids = "local_surrogate",
    scope = "local",
    local_cases = c(97L, 103L)
  )
  result = csdg_audit(
    fx$task,
    fx$learner,
    cards$claim,
    cards$measurement,
    cards$explanation,
    config = csdg_config(
      seed = 22L,
      resampling = list(folds = 2L, repeats = 1L),
      faithfulness = list(n_perturb = 50L),
      stability = list(pfi_repetitions = 1L)
    ),
    local_cases = c(97L, 103L)
  )
  result$metadata$student_predictions = data.frame(
    student_id = c("student-001", "student-002"),
    truth = c(0L, 1L),
    prob = c(0.2, 0.8)
  )
  result$metadata$respondent_records = data.frame(
    respondent_code = c("respondent-001", "respondent-002"),
    metric = c(0.1, 0.2)
  )
  result$metadata$participant_contacts = data.frame(
    first_name = c("Privatefirst", "Privatesecond"),
    last_name = c("Personone", "Persontwo"),
    email_address = c("private.one@example.org", "private.two@example.org"),
    phone_number = c("+49-555-0101", "+49-555-0102"),
    date_of_birth = as.Date(c("1990-01-01", "1991-02-02"))
  )
  result$metadata$first_name = "Privatefirst"
  result$metadata$emailAddress = "private.camel@example.org"
  result$metadata$nested_private = list(
    id = "metadata-id-secret",
    uuid = "metadata-uuid-secret",
    summary = "aggregate nested metadata"
  )
  result$metadata$oof_scores = data.frame(
    actual_outcome = c("outcome-secret-001", "outcome-secret-002"),
    predicted_score = c(0.2, 0.8)
  )
  result$metadata$prediction_record = list(
    actual_outcome = "scalar-outcome-secret",
    predicted_score = 0.7,
    summary = "row-level prediction"
  )
  result$metadata$identified_prediction_record = list(
    participant_id = "identified-participant-secret",
    actual_outcome = "identified-outcome-secret",
    predicted_score = 0.8
  )
  result$metadata$named_contact_vector = c(
    first_name = "atomic-name-secret",
    emailAddress = "atomic-email-secret@example.org"
  )
  result$metadata$named_prediction_vector = c(
    actual_outcome = "atomic-outcome-secret",
    predictedScore = "0.7"
  )
  result$metadata$contact_matrix = matrix(
    c("matrix-name-secret", "matrix-email-secret@example.org"),
    nrow = 1L,
    dimnames = list("private-row-name" = "private-row-name", c("firstName", "emailAddress"))
  )
  result$metadata$prediction_matrix = matrix(
    c("matrix-outcome-secret", "0.8"),
    nrow = 1L,
    dimnames = list("private-row-name" = "private-row-name", c("actualOutcome", "predictedScore"))
  )
  result$metadata$contact_array = array(
    c("array-name-secret", "aggregate"),
    dim = c(1L, 2L, 1L),
    dimnames = list("private-row", c("firstName", "metric"), "slice")
  )
  result$metadata$prediction_array = array(
    c("array-outcome-secret", "0.9"),
    dim = c(1L, 2L, 1L),
    dimnames = list("private-row", c("actualOutcome", "predictedScore"), "slice")
  )
  result$metadata$prediction_record_list = list(
    list(actual_outcome = "list-outcome-secret-1", predicted_score = 0.6),
    list(actual_outcome = "list-outcome-secret-2", predicted_score = 0.9)
  )
  base_fit = stats::lm(mpg ~ wt, data = mtcars)
  result$metadata$base_model = base_fit
  result$metadata$fitted_object = base_fit
  result$metadata$aggregate_table = data.frame(metric = "auc", estimate = 0.8)
  attr(result$metadata$aggregate_table, "private_model") = base_fit
  attr(result$metadata$aggregate_table, "student_id") = "attribute-student-secret"
  result$metadata$participantContacts = "container-contact-secret"
  result$metadata$private_model = mlr3::lrn("classif.rpart")
  result$metadata$public_note = "aggregate metadata"
  result$config$performance$measures = list(
    auc = mlr3::msr("classif.auc"),
    classification_error = mlr3::msr("classif.ce")
  )
  result$config$performance$primary = mlr3::msr("classif.auc")
  result$plan[, student_id := sprintf("student-%03d", seq_len(.N))]
  result$plan[, private_model := rep(list(mlr3::lrn("classif.rpart")), .N)]
  result$gates$G4$diagnostics$student_predictions = result$metadata$student_predictions
  result$gates$G4$diagnostics$respondent_records = result$metadata$respondent_records
  result$gates$G4$diagnostics$private_model = mlr3::lrn("classif.rpart")
  result$gates$G4$diagnostics$public_note = "aggregate gate metadata"

  parent = tempfile("csdg-private-export-")
  default_path = csdg_export(result, parent, prefix = "default")
  default_result = readRDS(file.path(default_path, "csdg_result.rds"))
  expect_null(default_result$artifacts$oof$train_sets)
  expect_null(default_result$artifacts$oof$test_sets)
  expect_null(default_result$artifacts$local_faithfulness$summary)
  expect_null(default_result$artifacts$local_faithfulness$coefficients)
  expect_true(data.table::is.data.table(default_result$artifacts$local_faithfulness$aggregate_summary))
  expect_equal(
    default_result$artifacts$local_faithfulness$evaluation$method,
    "deterministic weighted ridge cross-fitting"
  )
  expect_match(
    default_result$artifacts$local_faithfulness$evaluation$fit_separation,
    "without that perturbation"
  )
  expect_null(default_result$gates$G4$evidence$summary)
  expect_null(default_result$gates$G4$evidence$coefficients)
  expect_null(default_result$explanation$local_cases)
  expect_null(default_result$metadata$student_predictions)
  expect_null(default_result$metadata$respondent_records)
  expect_null(default_result$metadata$participant_contacts)
  expect_null(default_result$metadata$first_name)
  expect_null(default_result$metadata$emailAddress)
  expect_null(default_result$metadata$nested_private$id)
  expect_null(default_result$metadata$nested_private$uuid)
  expect_identical(default_result$metadata$nested_private$summary, "aggregate nested metadata")
  expect_null(default_result$metadata$oof_scores)
  expect_null(default_result$metadata[["prediction_record"]])
  expect_null(default_result$metadata[["identified_prediction_record"]])
  expect_null(default_result$metadata$named_contact_vector)
  expect_null(default_result$metadata$named_prediction_vector)
  expect_null(default_result$metadata$contact_matrix)
  expect_null(default_result$metadata$prediction_matrix)
  expect_null(default_result$metadata$contact_array)
  expect_null(default_result$metadata$prediction_array)
  expect_length(default_result$metadata$prediction_record_list, 0L)
  expect_null(default_result$metadata$base_model)
  expect_null(default_result$metadata$fitted_object)
  expect_identical(default_result$metadata$aggregate_table, data.frame(metric = "auc", estimate = 0.8))
  expect_null(default_result$metadata$participantContacts)
  expect_null(default_result$metadata$private_model)
  expect_identical(default_result$metadata$public_note, "aggregate metadata")
  expect_identical(default_result$config$performance$measures$auc, "classif.auc")
  expect_identical(default_result$config$performance$measures$classification_error, "classif.ce")
  expect_identical(default_result$config$performance$primary, "classif.auc")
  expect_false("student_id" %in% names(default_result$plan))
  expect_false("private_model" %in% names(default_result$plan))
  expect_null(default_result$gates$G4$diagnostics$student_predictions)
  expect_null(default_result$gates$G4$diagnostics$respondent_records)
  expect_null(default_result$gates$G4$diagnostics$private_model)
  expect_identical(default_result$gates$G4$diagnostics$public_note, "aggregate gate metadata")

  text_files = list.files(
    default_path,
    pattern = "[.](json|csv|md|txt)$",
    recursive = TRUE,
    full.names = TRUE
  )
  bundle_text = paste(unlist(lapply(text_files, readLines, warn = FALSE)), collapse = "\n")
  expect_false(grepl("case_id", bundle_text, fixed = TRUE))
  expect_false(grepl("train_sets", bundle_text, fixed = TRUE))
  expect_false(grepl("test_sets", bundle_text, fixed = TRUE))
  expect_false(grepl('"local_cases"', bundle_text, fixed = TRUE))
  expect_false(grepl("student-001", bundle_text, fixed = TRUE))
  expect_false(grepl("respondent-001", bundle_text, fixed = TRUE))
  expect_false(grepl("Privatefirst", bundle_text, fixed = TRUE))
  expect_false(grepl("private.one@example.org", bundle_text, fixed = TRUE))
  expect_false(grepl("private.camel@example.org", bundle_text, fixed = TRUE))
  expect_false(grepl("+49-555-0101", bundle_text, fixed = TRUE))
  expect_false(grepl("metadata-id-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("metadata-uuid-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("outcome-secret-001", bundle_text, fixed = TRUE))
  expect_false(grepl("scalar-outcome-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("identified-participant-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("atomic-name-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("atomic-outcome-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("matrix-name-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("matrix-outcome-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("array-name-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("array-outcome-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("list-outcome-secret-1", bundle_text, fixed = TRUE))
  expect_false(grepl("attribute-student-secret", bundle_text, fixed = TRUE))
  expect_false(grepl("container-contact-secret", bundle_text, fixed = TRUE))
  expect_false(grepl('"private_model"', bundle_text, fixed = TRUE))
  expect_true(grepl("aggregate metadata", bundle_text, fixed = TRUE))
  expect_true(grepl("aggregate gate metadata", bundle_text, fixed = TRUE))

  explicit_path = csdg_export(
    result,
    parent,
    prefix = "explicit",
    include_predictions = TRUE
  )
  explicit_result = readRDS(file.path(explicit_path, "csdg_result.rds"))
  expect_equal(explicit_result$artifacts$oof$train_sets, result$artifacts$oof$train_sets)
  expect_equal(explicit_result$artifacts$oof$test_sets, result$artifacts$oof$test_sets)
  expect_setequal(explicit_result$gates$G4$evidence$summary$case_id, c("97", "103"))
  expect_true("case_id" %in% names(explicit_result$gates$G4$evidence$coefficients))
  expect_null(explicit_result$metadata$student_predictions)
  expect_null(explicit_result$metadata$respondent_records)
  expect_null(explicit_result$metadata$participant_contacts)
  expect_null(explicit_result$metadata$first_name)
  expect_null(explicit_result$metadata$emailAddress)
  expect_null(explicit_result$metadata$nested_private$id)
  expect_null(explicit_result$metadata$nested_private$uuid)
  expect_identical(explicit_result$metadata$nested_private$summary, "aggregate nested metadata")
  expect_identical(explicit_result$metadata$oof_scores, result$metadata$oof_scores)
  expect_identical(explicit_result$metadata$prediction_record, result$metadata$prediction_record)
  expect_null(explicit_result$metadata[["identified_prediction_record"]])
  expect_null(explicit_result$metadata$named_contact_vector)
  expect_identical(explicit_result$metadata$named_prediction_vector, result$metadata$named_prediction_vector)
  expect_null(explicit_result$metadata$contact_matrix)
  expected_prediction_matrix = result$metadata$prediction_matrix
  rownames(expected_prediction_matrix) = NULL
  expect_identical(explicit_result$metadata$prediction_matrix, expected_prediction_matrix)
  expect_null(explicit_result$metadata$contact_array)
  expected_prediction_array = result$metadata$prediction_array
  dimnames(expected_prediction_array) = NULL
  expect_identical(explicit_result$metadata$prediction_array, expected_prediction_array)
  expect_identical(explicit_result$metadata$prediction_record_list, result$metadata$prediction_record_list)
  expect_null(explicit_result$metadata$base_model)
  expect_null(explicit_result$metadata$fitted_object)
  expect_identical(explicit_result$metadata$aggregate_table, data.frame(metric = "auc", estimate = 0.8))
  expect_null(explicit_result$metadata$participantContacts)
  expect_null(explicit_result$metadata$private_model)
  expect_false("student_id" %in% names(explicit_result$plan))
  expect_null(explicit_result$gates$G4$diagnostics$student_predictions)
  expect_null(explicit_result$gates$G4$diagnostics$respondent_records)
  expect_null(explicit_result$gates$G4$diagnostics$private_model)
  explicit_text_files = list.files(
    explicit_path,
    pattern = "[.](json|csv|md|txt)$",
    recursive = TRUE,
    full.names = TRUE
  )
  explicit_bundle_text = paste(unlist(lapply(explicit_text_files, readLines, warn = FALSE)), collapse = "\n")
  expect_true(grepl("case_id", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("student-001", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("respondent-001", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("Privatefirst", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("private.one@example.org", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("private.camel@example.org", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("+49-555-0101", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("metadata-id-secret", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("metadata-uuid-secret", explicit_bundle_text, fixed = TRUE))
  expect_true(grepl("outcome-secret-001", explicit_bundle_text, fixed = TRUE))
  expect_true(grepl("scalar-outcome-secret", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("identified-participant-secret", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("atomic-name-secret", explicit_bundle_text, fixed = TRUE))
  expect_true(grepl("atomic-outcome-secret", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("matrix-name-secret", explicit_bundle_text, fixed = TRUE))
  expect_true(grepl("matrix-outcome-secret", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("array-name-secret", explicit_bundle_text, fixed = TRUE))
  expect_true(grepl("array-outcome-secret", explicit_bundle_text, fixed = TRUE))
  expect_true(grepl("list-outcome-secret-1", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("attribute-student-secret", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("container-contact-secret", explicit_bundle_text, fixed = TRUE))
  expect_false(grepl("container-contact-secret", explicit_bundle_text, fixed = TRUE))
})

test_that("default export rejects unnamed models and individual prediction tables", {
  skip_if_not_installed("rpart")
  learner = mlr3::lrn("classif.rpart")
  adversarial = list(
    unnamed_model = list(list(learner)),
    student_predictions = data.frame(
      student_id = c("student-001", "student-002"),
      truth = c(0L, 1L),
      prob = c(0.2, 0.8)
    ),
    aggregate = data.frame(metric = "auc", estimate = 0.8)
  )
  names(adversarial$unnamed_model) = NULL

  sanitized = mlr3autoiml:::.sanitize_export_object(
    adversarial,
    include_models = FALSE,
    include_predictions = FALSE
  )

  expect_length(sanitized$unnamed_model, 0L)
  expect_null(sanitized$student_predictions)
  expect_identical(sanitized$aggregate, adversarial$aggregate)
  expect_null(
    mlr3autoiml:::.sanitize_export_object(
      adversarial$student_predictions,
      include_models = FALSE,
      include_predictions = TRUE
    )
  )
  expect_setequal(
    mlr3autoiml:::.export_sensitive_table_columns(
      c(
        "respondent_code", "person_hash", "uuid", "first_name", "email_address", "phone_number",
        "date_of_birth", "emailAddress", "case_id", "truth", "actual_outcome", "predictedScore", "gate_id",
        "model_name"
      )
    ),
    c(
      "respondent_code", "person_hash", "uuid", "first_name", "email_address", "phone_number",
      "date_of_birth", "emailAddress", "case_id", "truth", "actual_outcome", "predictedScore"
    )
  )
  expect_setequal(
    mlr3autoiml:::.export_private_field_names(
      c("respondent_code", "case_id", "first_name", "emailAddress", "gate_name", "model_name")
    ),
    c("respondent_code", "first_name", "emailAddress")
  )
})

test_that("default export removes S4 objects", {
  s4_fit = stats4::mle(function(mu) (mu - 1)^2, start = list(mu = 0))

  expect_null(
    mlr3autoiml:::.sanitize_export_object(
      s4_fit,
      include_models = FALSE,
      include_predictions = FALSE
    )
  )
})

test_that("default export removes list matrices containing learners", {
  skip_if_not_installed("rpart")
  learner_matrix = matrix(list(mlr3::lrn("classif.rpart")), nrow = 1L, ncol = 1L)

  expect_null(
    mlr3autoiml:::.sanitize_export_object(
      learner_matrix,
      include_models = FALSE,
      include_predictions = FALSE
    )
  )
})

test_that("default export removes outcome-only individual tables", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 60L)
  cards = make_cards(fx$task)
  result = csdg_audit(
    fx$task,
    fx$learner,
    cards$claim,
    cards$measurement,
    cards$explanation,
    config = csdg_config(
      seed = 16L,
      resampling = list(folds = 2L, repeats = 1L),
      stability = list(pfi_repetitions = 1L)
    )
  )
  secrets = c("SECRET-A", "SECRET-B")
  result$metadata$outcome_only = list(
    outcomes = data.frame(actual_outcome = secrets, age = c(10, 11))
  )

  path = csdg_export(result, tempfile("csdg-outcome-only-parent-"), prefix = "safe")
  exported = readRDS(file.path(path, "csdg_result.rds"))
  expect_null(exported$metadata$outcome_only$outcomes)

  serialized = serialize(exported, NULL, version = 3L)
  public_files = list.files(path, recursive = TRUE, full.names = TRUE)
  serialized_leaks = vapply(secrets, function(secret) {
    length(grepRaw(secret, serialized, fixed = TRUE)) > 0L
  }, logical(1L))
  public_bytes = unlist(lapply(public_files, function(public_file) {
    readBin(public_file, what = "raw", n = file.info(public_file)$size)
  }), use.names = FALSE)
  public_leaks = vapply(secrets, function(secret) {
    length(grepRaw(secret, public_bytes, fixed = TRUE)) > 0L
  }, logical(1L))
  expect_false(any(serialized_leaks))
  expect_false(any(public_leaks))
})
