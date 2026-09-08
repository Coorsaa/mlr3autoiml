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
  expect_equal(card[gate_id == "G6a", status], "not_applicable")
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
    stability = list(pfi_repetitions = 1L, top_k = 2L, min_top_k_overlap = 0)
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

  text_files = list.files(
    default_path,
    pattern = "[.](json|csv)$",
    recursive = TRUE,
    full.names = TRUE
  )
  bundle_text = paste(unlist(lapply(text_files, readLines, warn = FALSE)), collapse = "\n")
  expect_false(grepl("case_id", bundle_text, fixed = TRUE))
  expect_false(grepl("train_sets", bundle_text, fixed = TRUE))
  expect_false(grepl("test_sets", bundle_text, fixed = TRUE))
  expect_false(grepl('"local_cases"', bundle_text, fixed = TRUE))

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
})
