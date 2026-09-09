
#' @rdname csdg_cards
#' @export
csdg_config = function(
    seed = 20260201L,
    resampling = list(),
    performance = list(),
    dependence = list(),
    calibration = list(),
    faithfulness = list(),
    stability = list(),
    generalization = list(),
    subgroup = list(),
    decision = list(),
    criteria = list(),
    export = list(),
    ...) {
  .extra_config = list(...)
  .assert_named_dots(.extra_config)
  if (length(.extra_config)) {
    stop(
      "Unknown top-level CSDG configuration field(s): ",
      paste(names(.extra_config) %||% rep("<unnamed>", length(.extra_config)),
            collapse = ", "),
      ". Use the documented nested configuration sections; ",
      "analysis-specific controls belong in the analysis script, not csdg_config().",
      call. = FALSE
    )
  }

  checkmate::assert_int(seed, lower = 0)
  sections = list(
    resampling = resampling,
    performance = performance,
    dependence = dependence,
    calibration = calibration,
    faithfulness = faithfulness,
    stability = stability,
    generalization = generalization,
    subgroup = subgroup,
    decision = decision,
    criteria = criteria,
    export = export
  )
  allowed_names = list(
    resampling = c("folds", "repeats", "store_models"),
    performance = c(
      "measures", "primary", "baseline", "conditional_prediction_bootstrap",
      "minimum_primary_score", "maximum_primary_score", "minimum_baseline_improvement"
    ),
    dependence = "max_levels",
    calibration = c(
      "bins", "maximum_brier", "maximum_logloss", "maximum_ece", "maximum_rmse",
      "calibration_slope_range", "maximum_abs_intercept"
    ),
    faithfulness = c(
      "n_perturb", "kernel_width", "target_scale", "neighborhood_method", "empirical_neighbors",
      "min_weighted_r2", "maximum_weighted_rmse", "maximum_weighted_mae",
      "maximum_target_case_absolute_error"
    ),
    stability = c("pfi_repetitions", "loss", "top_k", "min_top_k_overlap"),
    generalization = c(
      "rashomon_tolerance_absolute", "rashomon_tolerance_relative",
      "rashomon_reference_learner",
      "minimum_transport_score", "maximum_transport_score"
    ),
    subgroup = c("min_n", "thresholds", "conditional_bootstrap", "metric", "maximum_gap"),
    decision = c("thresholds", "minimum_fraction_beneficial"),
    criteria = c(
      "performance.minimum_primary_score",
      "performance.maximum_primary_score",
      "performance.minimum_baseline_improvement",
      "calibration.maximum_brier",
      "calibration.maximum_logloss",
      "calibration.maximum_ece",
      "calibration.maximum_rmse",
      "calibration.minimum_slope",
      "calibration.maximum_slope",
      "calibration.maximum_abs_intercept",
      "faithfulness.min_weighted_r2",
      "faithfulness.maximum_weighted_rmse",
      "faithfulness.maximum_weighted_mae",
      "faithfulness.maximum_target_case_absolute_error",
      "stability.min_top_k_overlap",
      "generalization.rashomon_tolerance",
      "generalization.minimum_transport_score",
      "generalization.maximum_transport_score",
      "subgroup.maximum_gap",
      "decision.minimum_fraction_beneficial"
    ),
    export = c("include_models", "include_predictions")
  )
  for (section_name in names(sections)) {
    section = sections[[section_name]]
    .assert_named_list(section, section_name)
    unknown = setdiff(names(section), allowed_names[[section_name]])
    if (length(unknown)) {
      .csdg_stop(
        "Unknown `%s` configuration field(s): %s.",
        section_name,
        paste(unknown, collapse = ", ")
      )
    }
  }
  defaults = list(
    seed = as.integer(seed),
    resampling = list(
      folds = 5L,
      repeats = 5L,
      store_models = TRUE
    ),
    performance = list(
      measures = NULL,
      primary = NULL,
      baseline = TRUE,
      conditional_prediction_bootstrap = 0L,
      minimum_primary_score = NULL,
      maximum_primary_score = NULL,
      minimum_baseline_improvement = NULL
    ),
    dependence = list(max_levels = 50L),
    calibration = list(
      bins = 10L,
      maximum_brier = NULL,
      maximum_logloss = NULL,
      maximum_ece = NULL,
      maximum_rmse = NULL,
      calibration_slope_range = NULL,
      maximum_abs_intercept = NULL
    ),
    faithfulness = list(
      n_perturb = 500L,
      kernel_width = 0.75,
      target_scale = "response",
      neighborhood_method = "synthetic",
      empirical_neighbors = 500L,
      min_weighted_r2 = NULL,
      maximum_weighted_rmse = NULL,
      maximum_weighted_mae = NULL,
      maximum_target_case_absolute_error = NULL
    ),
    stability = list(
      pfi_repetitions = 10L,
      loss = NULL,
      top_k = 10L,
      min_top_k_overlap = NULL
    ),
    generalization = list(
      rashomon_tolerance_absolute = NULL,
      rashomon_tolerance_relative = NULL,
      rashomon_reference_learner = NULL,
      minimum_transport_score = NULL,
      maximum_transport_score = NULL
    ),
    subgroup = list(
      min_n = 1L,
      thresholds = 0.5,
      conditional_bootstrap = 0L,
      metric = NULL,
      maximum_gap = NULL
    ),
    decision = list(
      thresholds = seq(0.05, 0.95, by = 0.05),
      minimum_fraction_beneficial = NULL
    ),
    criteria = list(),
    export = list(
      include_models = FALSE,
      include_predictions = FALSE
    )
  )

  cfg = defaults
  cfg$resampling = .recursive_modify(cfg$resampling, resampling)
  cfg$performance = .recursive_modify(cfg$performance, performance)
  cfg$dependence = .recursive_modify(cfg$dependence, dependence)
  cfg$calibration = .recursive_modify(cfg$calibration, calibration)
  cfg$faithfulness = .recursive_modify(cfg$faithfulness, faithfulness)
  cfg$stability = .recursive_modify(cfg$stability, stability)
  cfg$generalization = .recursive_modify(cfg$generalization, generalization)
  cfg$subgroup = .recursive_modify(cfg$subgroup, subgroup)
  cfg$decision = .recursive_modify(cfg$decision, decision)
  cfg$criteria = .recursive_modify(cfg$criteria, criteria)
  cfg$export = .recursive_modify(cfg$export, export)
  checkmate::assert_int(cfg$resampling$folds, lower = 2)
  checkmate::assert_int(cfg$resampling$repeats, lower = 1)
  checkmate::assert_int(cfg$performance$conditional_prediction_bootstrap, lower = 0)
  checkmate::assert_int(cfg$dependence$max_levels, lower = 2)
  checkmate::assert_int(cfg$calibration$bins, lower = 2)
  checkmate::assert_int(cfg$faithfulness$n_perturb, lower = 50)
  checkmate::assert_int(cfg$faithfulness$empirical_neighbors, lower = 2L)
  checkmate::assert_int(cfg$stability$pfi_repetitions, lower = 1)
  checkmate::assert_int(cfg$stability$top_k, lower = 1)
  checkmate::assert_int(cfg$subgroup$min_n, lower = 1)
  checkmate::assert_int(cfg$subgroup$conditional_bootstrap, lower = 0)
  checkmate::assert_number(
    cfg$faithfulness$kernel_width,
    lower = .Machine$double.eps,
    finite = TRUE
  )
  checkmate::assert_choice(cfg$faithfulness$target_scale, c("response", "link"))
  checkmate::assert_choice(cfg$faithfulness$neighborhood_method, c("synthetic", "empirical_knn"))
  checkmate::assert_flag(cfg$resampling$store_models)
  checkmate::assert_flag(cfg$performance$baseline)
  checkmate::assert_flag(cfg$export$include_models)
  checkmate::assert_flag(cfg$export$include_predictions)
  checkmate::assert_true(
    is.null(cfg$performance$measures) || is.character(cfg$performance$measures) ||
      inherits(cfg$performance$measures, "Measure") || is.list(cfg$performance$measures),
    .var.name = "performance.measures"
  )
  checkmate::assert_true(
    is.null(cfg$performance$primary) || checkmate::test_string(cfg$performance$primary, min.chars = 1L) ||
      inherits(cfg$performance$primary, "Measure"),
    .var.name = "performance.primary"
  )
  if (!is.null(cfg$stability$loss)) checkmate::assert_string(cfg$stability$loss, min.chars = 1L)
  if (!is.null(cfg$generalization$rashomon_reference_learner)) {
    checkmate::assert_string(
      cfg$generalization$rashomon_reference_learner,
      min.chars = 1L,
      .var.name = "generalization.rashomon_reference_learner"
    )
  }
  checkmate::assert_number(cfg$subgroup$thresholds, lower = 0, upper = 1, finite = TRUE)
  optional_numbers = c(
    "performance.minimum_primary_score" = cfg$performance$minimum_primary_score,
    "performance.maximum_primary_score" = cfg$performance$maximum_primary_score,
    "performance.minimum_baseline_improvement" = cfg$performance$minimum_baseline_improvement,
    "faithfulness.min_weighted_r2" = cfg$faithfulness$min_weighted_r2,
    "faithfulness.maximum_weighted_rmse" = cfg$faithfulness$maximum_weighted_rmse,
    "faithfulness.maximum_weighted_mae" = cfg$faithfulness$maximum_weighted_mae,
    "faithfulness.maximum_target_case_absolute_error" = cfg$faithfulness$maximum_target_case_absolute_error,
    "stability.min_top_k_overlap" = cfg$stability$min_top_k_overlap,
    "generalization.rashomon_tolerance_absolute" = cfg$generalization$rashomon_tolerance_absolute,
    "generalization.rashomon_tolerance_relative" = cfg$generalization$rashomon_tolerance_relative,
    "calibration.maximum_brier" = cfg$calibration$maximum_brier,
    "calibration.maximum_logloss" = cfg$calibration$maximum_logloss,
    "calibration.maximum_ece" = cfg$calibration$maximum_ece,
    "calibration.maximum_rmse" = cfg$calibration$maximum_rmse,
    "calibration.maximum_abs_intercept" = cfg$calibration$maximum_abs_intercept,
    "generalization.minimum_transport_score" = cfg$generalization$minimum_transport_score,
    "generalization.maximum_transport_score" = cfg$generalization$maximum_transport_score,
    "subgroup.maximum_gap" = cfg$subgroup$maximum_gap,
    "decision.minimum_fraction_beneficial" = cfg$decision$minimum_fraction_beneficial
  )
  for (name in names(optional_numbers)) {
    if (!is.null(optional_numbers[[name]])) {
      checkmate::assert_number(optional_numbers[[name]], finite = TRUE, .var.name = name)
    }
  }
  if (!is.null(cfg$faithfulness$min_weighted_r2)) {
    checkmate::assert_number(
      cfg$faithfulness$min_weighted_r2,
      lower = 0,
      upper = 1,
      finite = TRUE,
      .var.name = "faithfulness.min_weighted_r2"
    )
  }
  if (!is.null(cfg$stability$min_top_k_overlap)) {
    checkmate::assert_number(
      cfg$stability$min_top_k_overlap,
      lower = 0,
      upper = 1,
      finite = TRUE,
      .var.name = "stability.min_top_k_overlap"
    )
  }
  nonnegative_numbers = c(
    "calibration.maximum_brier" = cfg$calibration$maximum_brier,
    "calibration.maximum_logloss" = cfg$calibration$maximum_logloss,
    "calibration.maximum_ece" = cfg$calibration$maximum_ece,
    "calibration.maximum_rmse" = cfg$calibration$maximum_rmse,
    "calibration.maximum_abs_intercept" = cfg$calibration$maximum_abs_intercept,
    "faithfulness.maximum_weighted_rmse" = cfg$faithfulness$maximum_weighted_rmse,
    "faithfulness.maximum_weighted_mae" = cfg$faithfulness$maximum_weighted_mae,
    "faithfulness.maximum_target_case_absolute_error" = cfg$faithfulness$maximum_target_case_absolute_error,
    "generalization.rashomon_tolerance_absolute" = cfg$generalization$rashomon_tolerance_absolute,
    "generalization.rashomon_tolerance_relative" = cfg$generalization$rashomon_tolerance_relative,
    "subgroup.maximum_gap" = cfg$subgroup$maximum_gap
  )
  for (name in names(nonnegative_numbers)) {
    if (!is.null(nonnegative_numbers[[name]])) {
      checkmate::assert_number(nonnegative_numbers[[name]], lower = 0, finite = TRUE, .var.name = name)
    }
  }
  if (!is.null(cfg$calibration$calibration_slope_range)) {
    checkmate::assert_numeric(
      cfg$calibration$calibration_slope_range,
      finite = TRUE,
      len = 2L,
      sorted = TRUE
    )
  }
  if (!is.null(cfg$subgroup$metric)) {
    checkmate::assert_string(cfg$subgroup$metric, min.chars = 1L)
  }
  if (xor(is.null(cfg$subgroup$metric), is.null(cfg$subgroup$maximum_gap))) {
    .csdg_stop("`subgroup$metric` and `subgroup$maximum_gap` must be configured together.")
  }
  checkmate::assert_numeric(
    cfg$decision$thresholds,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    min.len = 1L,
    unique = TRUE
  )
  if (any(cfg$decision$thresholds <= 0 | cfg$decision$thresholds >= 1)) {
    .csdg_stop("`decision$thresholds` must be strictly between zero and one.")
  }
  if (!is.null(cfg$decision$minimum_fraction_beneficial)) {
    checkmate::assert_number(
      cfg$decision$minimum_fraction_beneficial,
      lower = 0,
      upper = 1,
      finite = TRUE
    )
  }

  for (criterion_name in names(cfg$criteria)) {
    metadata = cfg$criteria[[criterion_name]]
    .assert_named_list(metadata, sprintf("criteria.%s", criterion_name))
    assert_subset(
      names(metadata),
      c("source", "rationale"),
      empty.ok = FALSE,
      .var.name = sprintf("criteria.%s", criterion_name)
    )
    if (!setequal(names(metadata), c("source", "rationale"))) {
      .csdg_stop("`criteria$%s` must contain exactly `source` and `rationale`.", criterion_name)
    }
    assert_string(metadata$source, min.chars = 1L, .var.name = sprintf("criteria.%s.source", criterion_name))
    assert_string(
      metadata$rationale,
      min.chars = 1L,
      .var.name = sprintf("criteria.%s.rationale", criterion_name)
    )
  }

  structure(cfg, class = c("CSDGConfig", "list"))
}
