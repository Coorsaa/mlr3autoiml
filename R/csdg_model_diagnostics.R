# Full-fit model diagnostics used by both package clients and the publication analyses.
#
# These functions describe a supplied fitted model. They do not establish predictive validity, causal effects, or
# near-equivalence between learners.

.csdg_diagnostic_assert_task = function(task) {
  assert_class(task, "TaskSupervised", .var.name = "task")
  invisible(TRUE)
}

.csdg_diagnostic_assert_fitted_on_task = function(task, learner) {
  .csdg_diagnostic_assert_task(task)
  assert_class(learner, "Learner", .var.name = "fitted_learner")
  train_task = tryCatch(learner$state$train_task, error = function(e) NULL)
  if (is.null(train_task)) {
    stop("`fitted_learner` must be trained before model diagnostics are computed.", call. = FALSE)
  }
  trained_rows = sort(as.character(train_task$row_ids))
  expected_rows = sort(as.character(task$row_ids))
  same_task_hash = identical(learner$state$task_hash, task$hash)
  same_features = setequal(learner$state$feature_names, task$feature_names)
  same_target = setequal(train_task$target_names, task$target_names)
  same_task_type = inherits(train_task, class(task)[[1L]])
  if (!isTRUE(same_task_hash) || !identical(trained_rows, expected_rows) || !isTRUE(same_features) ||
      !isTRUE(same_target) || !isTRUE(same_task_type)) {
    stop(
      "`fitted_learner` must be a full fit on the supplied task; fold fits are not accepted here.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.csdg_diagnostic_assert_count = function(value, name, lower = 1L, upper = Inf) {
  assert_int(value, lower = lower, upper = upper, .var.name = name)
  as.integer(value)
}

.csdg_diagnostic_resolve_class = function(task, class_label) {
  if (inherits(task, "TaskRegr")) {
    if (!is.null(class_label)) {
      stop("`class_label` must be NULL for a regression task.", call. = FALSE)
    }
    return(NULL)
  }
  if (!inherits(task, "TaskClassif")) {
    stop("Only regression and classification tasks are supported.", call. = FALSE)
  }
  if (is.null(class_label) && length(task$class_names) == 2L) {
    class_label = task$positive
    if (is.null(class_label) || !nzchar(class_label)) {
      class_label = task$class_names[[2L]]
    }
  }
  if (is.null(class_label)) {
    stop("`class_label` is required for a multiclass task.", call. = FALSE)
  }
  assert_choice(class_label, task$class_names, .var.name = "class_label")
  class_label
}

.csdg_diagnostic_numeric_features = function(task, features, minimum = 1L) {
  numeric_ids = task$feature_types[type %in% c("numeric", "integer"), id]
  if (is.null(features)) {
    data = task$data(cols = numeric_ids)
    variance = vapply(data, function(x) var(as.numeric(x), na.rm = TRUE), numeric(1L))
    variance[!is.finite(variance) | variance <= 0] = NA_real_
    features = names(sort(variance, decreasing = TRUE, na.last = NA))
  }
  assert_character(
    features,
    any.missing = FALSE,
    min.len = minimum,
    unique = TRUE,
    .var.name = "features"
  )
  missing = setdiff(features, task$feature_names)
  unsupported = setdiff(features, numeric_ids)
  if (length(missing)) {
    stop("Unknown task features: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (length(unsupported)) {
    stop(
      "ALE and the interaction screen require numeric or integer features; unsupported: ",
      paste(unsupported, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (length(features) < minimum) {
    stop("At least ", minimum, " eligible feature(s) are required.", call. = FALSE)
  }
  features
}

.csdg_diagnostic_random_seed = function() {
  present = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  list(
    present = present,
    value = if (present) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  )
}

.csdg_diagnostic_restore_random_seed = function(state) {
  if (isTRUE(state$present)) {
    assign(".Random.seed", state$value, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    remove(".Random.seed", envir = .GlobalEnv)
  }
  invisible(NULL)
}

.csdg_diagnostic_sample_data = function(task, sample_n, seed) {
  sample_n = .csdg_diagnostic_assert_count(sample_n, "sample_n", upper = task$nrow)
  set.seed(as.integer(seed))
  rows = sample(task$row_ids, size = sample_n, replace = FALSE)
  as.data.table(task$data(rows = rows, cols = task$feature_names))
}

.csdg_diagnostic_tag = function(table, estimand, limitation, privacy_scope) {
  result = copy(as.data.table(table))
  result[, `:=`(
    estimand = as.character(estimand),
    limitation = as.character(limitation),
    privacy_scope = as.character(privacy_scope),
    interpretation_scope = "descriptive fitted-model diagnostic; not a causal effect"
  )]
  result[]
}

#' Full-fit model-query diagnostics
#'
#' @description
#' These functions calculate complementary diagnostics for a caller-supplied learner that has already been fitted
#' to the complete supplied task.
#' They label the estimand, limitations, interpretation scope, and privacy scope of every returned table.
#'
#' `csdg_effect_diagnostics()` returns one-dimensional accumulated local effects (ALE), partial dependence (PDP),
#' individual conditional expectation (ICE) curves, and aggregate ICE summaries.
#' The ALE table is dependence-aware, whereas PDP and ICE are intervention-style model queries that can leave
#' empirical support.
#'
#' `csdg_interaction_diagnostics()` screens numeric feature pairs with the Friedman-Popescu H statistic and computes
#' a two-dimensional ALE surface for the strongest nonzero pair or pairs.
#'
#' `csdg_shapley_diagnostics()` calculates bounded Monte Carlo Shapley contributions for explicitly supplied cases.
#' Conditional mode uses the package's approximate k-nearest-neighbor sampler and is not exact conditional Shapley
#' estimation.
#'
#' `csdg_prediction_multiplicity()` fits caller-confirmed near-equivalent learners to the complete task and summarizes
#' their prediction dispersion.
#' Near-equivalence must be established separately with common held-out splits and a prespecified tolerance.
#'
#' `csdg_mask_effect_support()` suppresses sparse one- or two-dimensional ALE estimates while retaining their support
#' counts and an explicit support indicator.
#'
#' @param task An [mlr3::TaskSupervised] object.
#' @param fitted_learner An [mlr3::Learner] fitted to every row and feature of `task`.
#' @param features Unique numeric or integer feature names.
#' @param sample_n Number of task rows sampled for the model queries.
#' @param ale_bins Number of empirical intervals per ALE dimension.
#' @param grid_n Number of points on each PDP and ICE grid.
#' @param ice_keep_n Number of private ICE curves retained per feature.
#' @param seed Non-negative integer random seed.
#' @param class_label Class whose probability is analyzed for classification.
#'   It is inferred from the positive class for binary classification and must be `NULL` for regression.
#' @param h_grid_n Number of points per feature in the H-statistic screen.
#' @param top_n_pairs Number of strongest nonzero feature pairs for which two-dimensional ALE is computed.
#' @param max_features Maximum number of features admitted to the interaction screen.
#' @param case_rows Between one and 100 task row ids selected for Shapley diagnostics.
#' @param background_n Number of sampled empirical background rows.
#' @param sample_size Number of Monte Carlo draws per Shapley contribution.
#' @param mode Either `"marginal"` or `"conditional"`.
#' @param conditional_k Number of neighbors used by the approximate conditional sampler.
#' @param case_selection How cases were selected: `"post_hoc_communication"`, `"random_analysis_sample"`, or
#'   `"prespecified_external"`.
#' @param candidate_learners A uniquely named list containing at least two [mlr3::Learner] objects.
#' @param near_equivalent_ids At least two names from `candidate_learners` that a separate held-out comparison has
#'   classified as near-equivalent.
#' @param row_ids Non-empty subset of task row ids on which to compare predictions.
#' @param range_thresholds Optional finite, non-negative thresholds for the cross-model prediction range.
#' @param keep_private_predictions Whether to return row-level predictions and dispersion.
#' @param x A one- or two-dimensional ALE table returned by these diagnostics.
#' @param min_n Minimum sampled observations required per interval or cell.
#'
#' @return
#' `csdg_effect_diagnostics()` returns `ale_1d`, `pdp`, `ice_private`, `ice_quantiles`, `ice_spread`, and `settings`.
#'
#' `csdg_interaction_diagnostics()` returns `interaction_screen`, `ale_2d`, and `settings`.
#'
#' `csdg_shapley_diagnostics()` returns `local_private`, `global`, `additivity_private`, and `settings`.
#'
#' `csdg_prediction_multiplicity()` returns private row-level outputs when requested, aggregate `distribution`,
#' `model_summary`, `pairwise`, `threshold_shares`, and `settings`.
#'
#' `csdg_mask_effect_support()` returns a copy of `x` with unsupported effect estimates replaced by `NA`, plus
#' `support_threshold` and `supported` columns.
#'
#' @name csdg_model_diagnostics
NULL

#' @rdname csdg_model_diagnostics
#' @export
csdg_effect_diagnostics = function(
    task,
    fitted_learner,
    features,
    sample_n = 500L,
    ale_bins = 12L,
    grid_n = 15L,
    ice_keep_n = 40L,
    seed = 20260201L,
    class_label = NULL) {
  .csdg_diagnostic_assert_fitted_on_task(task, fitted_learner)
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  features = .csdg_diagnostic_numeric_features(task, features)
  sample_n = .csdg_diagnostic_assert_count(sample_n, "sample_n", upper = task$nrow)
  ale_bins = .csdg_diagnostic_assert_count(ale_bins, "ale_bins", lower = 2L, upper = 50L)
  grid_n = .csdg_diagnostic_assert_count(grid_n, "grid_n", lower = 2L, upper = 50L)
  ice_keep_n = .csdg_diagnostic_assert_count(ice_keep_n, "ice_keep_n", upper = sample_n)
  seed = .csdg_diagnostic_assert_count(seed, "seed", lower = 0L, upper = .Machine$integer.max)
  class_label = .csdg_diagnostic_resolve_class(task, class_label)
  sampled = .csdg_diagnostic_sample_data(task, sample_n, seed)
  ale_trim = c(0.05, 0.95)

  pieces = lapply(seq_along(features), function(index) {
    feature = features[[index]]
    feature_seed = seed + index * 1009L
    ale = .autoiml_ale_1d_iml(
      task = task,
      model = fitted_learner,
      X = sampled,
      feature = feature,
      bins = ale_bins,
      trim = ale_trim,
      class_labels = class_label,
      seed = feature_seed
    )
    curves = .autoiml_pdp_ice_1d(
      task = task,
      model = fitted_learner,
      X = sampled,
      feature = feature,
      grid_n = grid_n,
      grid_type = "quantile",
      class_labels = class_label,
      ice_keep_n = ice_keep_n,
      ice_center = "none",
      seed = feature_seed
    )
    if (is.null(ale) || !nrow(ale) || is.null(curves) || !nrow(curves$pd) || !nrow(curves$ice)) {
      stop("Effect diagnostics could not be computed for feature `", feature, "`.", call. = FALSE)
    }
    list(ale = ale, pdp = curves$pd, ice = curves$ice, ice_spread = curves$ice_spread)
  })

  ale = rbindlist(lapply(pieces, `[[`, "ale"), fill = TRUE)
  pdp = rbindlist(lapply(pieces, `[[`, "pdp"), fill = TRUE)
  setnames(pdp, "pd", "mean_prediction")
  ice = rbindlist(lapply(pieces, `[[`, "ice"), fill = TRUE)
  ice[, ice_curve_id := sprintf("curve_%03d", match(row_index, unique(row_index))), by = feature]
  ice[, row_index := NULL]
  ice_quantiles = ice[, .(
    q05_prediction = quantile(yhat, 0.05, na.rm = TRUE, names = FALSE),
    q25_prediction = quantile(yhat, 0.25, na.rm = TRUE, names = FALSE),
    median_prediction = median(yhat, na.rm = TRUE),
    q75_prediction = quantile(yhat, 0.75, na.rm = TRUE, names = FALSE),
    q95_prediction = quantile(yhat, 0.95, na.rm = TRUE, names = FALSE),
    n_curves = uniqueN(ice_curve_id)
  ), by = .(class_label, feature, x)]
  ice_spread = rbindlist(lapply(pieces, `[[`, "ice_spread"), fill = TRUE)

  list(
    ale_1d = .csdg_diagnostic_tag(
      ale,
      paste(
        "Centered accumulated local prediction differences at empirical interval midpoints. High-cardinality",
        "grids span the sampled 5th-95th percentiles; low-cardinality features retain their full support."
      ),
      paste(
        "Full-fit, associational model diagnostic. Tail observations enter the outer trimmed intervals. Sparse",
        "intervals and correlated omitted features can affect the curve; model-fit uncertainty is not represented."
      ),
      "shareable_aggregate"
    ),
    pdp = .csdg_diagnostic_tag(
      pdp,
      "Mean fitted prediction under a one-feature intervention over sampled empirical reference rows.",
      paste(
        "Interventional model query that can create unsupported feature combinations when predictors are dependent;",
        "it is complementary to, and is not, ALE."
      ),
      "shareable_aggregate"
    ),
    ice_private = .csdg_diagnostic_tag(
      ice,
      "Per-reference-row fitted prediction under a one-feature intervention.",
      paste(
        "Interventional model query that can leave empirical support. Curves are row-level model outputs and must",
        "remain in the private run directory."
      ),
      "private_row_level"
    ),
    ice_quantiles = .csdg_diagnostic_tag(
      ice_quantiles,
      "Quantiles of per-reference-row intervention curves at each feature grid value.",
      paste(
        "Aggregate ICE summary; it can hide multimodality and can include unsupported interventions when predictors",
        "are dependent. It is not ALE."
      ),
      "shareable_aggregate"
    ),
    ice_spread = .csdg_diagnostic_tag(
      ice_spread,
      "Mean gridwise dispersion of uncentered individual fitted-prediction curves.",
      paste(
        "Descriptive curve heterogeneity can reflect scale, dependence, and interactions; it is not sampling",
        "uncertainty."
      ),
      "shareable_aggregate"
    ),
    settings = list(
      features = features,
      sample_n = nrow(sampled),
      ale_bins = ale_bins,
      ale_trim = ale_trim,
      grid_n = grid_n,
      ice_keep_n = ice_keep_n,
      seed = seed,
      class_label = class_label
    )
  )
}

.csdg_diagnostic_h_statistic = function(task, fitted_learner, data, feature_1, feature_2, grid_n, class_label) {
  grid_1 = .autoiml_grid_1d_iml(data[[feature_1]], grid_n = grid_n, grid_type = "quantile")
  grid_2 = .autoiml_grid_1d_iml(data[[feature_2]], grid_n = grid_n, grid_type = "quantile")
  if (length(grid_1) < 2L || length(grid_2) < 2L) {
    return(NULL)
  }
  n = nrow(data)
  baseline = .autoiml_predict_score(fitted_learner, data, task, class_of_interest = class_label)
  mean_baseline = mean(baseline, na.rm = TRUE)

  batch_1 = rbindlist(lapply(grid_1, function(value) {
    .autoiml_set_feature_value(copy(data), task, feature_1, value)
  }), use.names = TRUE)
  pred_1 = .autoiml_predict_score(fitted_learner, batch_1, task, class_of_interest = class_label)
  pd_1 = colMeans(matrix(pred_1, nrow = n, ncol = length(grid_1)), na.rm = TRUE)

  batch_2 = rbindlist(lapply(grid_2, function(value) {
    .autoiml_set_feature_value(copy(data), task, feature_2, value)
  }), use.names = TRUE)
  pred_2 = .autoiml_predict_score(fitted_learner, batch_2, task, class_of_interest = class_label)
  pd_2 = colMeans(matrix(pred_2, nrow = n, ncol = length(grid_2)), na.rm = TRUE)

  grid_index = expand.grid(i = seq_along(grid_1), j = seq_along(grid_2))
  batch_12 = rbindlist(lapply(seq_len(nrow(grid_index)), function(index) {
    modified = .autoiml_set_feature_value(
      copy(data), task, feature_1, grid_1[[grid_index$i[[index]]]]
    )
    .autoiml_set_feature_value(modified, task, feature_2, grid_2[[grid_index$j[[index]]]])
  }), use.names = TRUE)
  pred_12 = .autoiml_predict_score(fitted_learner, batch_12, task, class_of_interest = class_label)
  pd_12 = matrix(
    colMeans(matrix(pred_12, nrow = n, ncol = nrow(grid_index)), na.rm = TRUE),
    nrow = length(grid_1),
    ncol = length(grid_2)
  )

  centered_1 = pd_1 - mean_baseline
  centered_2 = pd_2 - mean_baseline
  centered_12 = pd_12 - mean_baseline
  residual = centered_12 - outer(centered_1, rep(1, length(centered_2))) -
    outer(rep(1, length(centered_1)), centered_2)
  denominator = sum(centered_12^2, na.rm = TRUE)
  h_squared = if (is.finite(denominator) && denominator > 0) {
    sum(residual^2, na.rm = TRUE) / denominator
  } else {
    NA_real_
  }
  h = if (is.finite(h_squared)) sqrt(min(max(h_squared, 0), 1)) else NA_real_
  data.table(
    class_label = if (is.null(class_label)) NA_character_ else class_label,
    feature_1 = feature_1,
    feature_2 = feature_2,
    h_statistic = h,
    h_squared_unclipped = h_squared,
    grid_cells = length(grid_1) * length(grid_2),
    sample_n = n
  )
}

.csdg_diagnostic_rank_h_screen = function(screen) {
  screen = copy(screen)
  screen = screen[order(
    !is.finite(h_statistic), -h_statistic, feature_1, feature_2, na.last = TRUE
  )]
  screen[, rank := NA_integer_]
  finite_rows = which(is.finite(screen$h_statistic))
  screen[finite_rows, rank := seq_along(finite_rows)]
  screen[]
}

.csdg_mask_ale_1d = function(curve, min_interval_n) {
  min_interval_n = .csdg_diagnostic_assert_count(min_interval_n, "min_interval_n", lower = 1L)
  if (!is.data.table(curve) || !all(c("ale", "n_interval") %in% names(curve))) {
    stop("`curve` must be a one-dimensional ALE data table with `ale` and `n_interval`.", call. = FALSE)
  }
  public = copy(curve)
  public[, `:=`(
    support_threshold = min_interval_n,
    supported = is.finite(n_interval) & n_interval >= min_interval_n
  )]
  public[supported == FALSE, ale := NA_real_]
  if ("limitation" %in% names(public)) {
    public[, limitation := paste0(
      "Full-fit associational ALE curve. Intervals below ",
      min_interval_n,
      " sampled observations are suppressed; model-fitting uncertainty is not represented."
    )]
  }
  public[]
}

.csdg_mask_ale_2d = function(surface, min_cell_n) {
  min_cell_n = .csdg_diagnostic_assert_count(min_cell_n, "min_cell_n", lower = 1L)
  if (!is.data.table(surface) || !all(c("ale2d", "n_cell") %in% names(surface))) {
    stop("`surface` must be a two-dimensional ALE data table with `ale2d` and `n_cell`.", call. = FALSE)
  }
  public = copy(surface)
  public[, `:=`(
    support_threshold = min_cell_n,
    supported = is.finite(n_cell) & n_cell >= min_cell_n
  )]
  public[supported == FALSE, `:=`(ale2d = NA_real_, n_cell = NA_integer_)]
  public[, limitation := paste0(
    "Full-fit associational model surface for H-ranked pair(s). Cells below ",
    min_cell_n,
    " sampled observations are suppressed; model-fitting uncertainty is not represented."
  )]
  public[]
}

#' @rdname csdg_model_diagnostics
#' @export
csdg_mask_effect_support = function(x, min_n) {
  assert_data_frame(x, min.rows = 1L, .var.name = "x")
  assert_int(min_n, lower = 1L, .var.name = "min_n")
  if (all(c("ale", "n_interval") %in% names(x))) {
    return(.csdg_mask_ale_1d(as.data.table(x), min_n))
  }
  if (all(c("ale2d", "n_cell") %in% names(x))) {
    return(.csdg_mask_ale_2d(as.data.table(x), min_n))
  }
  stop(
    "`x` must contain either `ale` and `n_interval` or `ale2d` and `n_cell`.",
    call. = FALSE
  )
}

#' @rdname csdg_model_diagnostics
#' @export
csdg_interaction_diagnostics = function(
    task,
    fitted_learner,
    features = NULL,
    sample_n = 400L,
    h_grid_n = 6L,
    ale_bins = 8L,
    top_n_pairs = 1L,
    max_features = 6L,
    seed = 20260202L,
    class_label = NULL) {
  .csdg_diagnostic_assert_fitted_on_task(task, fitted_learner)
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  features_supplied = !is.null(features)
  features = .csdg_diagnostic_numeric_features(task, features, minimum = 2L)
  sample_n = .csdg_diagnostic_assert_count(sample_n, "sample_n", upper = task$nrow)
  max_features = .csdg_diagnostic_assert_count(max_features, "max_features", lower = 2L, upper = 10L)
  if (isTRUE(features_supplied) && length(features) > max_features) {
    stop(
      "`features` contains more entries than `max_features`; narrow the interaction screen explicitly.",
      call. = FALSE
    )
  }
  if (!isTRUE(features_supplied) && length(features) > max_features) {
    features = features[seq_len(max_features)]
  }
  h_grid_n = .csdg_diagnostic_assert_count(h_grid_n, "h_grid_n", lower = 2L, upper = 12L)
  ale_bins = .csdg_diagnostic_assert_count(ale_bins, "ale_bins", lower = 2L, upper = 20L)
  pair_count = choose(length(features), 2L)
  top_n_pairs = .csdg_diagnostic_assert_count(top_n_pairs, "top_n_pairs", upper = pair_count)
  seed = .csdg_diagnostic_assert_count(seed, "seed", lower = 0L, upper = .Machine$integer.max)
  class_label = .csdg_diagnostic_resolve_class(task, class_label)
  sampled = .csdg_diagnostic_sample_data(task, sample_n, seed)

  pairs = combn(features, 2L, simplify = FALSE)
  screen = rbindlist(lapply(pairs, function(pair) {
    .csdg_diagnostic_h_statistic(
      task = task,
      fitted_learner = fitted_learner,
      data = sampled,
      feature_1 = pair[[1L]],
      feature_2 = pair[[2L]],
      grid_n = h_grid_n,
      class_label = class_label
    )
  }), fill = TRUE)
  if (!nrow(screen)) {
    stop("The interaction screen did not produce any feature pairs.", call. = FALSE)
  }
  screen = .csdg_diagnostic_rank_h_screen(screen)
  minimum_h = sqrt(.Machine$double.eps)
  top = screen[is.finite(h_statistic) & h_statistic > minimum_h][seq_len(min(top_n_pairs, .N))]
  surfaces = if (nrow(top)) {
    rbindlist(lapply(seq_len(nrow(top)), function(index) {
      surface = .autoiml_ale_2d(
        task = task,
        model = fitted_learner,
        X = sampled,
        feature1 = top$feature_1[[index]],
        feature2 = top$feature_2[[index]],
        bins = ale_bins,
        class_label = class_label
      )
      if (is.null(surface) || !nrow(surface)) {
        stop(
          "Two-dimensional ALE failed for pair `", top$feature_1[[index]], "` / `",
          top$feature_2[[index]], "`.",
          call. = FALSE
        )
      }
      surface[, `:=`(
        interaction_rank = top$rank[[index]],
        surface_status = "computed_for_nonzero_h_pair"
      )]
      surface
    }), fill = TRUE)
  } else {
    data.table(
      feature1 = NA_character_,
      feature2 = NA_character_,
      x1_left = NA_real_,
      x1_right = NA_real_,
      x2_bottom = NA_real_,
      x2_top = NA_real_,
      x1 = NA_real_,
      x2 = NA_real_,
      ale2d = NA_real_,
      n_cell = NA_integer_,
      class_label = if (is.null(class_label)) NA_character_ else class_label,
      interaction_rank = NA_integer_,
      surface_status = "not_computed_no_h_above_numerical_tolerance"
    )
  }

  list(
    interaction_screen = .csdg_diagnostic_tag(
      screen,
      "Friedman-Popescu H from one- and two-feature partial-dependence model queries on a common sample.",
      paste(
        "Screening statistic only. Dependence and off-support interventions can distort H; values do not prove a",
        "causal or data-generating interaction."
      ),
      "shareable_aggregate"
    ),
    ale_2d = .csdg_diagnostic_tag(
      surfaces,
      "Centered accumulated second-order local prediction differences over empirical two-feature quantile cells.",
      paste(
        "Full-fit associational model surface for H-ranked pair(s). Empty cells use the retained nearest-cell",
        "approximation, and sparse cells should not be overinterpreted. No surface is computed when every H value",
        "is nonfinite or below numerical tolerance."
      ),
      "shareable_aggregate"
    ),
    settings = list(
      features = features,
      sample_n = nrow(sampled),
      h_grid_n = h_grid_n,
      ale_bins = ale_bins,
      top_n_pairs = top_n_pairs,
      minimum_h_for_ale_2d = minimum_h,
      seed = seed,
      class_label = class_label
    )
  )
}

#' @rdname csdg_model_diagnostics
#' @export
csdg_shapley_diagnostics = function(
    task,
    fitted_learner,
    case_rows,
    background_n = 100L,
    sample_size = 64L,
    seed = 20260203L,
    class_label = NULL,
    mode = c("marginal", "conditional"),
    conditional_k = 5L,
    case_selection = c("post_hoc_communication", "random_analysis_sample", "prespecified_external")) {
  .csdg_diagnostic_assert_fitted_on_task(task, fitted_learner)
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  mode = match.arg(mode)
  case_selection = match.arg(case_selection)
  assert_choice(mode, c("marginal", "conditional"), .var.name = "mode")
  assert_choice(
    case_selection,
    c("post_hoc_communication", "random_analysis_sample", "prespecified_external"),
    .var.name = "case_selection"
  )
  assert_atomic(case_rows, any.missing = FALSE, min.len = 1L, max.len = 100L, .var.name = "case_rows")
  case_rows = unique(case_rows)
  if (!length(case_rows) || length(case_rows) > 100L) {
    stop("`case_rows` must identify between 1 and 100 cases.", call. = FALSE)
  }
  unknown = setdiff(as.character(case_rows), as.character(task$row_ids))
  if (length(unknown)) {
    stop("`case_rows` contains rows outside the task.", call. = FALSE)
  }
  background_n = .csdg_diagnostic_assert_count(
    background_n, "background_n", lower = 10L, upper = min(500L, task$nrow)
  )
  sample_size = .csdg_diagnostic_assert_count(sample_size, "sample_size", lower = 4L, upper = 500L)
  conditional_k = .csdg_diagnostic_assert_count(conditional_k, "conditional_k", upper = background_n)
  seed = .csdg_diagnostic_assert_count(seed, "seed", lower = 0L, upper = .Machine$integer.max)
  class_label = .csdg_diagnostic_resolve_class(task, class_label)
  set.seed(seed)
  background_rows = sample(task$row_ids, size = background_n, replace = FALSE)
  background = as.data.table(task$data(rows = background_rows, cols = task$feature_names))
  baseline_prediction = mean(
    .autoiml_predict_score(fitted_learner, background, task, class_of_interest = class_label)
  )

  case_results = lapply(seq_along(case_rows), function(index) {
    x_interest = as.data.table(
      task$data(rows = case_rows[[index]], cols = task$feature_names)
    )
    values = .autoiml_shapley_iml(
      task = task,
      model = fitted_learner,
      x_interest = x_interest,
      background = background,
      sample_size = sample_size,
      seed = seed + index * 1009L,
      class_labels = class_label,
      mode = mode,
      conditional_k = conditional_k,
      conditional_weighted = TRUE
    )
    values[, case_id := sprintf("case_%03d", index)]
    values[, case_selection := case_selection]
    values[, `:=`(
      phi_monte_carlo_se = sqrt(pmax(phi_var, 0) / sample_size),
      phi_monte_carlo_se_definition = paste(
        "Square root of the unbiased per-draw contribution variance divided by the Monte Carlo draw count;",
        "conditional on the fixed fitted model and sampled background."
      )
    )]
    case_prediction = .autoiml_predict_score(
      fitted_learner,
      x_interest,
      task,
      class_of_interest = class_label
    )[[1L]]
    list(
      local = values,
      additivity = data.table(
        case_id = sprintf("case_%03d", index),
        class_label = if (is.null(class_label)) NA_character_ else class_label,
        case_selection = case_selection,
        background_baseline = baseline_prediction,
        case_prediction = case_prediction,
        sum_phi = sum(values$phi),
        additivity_residual = case_prediction - baseline_prediction - sum(values$phi),
        baseline_definition = "Mean fitted prediction over the sampled empirical background"
      )
    )
  })
  local = rbindlist(lapply(case_results, `[[`, "local"), fill = TRUE)
  additivity = rbindlist(lapply(case_results, `[[`, "additivity"), fill = TRUE)
  if (!nrow(local)) {
    stop("Shapley diagnostics returned no values.", call. = FALSE)
  }
  local[, absolute_phi := abs(phi)]
  local[, absolute_rank := frank(-absolute_phi, ties.method = "min"), by = .(case_id, class_label)]
  global = local[, .(
    mean_phi = mean(phi, na.rm = TRUE),
    mean_absolute_phi = mean(absolute_phi, na.rm = TRUE),
    median_absolute_phi = median(absolute_phi, na.rm = TRUE),
    sd_phi = sd(phi, na.rm = TRUE),
    mean_monte_carlo_variance = mean(phi_var, na.rm = TRUE),
    mean_monte_carlo_se = mean(phi_monte_carlo_se, na.rm = TRUE),
    n_cases = uniqueN(case_id)
  ), by = .(class_label, feature, shap_mode, case_selection)]
  setorder(global, -mean_absolute_phi, feature)
  global[, global_rank := seq_len(.N), by = class_label]
  global_privacy = if (length(case_rows) >= 5L) "shareable_aggregate" else "private_small_n_aggregate"

  semantics = if (identical(mode, "marginal")) {
    "Monte Carlo interventional Shapley contribution relative to empirical background draws."
  } else {
    "Approximate dependence-aware Monte Carlo Shapley contribution using the retained k-nearest-neighbor sampler."
  }
  limitation = if (identical(mode, "marginal")) {
    paste(
      "Breaks feature dependence by construction, can query unsupported combinations, and is not causal. Monte Carlo",
      "standard errors condition on the fixed fitted model and sampled background; they exclude background-sampling",
      "and model-fitting uncertainty."
    )
  } else {
    paste(
      "Heuristic conditional sampler, not exact conditional SHAP; results depend on distance, k, background, and",
      "Monte Carlo budgets, and are not causal. Monte Carlo standard errors condition on the fixed fitted model and",
      "sampled background; they exclude background-sampling and model-fitting uncertainty."
    )
  }
  selection_label = switch(
    case_selection,
    post_hoc_communication = "post-hoc communication cases",
    random_analysis_sample = "a reproducibly selected random analysis sample",
    prespecified_external = "externally prespecified cases"
  )
  selection_limitation = switch(
    case_selection,
    post_hoc_communication = paste(
      "Cases were selected post hoc for communication; the aggregate must not be called a prespecified or",
      "population-global explanation."
    ),
    random_analysis_sample = paste(
      "Population-level interpretation depends on the caller having sampled cases randomly from the stated analysis",
      "population before inspecting their explanations."
    ),
    prespecified_external = "The caller is responsible for documenting when and how the cases were prespecified."
  )

  list(
    local_private = .csdg_diagnostic_tag(
      local,
      paste0(semantics, " Cases are ", selection_label, "."),
      paste(limitation, selection_limitation),
      "private_case_level"
    ),
    global = .csdg_diagnostic_tag(
      global,
      paste0(
        "Mean absolute and signed Shapley contributions over ", length(case_rows), " ", selection_label, "."
      ),
      paste(
        limitation,
        selection_limitation,
        "Ranks describe only the supplied case set."
      ),
      global_privacy
    ),
    additivity_private = .csdg_diagnostic_tag(
      additivity,
      paste(
        "Difference between each fitted case prediction and the sampled empirical-background baseline, compared",
        "with the sum of its Monte Carlo Shapley contributions."
      ),
      paste(
        "A nonzero residual diagnoses Monte Carlo and conditional-sampler non-additivity; it must not be hidden or",
        "interpreted as prediction uncertainty. No standard error is estimated for the residual."
      ),
      "private_case_level"
    ),
    settings = list(
      n_cases = length(case_rows),
      background_n = background_n,
      sample_size = sample_size,
      seed = seed,
      mode = mode,
      conditional_k = conditional_k,
      case_selection = case_selection,
      class_label = class_label
    )
  )
}

#' @rdname csdg_model_diagnostics
#' @export
csdg_prediction_multiplicity = function(
    task,
    candidate_learners,
    near_equivalent_ids,
    row_ids = task$row_ids,
    seed = 20260204L,
    class_label = NULL,
    range_thresholds = NULL,
    keep_private_predictions = TRUE) {
  .csdg_diagnostic_assert_task(task)
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  assert_list(candidate_learners, min.len = 2L, .var.name = "candidate_learners")
  assert_true(
    all(vapply(candidate_learners, inherits, logical(1L), what = "Learner")),
    .var.name = "candidate_learners"
  )
  candidate_names = names(candidate_learners)
  if (is.null(candidate_names) || any(!nzchar(candidate_names)) || anyDuplicated(candidate_names)) {
    stop("`candidate_learners` must have unique, non-empty names.", call. = FALSE)
  }
  assert_character(
    near_equivalent_ids,
    any.missing = FALSE,
    min.len = 2L,
    unique = TRUE,
    .var.name = "near_equivalent_ids"
  )
  if (length(near_equivalent_ids) < 2L || any(!near_equivalent_ids %in% candidate_names)) {
    stop("`near_equivalent_ids` must name at least two supplied learners.", call. = FALSE)
  }
  assert_flag(keep_private_predictions)
  assert_atomic(row_ids, any.missing = FALSE, min.len = 1L, .var.name = "row_ids")
  unknown_rows = setdiff(as.character(row_ids), as.character(task$row_ids))
  if (!length(row_ids) || length(unknown_rows)) {
    stop("`row_ids` must be a non-empty subset of task rows.", call. = FALSE)
  }
  if (!is.null(range_thresholds)) {
    assert_numeric(
      range_thresholds,
      lower = 0,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L,
      .var.name = "range_thresholds"
    )
    range_thresholds = sort(unique(as.numeric(range_thresholds)))
  }
  seed = .csdg_diagnostic_assert_count(seed, "seed", lower = 0L, upper = .Machine$integer.max)
  class_label = .csdg_diagnostic_resolve_class(task, class_label)
  newdata = as.data.table(task$data(rows = row_ids, cols = task$feature_names))

  prediction_list = lapply(seq_along(near_equivalent_ids), function(index) {
    learner_id = near_equivalent_ids[[index]]
    fit = candidate_learners[[learner_id]]$clone(deep = TRUE)
    set.seed(seed + index * 1009L)
    fit$train(task)
    values = .autoiml_predict_score(fit, newdata, task, class_of_interest = class_label)
    if (length(values) != length(row_ids) || any(!is.finite(values))) {
      stop("Non-finite or incomplete predictions from candidate `", learner_id, "`.", call. = FALSE)
    }
    as.numeric(values)
  })
  prediction_matrix = do.call(cbind, prediction_list)
  colnames(prediction_matrix) = near_equivalent_ids
  row_range = apply(prediction_matrix, 1L, function(values) diff(range(values)))
  row_sd = apply(prediction_matrix, 1L, sd)
  row_min = apply(prediction_matrix, 1L, min)
  row_max = apply(prediction_matrix, 1L, max)

  private_predictions = as.data.table(prediction_matrix)
  private_predictions[, row_id := as.character(row_ids)]
  setcolorder(private_predictions, "row_id")
  private_dispersion = data.table(
    row_id = as.character(row_ids),
    minimum_prediction = row_min,
    maximum_prediction = row_max,
    prediction_range = row_range,
    prediction_sd = row_sd
  )
  probabilities = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
  distribution = data.table(
    quantile_probability = probabilities,
    prediction_range = as.numeric(quantile(row_range, probabilities, names = FALSE)),
    prediction_sd = as.numeric(quantile(row_sd, probabilities, names = FALSE)),
    n_rows = length(row_range),
    n_models = ncol(prediction_matrix)
  )
  model_summary = rbindlist(lapply(seq_along(near_equivalent_ids), function(index) {
    values = prediction_matrix[, index]
    data.table(
      learner_id = near_equivalent_ids[[index]],
      mean_prediction = mean(values),
      sd_prediction = sd(values),
      q05_prediction = quantile(values, 0.05, names = FALSE),
      median_prediction = median(values),
      q95_prediction = quantile(values, 0.95, names = FALSE),
      n_rows = length(values)
    )
  }))
  learner_pairs = combn(seq_along(near_equivalent_ids), 2L, simplify = FALSE)
  pairwise = rbindlist(lapply(learner_pairs, function(pair) {
    difference = abs(prediction_matrix[, pair[[1L]]] - prediction_matrix[, pair[[2L]]])
    data.table(
      learner_1 = near_equivalent_ids[[pair[[1L]]]],
      learner_2 = near_equivalent_ids[[pair[[2L]]]],
      mean_absolute_difference = mean(difference),
      median_absolute_difference = median(difference),
      p95_absolute_difference = quantile(difference, 0.95, names = FALSE),
      maximum_absolute_difference = max(difference),
      n_rows = length(difference)
    )
  }))
  threshold_shares = if (is.null(range_thresholds)) {
    data.table(
      range_threshold = numeric(),
      share_above_threshold = numeric(),
      n_rows = integer(),
      n_models = integer()
    )
  } else {
    data.table(range_threshold = range_thresholds)[, `:=`(
      share_above_threshold = vapply(range_threshold, function(value) mean(row_range > value), numeric(1L)),
      n_rows = length(row_range),
      n_models = ncol(prediction_matrix)
    )]
  }

  common_limitation = paste(
    "Near-equivalence membership is supplied by the caller and must come from a prespecified, common-split held-out",
    "comparison. Full-data fits here describe prediction variation and do not establish model equivalence."
  )
  list(
    private_predictions = if (isTRUE(keep_private_predictions)) {
      .csdg_diagnostic_tag(
        private_predictions,
        "Full-fit prediction from each caller-confirmed near-equivalent learner for each requested row.",
        paste(common_limitation, "These row-level predictions must remain private."),
        "private_row_level"
      )
    } else {
      NULL
    },
    private_row_dispersion = if (isTRUE(keep_private_predictions)) {
      .csdg_diagnostic_tag(
        private_dispersion,
        "Within-row prediction dispersion across caller-confirmed near-equivalent full-data fits.",
        paste(common_limitation, "Row-level dispersion must remain private."),
        "private_row_level"
      )
    } else {
      NULL
    },
    distribution = .csdg_diagnostic_tag(
      distribution,
      "Empirical quantiles of within-row prediction range and standard deviation across full-data fits.",
      paste(common_limitation, "Aggregate quantiles can hide subgroups with concentrated disagreement."),
      "shareable_aggregate"
    ),
    model_summary = .csdg_diagnostic_tag(
      model_summary,
      "Marginal distribution summaries of fitted predictions for each full-data candidate fit.",
      paste(common_limitation, "Marginal summaries do not show whether candidates disagree on the same rows."),
      "shareable_aggregate"
    ),
    pairwise = .csdg_diagnostic_tag(
      pairwise,
      "Pairwise absolute prediction differences across requested rows.",
      paste(common_limitation, "Aggregate differences do not identify disagreement mechanisms."),
      "shareable_aggregate"
    ),
    threshold_shares = .csdg_diagnostic_tag(
      threshold_shares,
      "Share of requested rows whose cross-model prediction range exceeds each prespecified threshold.",
      paste(common_limitation, "Thresholds are descriptive and require outcome-scale justification."),
      "shareable_aggregate"
    ),
    settings = list(
      learner_ids = near_equivalent_ids,
      n_rows = length(row_ids),
      seed = seed,
      class_label = class_label,
      range_thresholds = range_thresholds
    )
  )
}
