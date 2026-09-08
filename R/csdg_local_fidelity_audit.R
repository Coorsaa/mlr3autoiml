.local_fidelity_quantile = function(x, probability) {
  as.numeric(quantile(x, probability, names = FALSE, type = 8, na.rm = TRUE))
}

.local_fidelity_seed_matrix = function(seeds, n_cases) {
  if (is.matrix(seeds)) {
    seed_matrix = seeds
    if (nrow(seed_matrix) != n_cases) {
      .csdg_stop("A matrix supplied as `seeds` must have one row per requested case.")
    }
  } else {
    assert_integerish(
      seeds,
      lower = 0,
      upper = .Machine$integer.max - 100000L,
      any.missing = FALSE,
      min.len = 2L,
      unique = TRUE,
      .var.name = "seeds"
    )
    seed_matrix = matrix(rep(as.integer(seeds), each = n_cases), nrow = n_cases)
  }
  assert_integerish(
    as.vector(seed_matrix),
    lower = 0,
    upper = .Machine$integer.max - 100000L,
    any.missing = FALSE,
    min.len = 2L,
    .var.name = "seeds"
  )
  if (ncol(seed_matrix) < 2L) {
    .csdg_stop("`seeds` must provide at least two perturbation seeds per case.")
  }
  duplicated_rows = vapply(
    seq_len(nrow(seed_matrix)),
    function(index) anyDuplicated(seed_matrix[index, ]) > 0L,
    logical(1L)
  )
  if (any(duplicated_rows)) {
    .csdg_stop("Each case must have distinct perturbation seeds.")
  }
  matrix(as.integer(seed_matrix), nrow = nrow(seed_matrix), ncol = ncol(seed_matrix))
}

.local_fidelity_case_map = function(x, cases, case_labels, case_metadata) {
  model_index = vapply(cases, function(row_id) {
    matched = which(vapply(x$test_sets, function(rows) row_id %in% rows, logical(1L)))
    if (length(matched) != 1L) {
      .csdg_stop(
        "Case row id %d must occur in exactly one assessment split; found %d.",
        row_id,
        length(matched)
      )
    }
    matched
  }, integer(1L))
  result = data.table(
    case_label = case_labels,
    row_id = as.integer(cases),
    iteration = model_index,
    repetition = as.integer(x$fold_scores$repetition[model_index]),
    fold = as.integer(x$fold_scores$fold[model_index])
  )
  if (!is.null(case_metadata)) {
    result = cbind(result, .as_dt(case_metadata))
  }
  result[]
}

.local_fidelity_prepare_cases = function(x, cases, case_labels, case_metadata) {
  assert_class(x, "CSDGResample")
  assert_integerish(cases, any.missing = FALSE, min.len = 1L, unique = TRUE)
  if (is.null(x$train_sets) || is.null(x$test_sets) || length(x$train_sets) != length(x$test_sets) ||
      is.null(x$models) || length(x$models) != length(x$test_sets) || any(vapply(x$models, is.null, logical(1L)))) {
    .csdg_stop("Stored fold models and their analysis and assessment splits are required.")
  }
  cases = as.integer(cases)
  unknown = setdiff(cases, x$task$row_ids)
  if (length(unknown)) {
    .csdg_stop("Unknown case row ids: %s.", paste(unknown, collapse = ", "))
  }
  case_labels = case_labels %||% sprintf("case_%03d", seq_along(cases))
  assert_character(
    case_labels,
    any.missing = FALSE,
    min.chars = 1L,
    len = length(cases),
    unique = TRUE,
    .var.name = "case_labels"
  )
  if (!is.null(case_metadata)) {
    assert_data_frame(
      case_metadata,
      any.missing = FALSE,
      nrows = length(cases),
      min.cols = 1L,
      .var.name = "case_metadata"
    )
    if (anyDuplicated(names(case_metadata)) || any(!nzchar(names(case_metadata)))) {
      .csdg_stop("`case_metadata` must have unique, nonempty column names.")
    }
    reserved = intersect(names(case_metadata), c(
      "case_label", "row_id", "iteration", "repetition", "fold", "perturbation_replicate",
      "perturbation_seed", "crossfit_seed", "kernel_width", "analysis_role", "n_perturb",
      "n_crossfit_folds", "target_scale", "neighborhood_method", "weighted_r2", "weighted_rmse",
      "weighted_mae", "maximum_absolute_error", "target_case_model_prediction",
      "target_case_surrogate_prediction", "target_case_error", "target_case_absolute_error",
      "apparent_weighted_r2", "apparent_weighted_rmse",
      "perturbation_effective_n", "crossfit_effective_n_min", "crossfit_effective_n_max",
      "n_design_terms", "n_active_design_terms", "n_case_values_imputed", "case_imputed_features",
      "fidelity_threshold",
      "meets_threshold", "support_kernel_width", "nearest_training_distance",
      "tenth_nearest_training_distance", "fiftieth_nearest_training_distance",
      "proportion_training_within_kernel", "kernel_weight_effective_n", "n_training_background",
      "n_perturbation_replicates", "median_weighted_r2", "median_weighted_rmse", "mean_weighted_r2",
      "minimum_weighted_r2_across_seeds", "maximum_weighted_r2_across_seeds", "monte_carlo_sd",
      "monte_carlo_range", "proportion_replicates_below_threshold", "case_below_threshold",
      "case_summary_rule", "proximity_quartile"
    ))
    if (length(reserved)) {
      .csdg_stop("`case_metadata` uses reserved columns: %s.", paste(reserved, collapse = ", "))
    }
    atomic_columns = vapply(case_metadata, function(column) is.atomic(column) && is.null(dim(column)), logical(1L))
    if (any(!atomic_columns)) {
      .csdg_stop("`case_metadata` columns must be atomic vectors.")
    }
  }
  .local_fidelity_case_map(x, cases, case_labels, case_metadata)
}

.local_support_metrics = function(case, background, kernel_width) {
  distance = .local_distance(background, case, background)
  distance = distance[is.finite(distance)]
  if (!length(distance)) {
    .csdg_stop("No finite fold-training distance is available for a requested local case.")
  }
  weights = exp(pmax(-(distance^2) / kernel_width^2, log(.Machine$double.xmin)))
  ordered = sort(distance)
  data.table(
    support_kernel_width = kernel_width,
    nearest_training_distance = ordered[[1L]],
    tenth_nearest_training_distance = ordered[[min(10L, length(ordered))]],
    fiftieth_nearest_training_distance = ordered[[min(50L, length(ordered))]],
    proportion_training_within_kernel = mean(distance <= kernel_width),
    kernel_weight_effective_n = .local_effective_n(weights),
    n_training_background = length(distance)
  )
}

.local_fidelity_case_data = function(x, case_map) {
  features = x$task$feature_names
  model_index = case_map$iteration[[1L]]
  case = .as_dt(x$task$data(rows = case_map$row_id[[1L]], cols = features))
  background = .as_dt(x$task$data(rows = x$train_sets[[model_index]], cols = features))
  imputed = .impute_local_case_from_background(case, background)
  list(case = imputed$case, background = background, imputed_features = imputed$features)
}

.local_support_case = function(x, case_map, kernel_width) {
  case_data = .local_fidelity_case_data(x, case_map)
  cbind(
    case_map[, setdiff(names(case_map), "row_id"), with = FALSE],
    data.table(
      n_case_values_imputed = length(case_data$imputed_features),
      case_imputed_features = paste(case_data$imputed_features, collapse = ",")
    ),
    .local_support_metrics(case_data$case, case_data$background, kernel_width)
  )
}

#' Diagnose fold-training support for held-out local cases
#'
#' Measures the proximity of each requested held-out case to the analysis rows for its corresponding fold model.
#' Missing case values are filled from that fold-training background before distances are calculated.
#'
#' @param x A `CSDGResample` with stored fold models.
#' @param cases Unique task row ids that each occur in exactly one assessment split.
#' @param kernel_width Positive kernel width used for the within-kernel proportion and Kish effective sample size.
#' @param case_labels Optional unique pseudonymous labels in the same order as `cases`.
#' @param case_metadata Optional data frame with one row per case and columns, such as a prespecified risk stratum,
#'   that are copied to the output.
#'
#' @return A data table with held-out split identifiers, optional case metadata, case-imputation details,
#'   nearest-neighbor distances, the proportion of fold-training rows within the kernel, kernel-weight Kish effective
#'   sample size, and the fold-training background size.
#' @examplesIf requireNamespace("rpart", quietly = TRUE)
#' set.seed(1L)
#' example_x1 = rnorm(60L)
#' example_data = data.frame(
#'   x1 = example_x1,
#'   x2 = rnorm(60L),
#'   y = factor(ifelse(example_x1 > 0, "yes", "no"))
#' )
#' example_task = mlr3::as_task_classif(example_data, target = "y", positive = "yes")
#' example_learner = mlr3::lrn("classif.rpart", predict_type = "prob")
#' example_oof = csdg_resample(
#'   example_task,
#'   example_learner,
#'   mlr3::rsmp("cv", folds = 3L),
#'   store_models = TRUE,
#'   seed = 1L
#' )
#' csdg_local_support(example_oof, cases = example_oof$test_sets[[1L]][[1L]])
#' @export
csdg_local_support = function(
    x,
    cases,
    kernel_width = 0.75,
    case_labels = NULL,
    case_metadata = NULL) {
  assert_number(kernel_width, lower = .Machine$double.eps, finite = TRUE)
  case_map = .local_fidelity_prepare_cases(x, cases, case_labels, case_metadata)
  rbindlist(lapply(seq_len(nrow(case_map)), function(index) {
    .local_support_case(x, case_map[index], kernel_width)
  }))[]
}

.local_fidelity_case_result = function(
    x,
    case_map,
    seeds,
    crossfit_seeds,
    kernel_widths,
    primary_kernel_width,
    n_perturb,
    crossfit_folds,
    fidelity_threshold,
    target_scale,
    neighborhood_method,
    empirical_neighbors) {
  model_index = case_map$iteration[[1L]]
  case_data = .local_fidelity_case_data(x, case_map)
  case = case_data$case
  background = case_data$background
  imputed_features = case_data$imputed_features
  public_case = case_map[, setdiff(names(case_map), "row_id"), with = FALSE]

  support = cbind(
    public_case,
    data.table(
      n_case_values_imputed = length(imputed_features),
      case_imputed_features = paste(imputed_features, collapse = ",")
    ),
    .local_support_metrics(case, background, primary_kernel_width)
  )
  coefficient_store = new.env(parent = emptyenv())
  coefficient_store$records = list()
  replicates = rbindlist(lapply(seq_along(seeds), function(replicate) {
    seed = as.integer(seeds[[replicate]])
    crossfit_seed = as.integer(crossfit_seeds[[replicate]])
    neighborhood = if (identical(neighborhood_method, "synthetic")) {
      .make_local_neighborhood(case, background, as.integer(n_perturb), seed)
    } else {
      .make_empirical_local_neighborhood(case, background, as.integer(n_perturb), seed, empirical_neighbors)
    }
    neighborhood = rbindlist(list(case, neighborhood), fill = TRUE)
    prediction = .predict_newdata(x$models[[model_index]], neighborhood, task = x$task)
    predicted_outcome = .prediction_vector(prediction, x$task$task_type, x$task$positive)
    predicted_outcome = .local_prediction_scale(predicted_outcome, x$task$task_type, target_scale)
    distance = .local_distance(neighborhood, case, background)
    design = model.matrix(~ ., data = data.frame(neighborhood, check.names = TRUE))
    evaluation_rows = seq.int(2L, nrow(neighborhood))
    fold_id = c(
      NA_integer_,
      .local_crossfit_folds(length(evaluation_rows), crossfit_seed, folds = crossfit_folds)
    )

    rbindlist(lapply(kernel_widths, function(kernel_width) {
      weights = exp(pmax(-(distance^2) / kernel_width^2, log(.Machine$double.xmin)))
      crossfit = .local_crossfit_surrogate(design, predicted_outcome, weights, fold_id)
      crossfit_metrics = .local_weighted_metrics(
        predicted_outcome[evaluation_rows],
        crossfit$predicted[evaluation_rows],
        weights[evaluation_rows]
      )
      final_fit = .local_weighted_ridge(design, predicted_outcome, weights)
      target_fit = .local_weighted_ridge(
        design[evaluation_rows, , drop = FALSE],
        predicted_outcome[evaluation_rows],
        weights[evaluation_rows]
      )
      target_prediction = as.numeric(design[1L, , drop = FALSE] %*% target_fit$coefficients)
      apparent_metrics = .local_weighted_metrics(
        predicted_outcome[evaluation_rows],
        final_fit$fitted[evaluation_rows],
        weights[evaluation_rows]
      )
      if (abs(kernel_width - primary_kernel_width) < 1e-12) {
        coefficient_store$records[[length(coefficient_store$records) + 1L]] = cbind(
          public_case,
          data.table(
            perturbation_replicate = as.integer(replicate),
            perturbation_seed = seed,
            crossfit_seed = crossfit_seed,
            target_scale = target_scale,
            neighborhood_method = neighborhood_method,
            term = colnames(design),
            coefficient = as.numeric(final_fit$coefficients),
            absolute_rank = frank(-abs(final_fit$coefficients), ties.method = "average")
          )
        )
      }
      cbind(public_case, data.table(
        perturbation_replicate = as.integer(replicate),
        perturbation_seed = seed,
        crossfit_seed = crossfit_seed,
        kernel_width = kernel_width,
        analysis_role = if (abs(kernel_width - primary_kernel_width) < 1e-12) "primary" else "sensitivity",
        target_scale = target_scale,
        neighborhood_method = neighborhood_method,
        n_perturb = as.integer(n_perturb),
        n_crossfit_folds = length(crossfit$folds),
        weighted_r2 = crossfit_metrics$r2,
        weighted_rmse = crossfit_metrics$rmse,
        weighted_mae = crossfit_metrics$mae,
        maximum_absolute_error = crossfit_metrics$maximum_absolute_error,
        target_case_model_prediction = predicted_outcome[[1L]],
        target_case_surrogate_prediction = target_prediction,
        target_case_error = target_prediction - predicted_outcome[[1L]],
        target_case_absolute_error = abs(target_prediction - predicted_outcome[[1L]]),
        apparent_weighted_r2 = apparent_metrics$r2,
        apparent_weighted_rmse = apparent_metrics$rmse,
        perturbation_effective_n = final_fit$effective_n,
        crossfit_effective_n_min = min(crossfit$effective_n),
        crossfit_effective_n_max = max(crossfit$effective_n),
        n_design_terms = ncol(design),
        n_active_design_terms = final_fit$n_active_terms,
        n_case_values_imputed = length(imputed_features),
        fidelity_threshold = fidelity_threshold %||% NA_real_,
        meets_threshold = if (is.null(fidelity_threshold)) {
          NA
        } else {
          is.finite(crossfit_metrics$r2) && crossfit_metrics$r2 >= fidelity_threshold
        }
      ))
    }))
  }))

  finite_columns = c(
    "weighted_r2", "weighted_rmse", "weighted_mae", "maximum_absolute_error",
    "target_case_model_prediction", "target_case_surrogate_prediction", "target_case_error",
    "target_case_absolute_error", "apparent_weighted_r2", "apparent_weighted_rmse",
    "perturbation_effective_n", "crossfit_effective_n_min", "crossfit_effective_n_max"
  )
  if (any(!is.finite(as.matrix(replicates[, ..finite_columns])))) {
    .csdg_stop("A local-fidelity replicate contains a non-finite metric.")
  }
  list(
    replicates = replicates[],
    support = support[],
    coefficients = rbindlist(coefficient_store$records, fill = TRUE)
  )
}

.local_fidelity_spearman = function(x, y) {
  keep = is.finite(x) & is.finite(y)
  if (sum(keep) < 2L || length(unique(x[keep])) < 2L || length(unique(y[keep])) < 2L) {
    return(NA_real_)
  }
  cor(x[keep], y[keep], method = "spearman")
}

.local_fidelity_distribution_row = function(
    values,
    scope,
    kernel_width,
    n_cases,
    n_replicates,
    n_underlying_local_fits,
    fidelity_threshold,
    threshold_applicable = TRUE) {
  data.table(
    scope = scope,
    kernel_width = kernel_width,
    n_values = length(values),
    n_cases = n_cases,
    perturbation_replicates_per_case = n_replicates,
    n_underlying_local_fits = n_underlying_local_fits,
    minimum = min(values),
    first_quartile = .local_fidelity_quantile(values, 0.25),
    median = median(values),
    third_quartile = .local_fidelity_quantile(values, 0.75),
    maximum = max(values),
    mean = mean(values),
    standard_deviation = sd(values),
    threshold_applicable = threshold_applicable,
    n_below_threshold = if (threshold_applicable) sum(values < fidelity_threshold) else NA_integer_,
    proportion_below_threshold = if (threshold_applicable) mean(values < fidelity_threshold) else NA_real_,
    fidelity_threshold = if (threshold_applicable) fidelity_threshold else NA_real_
  )
}

.summarize_local_fidelity_audit = function(
    replicates,
    support,
    coefficients,
    primary_kernel_width,
    fidelity_threshold) {
  threshold = fidelity_threshold %||% NA_real_
  threshold_applicable = !is.null(fidelity_threshold)
  case_keys = c("case_label", "iteration", "repetition", "fold")
  n_replicates = length(unique(replicates$perturbation_replicate))
  primary = replicates[abs(kernel_width - primary_kernel_width) < 1e-12]
  cases = primary[, .(
    n_perturbation_replicates = .N,
    median_weighted_r2 = median(weighted_r2),
    median_weighted_rmse = median(weighted_rmse),
    median_weighted_mae = median(weighted_mae),
    median_maximum_absolute_error = median(maximum_absolute_error),
    median_target_case_absolute_error = median(target_case_absolute_error),
    maximum_target_case_absolute_error = max(target_case_absolute_error),
    mean_weighted_r2 = mean(weighted_r2),
    minimum_weighted_r2_across_seeds = min(weighted_r2),
    maximum_weighted_r2_across_seeds = max(weighted_r2),
    monte_carlo_sd = sd(weighted_r2),
    monte_carlo_se = sd(weighted_r2) / sqrt(.N),
    monte_carlo_range = max(weighted_r2) - min(weighted_r2),
    proportion_replicates_below_threshold = if (threshold_applicable) {
      mean(weighted_r2 < fidelity_threshold)
    } else {
      NA_real_
    }
  ), by = .(case_label, iteration, repetition, fold)]
  cases[, `:=`(
    fidelity_threshold = threshold,
    case_below_threshold = if (threshold_applicable) median_weighted_r2 < threshold else NA,
    case_summary_rule = if (threshold_applicable) {
      "Median weighted R-squared across perturbation seeds compared with the supplied criterion"
    } else {
      "Median weighted R-squared across perturbation seeds; no adequacy criterion supplied"
    }
  )]
  cases = merge(
    cases,
    support,
    by = case_keys,
    all.x = TRUE,
    sort = FALSE
  )
  cases[, proximity_quartile := {
    distance_rank = rank(tenth_nearest_training_distance, ties.method = "first")
    1L + floor((distance_rank - 1L) * 4L / .N)
  }]
  cases = cases[order(iteration, case_label)]

  bandwidth_cases = replicates[, .(
    n_perturbation_replicates = .N,
    median_weighted_r2 = median(weighted_r2),
    median_weighted_rmse = median(weighted_rmse),
    median_weighted_mae = median(weighted_mae),
    median_maximum_absolute_error = median(maximum_absolute_error),
    median_target_case_absolute_error = median(target_case_absolute_error),
    case_below_threshold = if (threshold_applicable) median(weighted_r2) < threshold else NA
  ), by = .(case_label, iteration, repetition, fold, kernel_width, analysis_role)]
  support_metrics = c(
    "n_case_values_imputed", "case_imputed_features", "support_kernel_width", "nearest_training_distance",
    "tenth_nearest_training_distance", "fiftieth_nearest_training_distance", "proportion_training_within_kernel",
    "kernel_weight_effective_n", "n_training_background"
  )
  metadata_columns = setdiff(names(support), c(case_keys, support_metrics))
  if (length(metadata_columns)) {
    metadata = unique(support[, c(case_keys, metadata_columns), with = FALSE])
    bandwidth_cases = merge(bandwidth_cases, metadata, by = case_keys, all.x = TRUE, sort = FALSE)
  }
  bandwidth_cases[, fidelity_threshold := threshold]
  bandwidth_cases = bandwidth_cases[order(iteration, case_label, kernel_width)]

  widths = sort(unique(replicates$kernel_width))
  summary = rbindlist(c(
    list(
      .local_fidelity_distribution_row(
        cases$median_weighted_r2,
        "primary case medians",
        primary_kernel_width,
        nrow(cases),
        n_replicates,
        nrow(primary),
        fidelity_threshold,
        threshold_applicable = threshold_applicable
      ),
      .local_fidelity_distribution_row(
        primary$weighted_r2,
        "primary perturbation replicates",
        primary_kernel_width,
        nrow(cases),
        n_replicates,
        nrow(primary),
        fidelity_threshold,
        threshold_applicable = threshold_applicable
      ),
      .local_fidelity_distribution_row(
        cases$monte_carlo_sd,
        "case-level Monte Carlo standard deviations",
        primary_kernel_width,
        nrow(cases),
        n_replicates,
        nrow(primary),
        fidelity_threshold,
        threshold_applicable = FALSE
      )
    ),
    lapply(widths[abs(widths - primary_kernel_width) >= 1e-12], function(width) {
      values = bandwidth_cases[
        abs(kernel_width - width) < 1e-12,
        median_weighted_r2
      ]
      .local_fidelity_distribution_row(
        values,
        "bandwidth-sensitivity case medians",
        width,
        length(values),
        n_replicates,
        nrow(replicates[abs(kernel_width - width) < 1e-12]),
        fidelity_threshold,
        threshold_applicable = threshold_applicable
      )
    })
  ))
  summary[, uncertainty_semantics := paste(
    "Descriptive distributions only. Variation across perturbation seeds jointly reflects synthetic-neighborhood",
    paste(
      "draws and deterministic surrogate cross-fit partitions, not model-fit or sampling uncertainty;",
      "no confidence intervals or hypothesis tests."
    )
  )]

  by_support = cases[, .(
    n_cases = .N,
    minimum_tenth_nearest_training_distance = min(tenth_nearest_training_distance),
    median_tenth_nearest_training_distance = median(tenth_nearest_training_distance),
    maximum_tenth_nearest_training_distance = max(tenth_nearest_training_distance),
    median_nearest_training_distance = median(nearest_training_distance),
    median_proportion_training_within_kernel = median(proportion_training_within_kernel),
    median_kernel_weight_effective_n = median(kernel_weight_effective_n),
    median_case_median_r2 = median(median_weighted_r2),
    first_quartile_case_median_r2 = .local_fidelity_quantile(median_weighted_r2, 0.25),
    third_quartile_case_median_r2 = .local_fidelity_quantile(median_weighted_r2, 0.75),
    proportion_cases_below_threshold = if (threshold_applicable) mean(case_below_threshold) else NA_real_
  ), by = proximity_quartile]
  by_support[, `:=`(
    fidelity_threshold = threshold,
    spearman_rho_tenth_nearest_distance_all_cases = .local_fidelity_spearman(
      cases$tenth_nearest_training_distance,
      cases$median_weighted_r2
    ),
    spearman_rho_proportion_within_kernel_all_cases = .local_fidelity_spearman(
      cases$proportion_training_within_kernel,
      cases$median_weighted_r2
    ),
    inference = paste(
      "Quartile 1 has the nearest tenth training neighbor; greater distance means weaker observed training-set",
      paste(
        "proximity under the surrogate mixed-type distance. Associations are unadjusted, and covariates are not",
        "controlled. Descriptive only, not proof of joint support, with no p-value or confidence interval. Kish",
        "effective N describes only kernel-weight concentration."
      )
    )
  )]
  by_support = by_support[order(proximity_quartile)]
  coefficient_stability = if (nrow(coefficients)) {
    result = coefficients[, .(
      n_perturbation_replicates = .N,
      median_coefficient = median(coefficient),
      q10_coefficient = quantile(coefficient, 0.10, names = FALSE, type = 8),
      q90_coefficient = quantile(coefficient, 0.90, names = FALSE, type = 8),
      positive_fraction = mean(coefficient > 0),
      negative_fraction = mean(coefficient < 0),
      zero_fraction = mean(coefficient == 0),
      sign_consistency = max(mean(coefficient > 0), mean(coefficient < 0), mean(coefficient == 0)),
      median_absolute_rank = median(absolute_rank),
      absolute_rank_sd = sd(absolute_rank)
    ), by = .(case_label, iteration, repetition, fold, target_scale, neighborhood_method, term)]
    result[order(iteration, case_label, median_absolute_rank, term)]
  } else {
    data.table()
  }
  list(
    summary = summary[],
    cases = cases[],
    bandwidth_cases = bandwidth_cases[],
    by_support = by_support[],
    coefficient_stability = coefficient_stability[]
  )
}

.local_fidelity_require_columns = function(x, required, name) {
  missing = setdiff(required, names(x))
  if (length(missing)) {
    .csdg_stop("`%s` is missing required columns: %s.", name, paste(missing, collapse = ", "))
  }
  if (anyDuplicated(names(x)) || any(!nzchar(names(x)))) {
    .csdg_stop("`%s` must have unique, nonempty column names.", name)
  }
  invisible(TRUE)
}

.local_fidelity_validate_locations = function(x, name) {
  assert_character(x$case_label, any.missing = FALSE, min.chars = 1L, .var.name = paste0(name, "$case_label"))
  for (column in c("iteration", "repetition", "fold")) {
    assert_integerish(x[[column]], lower = 1L, any.missing = FALSE, .var.name = paste0(name, "$", column))
  }
  locations = unique(x[, .(case_label, iteration, repetition, fold)])
  if (any(locations[, .N, by = case_label]$N != 1L)) {
    .csdg_stop("Each `case_label` in `%s` must identify exactly one held-out case location.", name)
  }
  iteration_locations = unique(locations[, .(iteration, repetition, fold)])
  if (any(iteration_locations[, .N, by = iteration]$N != 1L)) {
    .csdg_stop("Each `iteration` in `%s` must identify exactly one repetition and fold.", name)
  }
  locations[]
}

.local_fidelity_same_keys = function(x, y, keys) {
  nrow(x) == nrow(y) && nrow(merge(x, y, by = keys, all = FALSE)) == nrow(x)
}

#' Summarize one or more systematic local-fidelity audits
#'
#' Validates and combines the public replicate and support tables returned by one or more calls to
#' [csdg_local_fidelity_audit()].
#' Every held-out case must have the same complete replicate-by-kernel-width grid.
#'
#' @param replicates A nonempty data frame formed by row-binding `replicates` tables from compatible
#'   [csdg_local_fidelity_audit()] results.
#' @param support A nonempty data frame formed by row-binding the corresponding `support` tables.
#' @param coefficients Optional coefficient tables from compatible audits.
#' @param primary_kernel_width The prespecified primary width, which must occur exactly once among the distinct widths.
#' @param fidelity_threshold Optional, claim- and use-specific weighted R-squared criterion used only for descriptive
#'   classification.
#'   When `NULL`, continuous relative and absolute fidelity metrics are returned without a pass/fail label.
#'
#' @return A named list containing aggregate `summary`, primary case-level `cases`, per-width `bandwidth_cases`,
#'   and descriptive `by_support` tables.
#' @examplesIf requireNamespace("mlr3learners", quietly = TRUE)
#' set.seed(1L)
#' example_data = data.frame(x1 = rnorm(80L), x2 = rnorm(80L))
#' example_data$y = factor(ifelse(
#'   runif(80L) < plogis(example_data$x1 - example_data$x2), "yes", "no"
#' ))
#' example_task = mlr3::as_task_classif(example_data, target = "y", positive = "yes")
#' example_oof = csdg_resample(
#'   example_task,
#'   mlr3::lrn("classif.log_reg", predict_type = "prob"),
#'   mlr3::rsmp("cv", folds = 2L),
#'   store_models = TRUE,
#'   seed = 1L
#' )
#' example_audit = csdg_local_fidelity_audit(
#'   example_oof,
#'   cases = example_oof$test_sets[[1L]][[1L]],
#'   seeds = c(11L, 12L),
#'   kernel_widths = c(0.75, 1.00),
#'   n_perturb = 50L
#' )
#' csdg_summarize_local_fidelity(example_audit$replicates, example_audit$support)$cases
#' @export
csdg_summarize_local_fidelity = function(
    replicates,
    support,
    coefficients = NULL,
    primary_kernel_width = 0.75,
    fidelity_threshold = NULL) {
  assert_data_frame(replicates, min.rows = 1L, .var.name = "replicates")
  assert_data_frame(support, min.rows = 1L, .var.name = "support")
  assert_number(primary_kernel_width, lower = .Machine$double.eps, finite = TRUE)
  if (!is.null(fidelity_threshold)) {
    assert_number(fidelity_threshold, lower = 0, upper = 1, finite = TRUE)
  }
  replicates = .as_dt(replicates)
  support = .as_dt(support)
  coefficients = if (is.null(coefficients)) data.table() else .as_dt(coefficients)

  case_keys = c("case_label", "iteration", "repetition", "fold")
  replicate_columns = c(
    case_keys, "perturbation_replicate", "perturbation_seed", "crossfit_seed", "kernel_width", "analysis_role",
    "target_scale", "neighborhood_method", "n_perturb", "n_crossfit_folds", "weighted_r2", "weighted_rmse",
    "weighted_mae", "maximum_absolute_error", "target_case_model_prediction",
    "target_case_surrogate_prediction", "target_case_error", "target_case_absolute_error",
    "apparent_weighted_r2", "apparent_weighted_rmse",
    "perturbation_effective_n", "crossfit_effective_n_min", "crossfit_effective_n_max", "n_design_terms",
    "n_active_design_terms", "n_case_values_imputed", "fidelity_threshold", "meets_threshold"
  )
  support_metrics = c(
    "n_case_values_imputed", "case_imputed_features", "support_kernel_width", "nearest_training_distance",
    "tenth_nearest_training_distance", "fiftieth_nearest_training_distance", "proportion_training_within_kernel",
    "kernel_weight_effective_n", "n_training_background"
  )
  .local_fidelity_require_columns(replicates, replicate_columns, "replicates")
  .local_fidelity_require_columns(support, c(case_keys, support_metrics), "support")
  replicate_locations = .local_fidelity_validate_locations(replicates, "replicates")
  support_locations = .local_fidelity_validate_locations(support, "support")

  integer_lowers = c(
    perturbation_replicate = 1L,
    perturbation_seed = 0L,
    crossfit_seed = 0L,
    n_perturb = 50L,
    n_crossfit_folds = 2L,
    n_design_terms = 1L,
    n_active_design_terms = 0L,
    n_case_values_imputed = 0L
  )
  for (column in names(integer_lowers)) {
    assert_integerish(
      replicates[[column]],
      lower = integer_lowers[[column]],
      any.missing = FALSE,
      .var.name = paste0("replicates$", column)
    )
  }
  if (any(replicates$perturbation_seed > .Machine$integer.max - 100000L) ||
      any(replicates$n_crossfit_folds > replicates$n_perturb) ||
      any(replicates$n_active_design_terms > replicates$n_design_terms)) {
    .csdg_stop("`replicates` contains invalid perturbation, cross-fitting, or design-term counts.")
  }
  assert_numeric(
    replicates$kernel_width,
    lower = .Machine$double.eps,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "replicates$kernel_width"
  )
  finite_metrics = c(
    "weighted_r2", "weighted_rmse", "weighted_mae", "maximum_absolute_error",
    "target_case_model_prediction", "target_case_surrogate_prediction", "target_case_error",
    "target_case_absolute_error", "apparent_weighted_r2", "apparent_weighted_rmse",
    "perturbation_effective_n", "crossfit_effective_n_min", "crossfit_effective_n_max"
  )
  for (column in finite_metrics) {
    lower = if (column %in% c(
      "weighted_r2", "apparent_weighted_r2", "target_case_model_prediction",
      "target_case_surrogate_prediction", "target_case_error"
    )) {
      -Inf
    } else if (column %in% c(
      "weighted_rmse", "weighted_mae", "maximum_absolute_error", "target_case_absolute_error",
      "apparent_weighted_rmse"
    )) {
      0
    } else {
      .Machine$double.eps
    }
    assert_numeric(
      replicates[[column]],
      lower = lower,
      finite = TRUE,
      any.missing = FALSE,
      .var.name = paste0("replicates$", column)
    )
  }
  if (any(replicates$crossfit_effective_n_min > replicates$crossfit_effective_n_max)) {
    .csdg_stop("`replicates` has cross-fit effective-sample-size minima above maxima.")
  }
  assert_character(
    replicates$analysis_role,
    any.missing = FALSE,
    min.chars = 1L,
    .var.name = "replicates$analysis_role"
  )
  assert_subset(unique(replicates$target_scale), c("response", "link"), empty.ok = FALSE)
  assert_subset(
    unique(replicates$neighborhood_method),
    c("synthetic", "empirical_knn"),
    empty.ok = FALSE
  )
  assert_subset(
    unique(replicates$analysis_role),
    c("primary", "sensitivity"),
    empty.ok = FALSE,
    .var.name = "replicates$analysis_role"
  )
  if (is.null(fidelity_threshold)) {
    if (any(!is.na(replicates$fidelity_threshold)) || any(!is.na(replicates$meets_threshold))) {
      .csdg_stop(
        "`fidelity_threshold` must be supplied when the replicate tables contain threshold classifications."
      )
    }
  } else {
    assert_numeric(
      replicates$fidelity_threshold,
      lower = 0,
      upper = 1,
      finite = TRUE,
      any.missing = FALSE,
      .var.name = "replicates$fidelity_threshold"
    )
    assert_logical(replicates$meets_threshold, any.missing = FALSE, .var.name = "replicates$meets_threshold")
  }

  widths = sort(unique(replicates$kernel_width))
  if (length(widths) > 1L && any(diff(widths) < 1e-12)) {
    .csdg_stop("`replicates$kernel_width` contains widths that are not distinct at the audit tolerance.")
  }
  primary_match = which(abs(widths - primary_kernel_width) < 1e-12)
  if (length(primary_match) != 1L) {
    .csdg_stop("`primary_kernel_width` must occur exactly once among the distinct replicate widths.")
  }
  primary_kernel_width = widths[[primary_match]]
  expected_role = ifelse(abs(replicates$kernel_width - primary_kernel_width) < 1e-12, "primary", "sensitivity")
  if (any(replicates$analysis_role != expected_role)) {
    .csdg_stop("`replicates$analysis_role` is inconsistent with `primary_kernel_width`.")
  }
  if (!is.null(fidelity_threshold)) {
    if (any(abs(replicates$fidelity_threshold - fidelity_threshold) >= 1e-12)) {
      .csdg_stop("`replicates$fidelity_threshold` is inconsistent with `fidelity_threshold`.")
    }
    expected_threshold_flag = replicates$weighted_r2 >= replicates$fidelity_threshold
    if (any(replicates$meets_threshold != expected_threshold_flag)) {
      .csdg_stop("`replicates$meets_threshold` is inconsistent with the raw weighted R-squared threshold rule.")
    }
  }

  replicate_indices = sort(unique(as.integer(replicates$perturbation_replicate)))
  if (length(replicate_indices) < 2L || !identical(replicate_indices, seq_along(replicate_indices))) {
    .csdg_stop("`replicates` must contain at least two common, consecutive perturbation replicate indices.")
  }
  grid_counts = replicates[, .N, by = c(case_keys, "perturbation_replicate", "kernel_width")]
  expected_rows = nrow(replicate_locations) * length(replicate_indices) * length(widths)
  if (nrow(replicates) != expected_rows || nrow(grid_counts) != expected_rows || any(grid_counts$N != 1L)) {
    .csdg_stop("`replicates` must form an exact common case-by-replicate-by-width grid without duplicates.")
  }
  case_grid = grid_counts[, .(
    n_replicates = length(unique(perturbation_replicate)),
    n_widths = length(unique(kernel_width))
  ), by = case_keys]
  if (any(case_grid$n_replicates != length(replicate_indices)) || any(case_grid$n_widths != length(widths))) {
    .csdg_stop("`replicates` must use the same replicate indices and kernel widths for every case.")
  }

  seed_grid = replicates[, .(n_seeds = length(unique(perturbation_seed))),
    by = c(case_keys, "perturbation_replicate")]
  if (any(seed_grid$n_seeds != 1L)) {
    .csdg_stop("Each case and perturbation replicate must use one stable seed across all kernel widths.")
  }
  case_seeds = unique(replicates[, c(case_keys, "perturbation_replicate", "perturbation_seed"), with = FALSE])
  distinct_seeds = case_seeds[, .(n_seeds = length(unique(perturbation_seed))), by = case_keys]
  if (any(distinct_seeds$n_seeds != length(replicate_indices))) {
    .csdg_stop("Each case must use distinct perturbation seeds across replicate indices.")
  }
  crossfit_seed_grid = unique(
    replicates[, c(case_keys, "perturbation_replicate", "crossfit_seed"), with = FALSE]
  )
  if (nrow(crossfit_seed_grid) != nrow(case_seeds)) {
    .csdg_stop("Each case and perturbation replicate must use one cross-fit seed across kernel widths.")
  }
  stable_columns = c(
    "n_perturb", "n_crossfit_folds", "n_design_terms", "n_active_design_terms", "n_case_values_imputed"
  )
  stable_grid = replicates[, lapply(.SD, function(value) length(unique(value))),
    by = c(case_keys, "perturbation_replicate"), .SDcols = stable_columns]
  if (any(as.matrix(stable_grid[, ..stable_columns]) != 1L)) {
    .csdg_stop("Replicate design and case-imputation fields must be stable across kernel widths.")
  }

  assert_integerish(
    support$n_case_values_imputed,
    lower = 0L,
    any.missing = FALSE,
    .var.name = "support$n_case_values_imputed"
  )
  assert_character(
    support$case_imputed_features,
    any.missing = FALSE,
    .var.name = "support$case_imputed_features"
  )
  assert_numeric(
    support$support_kernel_width,
    lower = .Machine$double.eps,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "support$support_kernel_width"
  )
  for (column in c(
    "nearest_training_distance", "tenth_nearest_training_distance", "fiftieth_nearest_training_distance"
  )) {
    assert_numeric(
      support[[column]],
      lower = 0,
      finite = TRUE,
      any.missing = FALSE,
      .var.name = paste0("support$", column)
    )
  }
  assert_numeric(
    support$kernel_weight_effective_n,
    lower = .Machine$double.eps,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "support$kernel_weight_effective_n"
  )
  assert_numeric(
    support$proportion_training_within_kernel,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "support$proportion_training_within_kernel"
  )
  assert_integerish(
    support$n_training_background,
    lower = 1L,
    any.missing = FALSE,
    .var.name = "support$n_training_background"
  )
  invalid_support = support$nearest_training_distance > support$tenth_nearest_training_distance |
    support$tenth_nearest_training_distance > support$fiftieth_nearest_training_distance |
    support$kernel_weight_effective_n > support$n_training_background + 1e-12 |
    abs(support$support_kernel_width - primary_kernel_width) >= 1e-12
  if (any(invalid_support)) {
    .csdg_stop("`support` contains invalid or primary-width-inconsistent support metrics.")
  }
  support_counts = support[, .N, by = case_keys]
  if (nrow(support) != nrow(support_locations) || any(support_counts$N != 1L) ||
      !.local_fidelity_same_keys(replicate_locations, support_locations, case_keys)) {
    .csdg_stop("`support` must contain exactly one row matching every replicate case location.")
  }
  imputation = unique(replicates[, c(case_keys, "n_case_values_imputed"), with = FALSE])
  if (nrow(imputation) != nrow(replicate_locations)) {
    .csdg_stop("`replicates$n_case_values_imputed` must be constant within each case location.")
  }
  imputation = merge(
    imputation,
    support[, c(case_keys, "n_case_values_imputed"), with = FALSE],
    by = case_keys,
    suffixes = c(".replicates", ".support"),
    sort = FALSE
  )
  if (any(imputation$n_case_values_imputed.replicates != imputation$n_case_values_imputed.support)) {
    .csdg_stop("Case-imputation counts must agree between `replicates` and `support`.")
  }

  if (nrow(coefficients)) {
    coefficient_columns = c(
      case_keys, "perturbation_replicate", "target_scale", "neighborhood_method", "term", "coefficient",
      "absolute_rank"
    )
    .local_fidelity_require_columns(coefficients, coefficient_columns, "coefficients")
  }
  .summarize_local_fidelity_audit(
    replicates,
    support,
    coefficients,
    primary_kernel_width,
    fidelity_threshold
  )
}

#' Audit held-out local-surrogate fidelity systematically
#'
#' Evaluates multiple prespecified held-out cases with their corresponding fold models across multiple perturbation
#' seeds and kernel widths.
#'
#' Synthetic or empirical-neighbor neighborhoods, mixed-type distances, weighted ridge fitting, cross-fitting,
#' relative and absolute errors, target-case error, coefficient stability, and support diagnostics are reported.
#'
#' @param x A `CSDGResample` with stored fold models.
#' @param cases Unique task row ids that each occur in exactly one assessment split.
#' @param seeds Either a vector of at least two distinct perturbation seeds, reused for every case, or an integer-like
#'   matrix with one row per case and one distinct seed per replicate column.
#' @param crossfit_seeds Optional vector or case-by-replicate matrix of cross-fit partition seeds.
#'   When `NULL`, seeds distinct from `seeds` are generated deterministically.
#' @param kernel_widths Unique positive kernel widths.
#' @param primary_kernel_width The prespecified primary width, which must occur in `kernel_widths`.
#' @param n_perturb Number of synthetic perturbations per case and seed.
#' @param crossfit_folds Number of deterministic surrogate cross-fitting folds.
#' @param fidelity_threshold Optional, claim- and use-specific weighted R-squared criterion used only for descriptive
#'   classification.
#'   When `NULL`, no binary fidelity label is created.
#' @param target_scale For classification, either `"response"` for probability or `"link"` for logit scale.
#' @param neighborhood_method Either synthetic independent perturbations or empirical k-nearest-neighbor resampling.
#' @param empirical_neighbors Number of nearest training rows eligible for empirical-neighbor resampling.
#' @param case_labels Optional unique pseudonymous labels in the same order as `cases`.
#' @param case_metadata Optional data frame with one row per case and columns, such as a prespecified risk stratum,
#'   that are copied to the replicate, support, and case-level outputs.
#'
#' @return A named list containing the complete `replicates` grid, `support` diagnostics, primary `cases` summaries,
#'   per-width `bandwidth_cases`, aggregate `summary`, descriptive `by_support` summaries, the private `case_map`,
#'   evaluation metadata, target scale, and limitations.
#' @examplesIf requireNamespace("mlr3learners", quietly = TRUE)
#' set.seed(1L)
#' example_x1 = rnorm(120L)
#' example_x2 = rnorm(120L)
#' example_probability = plogis(1.5 * example_x1 - 0.7 * example_x2)
#' example_data = data.frame(
#'   x1 = example_x1,
#'   x2 = example_x2,
#'   y = factor(ifelse(runif(120L) < example_probability, "yes", "no"))
#' )
#' example_task = mlr3::as_task_classif(example_data, target = "y", positive = "yes")
#' example_learner = mlr3::lrn("classif.log_reg", predict_type = "prob")
#' example_oof = csdg_resample(
#'   example_task,
#'   example_learner,
#'   mlr3::rsmp("cv", folds = 3L),
#'   store_models = TRUE,
#'   seed = 1L
#' )
#' audited_cases = c(example_oof$test_sets[[1L]][[1L]], example_oof$test_sets[[2L]][[1L]])
#' audit = csdg_local_fidelity_audit(
#'   example_oof,
#'   cases = audited_cases,
#'   seeds = c(101L, 102L),
#'   kernel_widths = c(0.75, 1.00),
#'   n_perturb = 50L
#' )
#' audit$summary
#' @export
csdg_local_fidelity_audit = function(
    x,
    cases,
    seeds = 20260201L + 0:19,
    crossfit_seeds = NULL,
    kernel_widths = c(0.50, 0.75, 1.00),
    primary_kernel_width = 0.75,
    n_perturb = 500L,
    crossfit_folds = 5L,
    fidelity_threshold = NULL,
    target_scale = c("response", "link"),
    neighborhood_method = c("synthetic", "empirical_knn"),
    empirical_neighbors = n_perturb,
    case_labels = NULL,
    case_metadata = NULL) {
  assert_numeric(
    kernel_widths,
    lower = .Machine$double.eps,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1L,
    unique = TRUE
  )
  assert_number(primary_kernel_width, lower = .Machine$double.eps, finite = TRUE)
  assert_int(n_perturb, lower = 50L)
  assert_int(crossfit_folds, lower = 2L, upper = n_perturb)
  if (!is.null(fidelity_threshold)) {
    assert_number(fidelity_threshold, lower = 0, upper = 1, finite = TRUE)
  }
  target_scale = match.arg(target_scale)
  neighborhood_method = match.arg(neighborhood_method)
  assert_int(empirical_neighbors, lower = 2L)
  if (identical(x$task_type, "regr") && identical(target_scale, "link")) {
    .csdg_stop("`target_scale = \"link\"` is available only for binary classification.")
  }
  kernel_widths = sort(as.numeric(kernel_widths))
  primary_match = which(abs(kernel_widths - primary_kernel_width) < 1e-12)
  if (length(primary_match) != 1L) {
    .csdg_stop("`primary_kernel_width` must occur exactly once in `kernel_widths`.")
  }
  primary_kernel_width = kernel_widths[[primary_match]]
  case_map = .local_fidelity_prepare_cases(x, cases, case_labels, case_metadata)
  seed_matrix = .local_fidelity_seed_matrix(seeds, nrow(case_map))
  crossfit_seed_matrix = if (is.null(crossfit_seeds)) {
    seed_matrix + 100000L
  } else {
    .local_fidelity_seed_matrix(crossfit_seeds, nrow(case_map))
  }
  if (!identical(dim(seed_matrix), dim(crossfit_seed_matrix))) {
    .csdg_stop("`crossfit_seeds` must provide the same case-by-replicate grid as `seeds`.")
  }

  had_random_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_random_seed) {
    caller_random_seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_random_seed) {
      assign(".Random.seed", caller_random_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      remove(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  evaluated = lapply(seq_len(nrow(case_map)), function(index) {
    .local_fidelity_case_result(
      x = x,
      case_map = case_map[index],
      seeds = seed_matrix[index, ],
      crossfit_seeds = crossfit_seed_matrix[index, ],
      kernel_widths = kernel_widths,
      primary_kernel_width = primary_kernel_width,
      n_perturb = n_perturb,
      crossfit_folds = crossfit_folds,
      fidelity_threshold = fidelity_threshold,
      target_scale = target_scale,
      neighborhood_method = neighborhood_method,
      empirical_neighbors = empirical_neighbors
    )
  })
  replicates = rbindlist(lapply(evaluated, `[[`, "replicates"))
  support = rbindlist(lapply(evaluated, `[[`, "support"))
  coefficients = rbindlist(lapply(evaluated, `[[`, "coefficients"), fill = TRUE)
  expected_rows = nrow(case_map) * ncol(seed_matrix) * length(kernel_widths)
  keys = replicates[, .N, by = .(case_label, perturbation_replicate, kernel_width)]
  if (nrow(replicates) != expected_rows || nrow(keys) != expected_rows || any(keys$N != 1L) ||
      nrow(support) != nrow(case_map) || anyDuplicated(support$case_label)) {
    .csdg_stop("The case-by-seed-by-bandwidth local-fidelity grid is incomplete or duplicated.")
  }
  summaries = csdg_summarize_local_fidelity(
    replicates,
    support,
    coefficients,
    primary_kernel_width,
    fidelity_threshold
  )
  list(
    replicates = replicates[],
    support = support[],
    coefficients = coefficients[],
    cases = summaries$cases,
    bandwidth_cases = summaries$bandwidth_cases,
    summary = summaries$summary,
    by_support = summaries$by_support,
    coefficient_stability = summaries$coefficient_stability,
    case_map = case_map[],
    evaluation = list(
      method = "deterministic weighted ridge cross-fitting",
      crossfit_folds = as.integer(crossfit_folds),
      fold_assignment = "balanced seeded permutation, independently generated for each case and seed",
      score_scope = paste(
        "Cross-fitted metrics score perturbations; target-case error is scored from a surrogate fitted",
        "without the original case"
      ),
      target_scale = target_scale,
      neighborhood_method = neighborhood_method,
      empirical_neighbors = if (identical(neighborhood_method, "empirical_knn")) empirical_neighbors else NULL,
      case_summary_rule = "median weighted R-squared across perturbation seeds",
      quartile_type = 8L,
      primary_kernel_width = primary_kernel_width,
      kernel_widths = kernel_widths,
      fidelity_threshold = fidelity_threshold,
      support_scope = "distances from each imputed case to its corresponding fold-training feature data",
      support_metrics = c(
        "nearest_training_distance", "tenth_nearest_training_distance",
        "fiftieth_nearest_training_distance", "proportion_training_within_kernel",
        "kernel_weight_effective_n", "n_training_background"
      ),
      metric_fields = c(
        "weighted_r2", "weighted_rmse", "weighted_mae", "maximum_absolute_error",
        "target_case_error", "target_case_absolute_error"
      ),
      apparent_metric_fields = c("apparent_weighted_r2", "apparent_weighted_rmse")
    ),
    target_scale = if (identical(x$task_type, "classif") && identical(target_scale, "link")) {
      "positive-class logit"
    } else if (identical(x$task_type, "classif")) {
      "positive-class predicted probability"
    } else {
      "predicted outcome"
    },
    limitations = c(
      "Each diagnostic uses a fold model that did not train on the evaluated case.",
      paste(
        "Each perturbation is scored by a cross-fitted surrogate that did not fit that perturbation;",
        "target-case error is scored using a surrogate fitted without the original held-out case."
      ),
      "Neighborhoods are generated from the corresponding fold-training data and remain perturbation-dependent.",
      paste(
        "Perturbation and cross-fit seeds are recorded separately so their variation can be isolated;",
        "neither source represents model-training or sampling uncertainty."
      ),
      paste(
        sprintf("Weighted R-squared is relative to model-prediction variance in the %s local neighborhood;",
          neighborhood_method),
        "weighted RMSE, MAE, maximum error, and target-case error supply absolute scale-dependent diagnostics."
      ),
      if (identical(neighborhood_method, "empirical_knn")) {
        paste(
          "Missing values in sampled empirical neighbors were completed with fold-training medians or modes",
          "before prediction and surrogate encoding."
        )
      },
      "Coefficient sign and absolute-rank stability are reported across perturbation repetitions.",
      paste(
        "Training-distance and kernel-weight support measures are descriptive, unadjusted diagnostics;",
        "they do not establish joint empirical support or transport."
      ),
      "Local fidelity does not make an explanation causal, actionable, or reliable outside the audited cases."
    )
  )
}
