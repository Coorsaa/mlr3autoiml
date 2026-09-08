# Bootstrap uncertainty for one-dimensional accumulated local effects.

.csdg_ale_bootstrap_grid = function(values, bins, trim) {
  numeric_values = suppressWarnings(as.numeric(values))
  numeric_values = numeric_values[is.finite(numeric_values)]
  unique_values = sort(unique(numeric_values))
  if (length(unique_values) <= bins + 1L) {
    return(unique_values)
  }
  .autoiml_grid_1d_iml(
    numeric_values,
    grid_n = bins + 1L,
    grid_type = "quantile",
    trim = trim
  )
}

.csdg_ale_bootstrap_row_map = function(task, resampling_unit, cluster, strata) {
  row_ids = task$row_ids
  if (resampling_unit == "row") {
    if (!is.null(cluster) || !is.null(strata)) {
      .csdg_stop("`cluster` and `strata` must be NULL when `resampling_unit = \"row\"`.")
    }
    return(NULL)
  }

  cluster = .normalize_named_vector(cluster, row_ids, "cluster")
  strata = .normalize_named_vector(strata, row_ids, "strata")
  if (is.null(cluster)) {
    .csdg_stop("`cluster` is required when `resampling_unit = \"cluster\"`.")
  }
  if (anyNA(cluster)) {
    .csdg_stop("`cluster` must not contain missing values.")
  }
  if (is.null(strata)) {
    strata = rep("__all__", length(row_ids))
  }
  if (anyNA(strata)) {
    .csdg_stop("`strata` must not contain missing values.")
  }

  row_map = data.table(
    row_position = seq_along(row_ids),
    bootstrap_cluster = as.character(cluster),
    bootstrap_stratum = as.character(strata)
  )
  mixed = row_map[, .(n_strata = uniqueN(bootstrap_stratum)), by = bootstrap_cluster][n_strata > 1L]
  if (nrow(mixed)) {
    .csdg_stop(
      "Each bootstrap cluster must belong to one stratum; mixed clusters: %s.",
      paste(mixed$bootstrap_cluster, collapse = ", ")
    )
  }
  row_map
}

.csdg_ale_bootstrap_positions = function(task, resampling_unit, row_map) {
  if (resampling_unit == "row") {
    return(sample.int(task$nrow, size = task$nrow, replace = TRUE))
  }

  strata = unique(row_map$bootstrap_stratum)
  unlist(lapply(strata, function(current_stratum) {
    current = row_map[bootstrap_stratum == current_stratum]
    clusters = unique(current$bootstrap_cluster)
    selected = sample(clusters, size = length(clusters), replace = TRUE)
    unlist(lapply(selected, function(current_cluster) {
      current[bootstrap_cluster == current_cluster, row_position]
    }), use.names = FALSE)
  }), use.names = FALSE)
}

.csdg_ale_bootstrap_task = function(task, row_positions, bootstrap_id) {
  columns = unique(c(task$feature_names, task$target_names))
  backend = as.data.table(task$data(rows = task$row_ids[row_positions], cols = columns))
  target = task$target_names[[1L]]
  task_id = paste0(task$id, "_ale_bootstrap_", sprintf("%05d", bootstrap_id))

  result = if (inherits(task, "TaskClassif")) {
    TaskClassif$new(task_id, backend = backend, target = target, positive = task$positive)
  } else if (inherits(task, "TaskRegr")) {
    TaskRegr$new(task_id, backend = backend, target = target)
  } else {
    .csdg_stop("ALE bootstrapping supports regression and classification tasks only.")
  }
  result
}

.csdg_ale_bootstrap_point_model = function(task, learner, seed) {
  model = learner$clone(deep = TRUE)
  train_task = tryCatch(model$state$train_task, error = function(e) NULL)
  if (is.null(train_task)) {
    set.seed(seed)
    model$train(task)
  } else {
    .csdg_diagnostic_assert_fitted_on_task(task, model)
  }
  model
}

.csdg_ale_bootstrap_curves = function(task, model, reference, features, grids, bins, trim, class_label, seed) {
  curves = lapply(seq_along(features), function(index) {
    feature = features[[index]]
    curve = .autoiml_ale_1d_iml(
      task = task,
      model = model,
      X = reference,
      feature = feature,
      bins = bins,
      trim = trim,
      grid = grids[[feature]],
      class_labels = class_label,
      seed = seed + index * 1009L
    )
    if (is.null(curve) || !nrow(curve)) {
      .csdg_stop("ALE could not be computed for feature `%s`.", feature)
    }
    curve
  })
  rbindlist(curves, use.names = TRUE, fill = TRUE)
}

#' Model-refit bootstrap intervals for one-dimensional ALE
#'
#' @description
#' Repeatedly resamples the analytic task, refits a fixed learner specification, and recomputes one-dimensional
#' accumulated local effects (ALE) on common feature grids.
#' The resulting percentile intervals include variation from resampling, refitting, and the bootstrap reference
#' sample.
#'
#' Row resampling is suitable only when respondents or observations are the independent sampling units.
#' Cluster resampling draws clusters with replacement, optionally independently within supplied strata, and keeps
#' every row from each selected cluster.
#'
#' The intervals are pointwise model-refit bootstrap intervals.
#' They are not simultaneous curve bands and do not include uncertainty from changing the learner specification,
#' hyperparameter search, outcome definition, or other analysis choices.
#'
#' @param task An [mlr3::TaskSupervised] object.
#' @param learner An [mlr3::Learner] object containing the fixed learner and preprocessing specification.
#'   It may be untrained or a full fit on `task`.
#' @param features Unique numeric or integer feature names.
#' @param sample_n Number of reference rows used for each ALE calculation.
#' @param ale_bins Number of empirical intervals per feature.
#' @param replicates Number of bootstrap refits.
#' @param confidence_level Pointwise percentile interval coverage.
#' @param resampling_unit Either `"row"` or `"cluster"`.
#' @param cluster Optional cluster vector aligned with `task$row_ids` or named by row id.
#' @param strata Optional stratum vector aligned with `task$row_ids` or named by row id.
#'   Cluster resampling is performed independently within strata.
#' @param min_interval_n Minimum reference-sample count required for a feature interval in a bootstrap replicate.
#' @param min_success_fraction Minimum fraction of requested replicates required at each feature interval.
#' @param seed Non-negative integer random seed.
#' @param class_label Class whose probability is analyzed for classification.
#'   It is inferred from the positive class for binary classification and must be `NULL` for regression.
#' @param keep_replicates Whether to return replicate-level ALE values.
#' @param parallel_workers Number of forked bootstrap workers.
#'   Values above one are supported on non-Windows platforms; the default is serial execution.
#' @param verbose Whether to report bootstrap progress.
#'
#' @return A list containing `ale_1d`, optional private `replicates_private`, `failures`, and `settings`.
#'   `ale_1d` contains the full-sample ALE estimate, pointwise percentile limits, separate point-curve and interval
#'   support flags, bootstrap support rates, and explicit uncertainty semantics.
#'
#' @importFrom mlr3 TaskClassif TaskRegr
#' @importFrom parallel mclapply
#' @export
csdg_ale_bootstrap = function(
    task,
    learner,
    features,
    sample_n = 500L,
    ale_bins = 12L,
    replicates = 200L,
    confidence_level = 0.95,
    resampling_unit = c("row", "cluster"),
    cluster = NULL,
    strata = NULL,
    min_interval_n = 10L,
    min_success_fraction = 0.80,
    seed = 20260203L,
    class_label = NULL,
    keep_replicates = FALSE,
    parallel_workers = 1L,
    verbose = interactive()) {
  .csdg_diagnostic_assert_task(task)
  assert_class(learner, "Learner", .var.name = "learner")
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)

  features = .csdg_diagnostic_numeric_features(task, features)
  sample_n = .csdg_diagnostic_assert_count(sample_n, "sample_n", upper = task$nrow)
  ale_bins = .csdg_diagnostic_assert_count(ale_bins, "ale_bins", lower = 2L, upper = 50L)
  replicates = .csdg_diagnostic_assert_count(replicates, "replicates", lower = 20L, upper = 10000L)
  assert_number(confidence_level, lower = 0.50, upper = 1, finite = TRUE, .var.name = "confidence_level")
  if (confidence_level >= 1) {
    .csdg_stop("`confidence_level` must be smaller than 1.")
  }
  resampling_unit = match.arg(resampling_unit)
  min_interval_n = .csdg_diagnostic_assert_count(min_interval_n, "min_interval_n", lower = 1L)
  assert_number(
    min_success_fraction,
    lower = 0.50,
    upper = 1,
    finite = TRUE,
    .var.name = "min_success_fraction"
  )
  seed = .csdg_diagnostic_assert_count(seed, "seed", lower = 0L, upper = .Machine$integer.max)
  class_label = .csdg_diagnostic_resolve_class(task, class_label)
  assert_flag(keep_replicates)
  parallel_workers = .csdg_diagnostic_assert_count(
    parallel_workers,
    "parallel_workers",
    lower = 1L,
    upper = replicates
  )
  if (parallel_workers > 1L && .Platform$OS.type == "windows") {
    .csdg_stop("`parallel_workers > 1` is not supported on Windows; use serial execution.")
  }
  assert_flag(verbose)
  row_map = .csdg_ale_bootstrap_row_map(task, resampling_unit, cluster, strata)

  trim = c(0.05, 0.95)
  point_reference = .csdg_diagnostic_sample_data(task, sample_n, seed)
  grids = setNames(lapply(features, function(feature) {
    grid = .csdg_ale_bootstrap_grid(point_reference[[feature]], ale_bins, trim)
    if (length(grid) < 3L) {
      .csdg_stop("Feature `%s` does not define at least two ALE intervals.", feature)
    }
    grid
  }), features)
  point_model = .csdg_ale_bootstrap_point_model(task, learner, seed + 1L)
  point = .csdg_ale_bootstrap_curves(
    task,
    point_model,
    point_reference,
    features,
    grids,
    ale_bins,
    trim,
    class_label,
    seed + 2L
  )

  learner_template = learner$clone(deep = TRUE)
  learner_template$reset()
  progress_every = max(1L, floor(replicates / 10L))
  compute_replicate = function(bootstrap_id) {
    replicate_seed = as.integer((as.double(seed) + bootstrap_id * 10007) %% .Machine$integer.max)
    result = tryCatch({
      set.seed(replicate_seed)
      row_positions = .csdg_ale_bootstrap_positions(task, resampling_unit, row_map)
      bootstrap_task = .csdg_ale_bootstrap_task(task, row_positions, bootstrap_id)
      model = learner_template$clone(deep = TRUE)
      set.seed(replicate_seed + 1L)
      model$train(bootstrap_task)
      reference = .csdg_diagnostic_sample_data(
        bootstrap_task,
        min(sample_n, bootstrap_task$nrow),
        replicate_seed + 2L
      )
      curve = .csdg_ale_bootstrap_curves(
        bootstrap_task,
        model,
        reference,
        features,
        grids,
        ale_bins,
        trim,
        class_label,
        replicate_seed + 3L
      )
      curve[, bootstrap_id := as.integer(bootstrap_id)]
      list(curve = curve, failure = NULL)
    }, error = function(error) {
      list(
        curve = NULL,
        failure = data.table(
          bootstrap_id = as.integer(bootstrap_id),
          message = conditionMessage(error)
        )
      )
    })
    if (parallel_workers == 1L && isTRUE(verbose) &&
        (bootstrap_id %% progress_every == 0L || bootstrap_id == replicates)) {
      .csdg_note("ALE bootstrap: %d of %d refits completed.", bootstrap_id, replicates)
    }
    result
  }
  if (parallel_workers > 1L && isTRUE(verbose)) {
    .csdg_note("ALE bootstrap: running %d refits on %d forked workers.", replicates, parallel_workers)
  }
  replicate_output = if (parallel_workers == 1L) {
    lapply(seq_len(replicates), compute_replicate)
  } else {
    mclapply(
      seq_len(replicates),
      compute_replicate,
      mc.cores = parallel_workers,
      mc.preschedule = TRUE,
      mc.set.seed = FALSE
    )
  }
  if (parallel_workers > 1L && isTRUE(verbose)) {
    .csdg_note("ALE bootstrap: all %d parallel refits returned.", replicates)
  }

  replicate_results = lapply(replicate_output, `[[`, "curve")
  failures = lapply(replicate_output, `[[`, "failure")
  replicate_table = rbindlist(replicate_results, use.names = TRUE, fill = TRUE)
  failure_table = rbindlist(failures, use.names = TRUE, fill = TRUE)
  successful_refits = uniqueN(replicate_table$bootstrap_id)
  required_refits = ceiling(replicates * min_success_fraction)
  if (successful_refits < required_refits) {
    .csdg_stop(
      "Only %d of %d ALE bootstrap refits succeeded; at least %d were required.",
      successful_refits,
      replicates,
      required_refits
    )
  }

  replicate_table[, replicate_supported := is.finite(ale) & n_interval >= min_interval_n]
  alpha = (1 - confidence_level) / 2
  intervals = replicate_table[, {
    eligible = ale[replicate_supported]
    n_eligible = length(eligible)
    enough = n_eligible >= required_refits
    .(
      ale_bootstrap_mean = if (enough) mean(eligible) else NA_real_,
      ale_bootstrap_median = if (enough) median(eligible) else NA_real_,
      ale_lower = if (enough) quantile(eligible, alpha, names = FALSE, type = 8) else NA_real_,
      ale_upper = if (enough) quantile(eligible, 1 - alpha, names = FALSE, type = 8) else NA_real_,
      n_bootstrap = n_eligible,
      bootstrap_support_rate = n_eligible / replicates,
      bootstrap_interval_n_median = as.numeric(median(n_interval[replicate_supported]))
    )
  }, by = .(class_label, feature, x_left, x_right, x)]

  result = merge(
    point,
    intervals,
    by = c("class_label", "feature", "x_left", "x_right", "x"),
    all.x = TRUE,
    sort = FALSE
  )
  result[, `:=`(
    confidence_level = confidence_level,
    interval_type = "pointwise percentile bootstrap",
    resampling_unit = if (resampling_unit == "row") "row" else "cluster within stratum",
    model_refit = TRUE,
    grid_scope = "fixed point-reference-sample empirical ALE boundaries",
    reference_scope = "bootstrap reference sample",
    bootstrap_replicates_requested = replicates,
    bootstrap_refits_successful = successful_refits,
    support_threshold = min_interval_n,
    supported = n_interval >= min_interval_n & is.finite(ale),
    interval_supported = n_interval >= min_interval_n & is.finite(ale) & n_bootstrap >= required_refits &
      is.finite(ale_lower) & is.finite(ale_upper)
  )]
  result[get("interval_supported") == FALSE, `:=`(ale_lower = NA_real_, ale_upper = NA_real_)]
  result[supported == FALSE, ale := NA_real_]
  result = .csdg_diagnostic_tag(
    result,
    paste(
      "Centered accumulated local prediction differences with pointwise percentile-bootstrap intervals from",
      "resampling, complete learner refitting, and ALE recomputation on common empirical grids."
    ),
    paste(
      "Intervals are pointwise rather than simultaneous and condition on the fixed learner specification,",
      "analytic sample definition, feature set, and grid. They do not include model-selection, hyperparameter-search,",
      "outcome-definition, or survey-design uncertainty beyond the declared resampling unit. An interval is omitted",
      "where fewer than the required fraction of bootstrap refits meet the interval-support threshold."
    ),
    "shareable_aggregate"
  )
  setorder(result, feature, x)

  list(
    ale_1d = result[],
    replicates_private = if (keep_replicates) replicate_table[] else NULL,
    failures = if (nrow(failure_table)) failure_table[] else data.table(
      bootstrap_id = integer(),
      message = character()
    ),
    settings = list(
      features = features,
      sample_n = sample_n,
      ale_bins = ale_bins,
      ale_trim = trim,
      replicates = replicates,
      successful_refits = successful_refits,
      confidence_level = confidence_level,
      interval_type = "pointwise percentile bootstrap",
      resampling_unit = resampling_unit,
      stratified = !is.null(strata),
      min_interval_n = min_interval_n,
      min_success_fraction = min_success_fraction,
      seed = seed,
      class_label = class_label,
      parallel_workers = parallel_workers
    )
  )
}
