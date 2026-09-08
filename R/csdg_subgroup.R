
.binary_group_metrics = function(dt, positive, threshold) {
  prob_col = .get_probability_column(dt, positive)
  p = .clip_probability(dt[[prob_col]])
  y = .truth_to_event(dt$truth, positive)
  ok = is.finite(p) & is.finite(y)
  p = p[ok]
  y = y[ok]
  if (!length(y)) return(NULL)
  pred = as.integer(p >= threshold)
  lp = stats::qlogis(p)
  cal = tryCatch(suppressWarnings(stats::glm(y ~ lp, family = stats::binomial())),
                  error = function(e) NULL)
  cal_in_the_large = tryCatch(
    suppressWarnings(stats::glm(y ~ 1, family = stats::binomial(), offset = lp)),
    error = function(e) NULL
  )
  coefs = if (is.null(cal)) c(NA_real_, NA_real_) else stats::coef(cal)
  calibration_in_the_large = if (is.null(cal_in_the_large)) {
    NA_real_
  } else {
    unname(stats::coef(cal_in_the_large)[[1L]])
  }
  data.table::data.table(
    n = length(y),
    prevalence = mean(y),
    auc = .auc_rank(y, p),
    logloss = -mean(y * log(p) + (1 - y) * log(1 - p)),
    brier = mean((y - p)^2),
    accuracy = mean(pred == y),
    sensitivity = if (sum(y == 1)) mean(pred[y == 1] == 1) else NA_real_,
    specificity = if (sum(y == 0)) mean(pred[y == 0] == 0) else NA_real_,
    decision_threshold = threshold,
    threshold_degenerate = data.table::uniqueN(pred) < 2L,
    calibration_in_the_large = calibration_in_the_large,
    calibration_intercept = unname(coefs[[1L]]),
    calibration_slope = unname(coefs[[2L]])
  )
}

.regression_group_metrics = function(dt) {
  y = as.numeric(dt$truth)
  p = as.numeric(dt$response)
  ok = is.finite(y) & is.finite(p)
  y = y[ok]
  p = p[ok]
  if (!length(y)) return(NULL)
  denom = sum((y - mean(y))^2)
  fit = if (length(y) >= 3L) stats::lm(y ~ p) else NULL
  coefs = if (is.null(fit)) c(NA_real_, NA_real_) else stats::coef(fit)
  data.table::data.table(
    n = length(y),
    outcome_mean = mean(y),
    prediction_mean = mean(p),
    rmse = sqrt(mean((y - p)^2)),
    mae = mean(abs(y - p)),
    r_squared = if (denom > 0) 1 - sum((y - p)^2) / denom else NA_real_,
    calibration_in_the_large = mean(y - p),
    calibration_intercept = unname(coefs[[1L]]),
    calibration_slope = unname(coefs[[2L]])
  )
}

.subgroup_metric_table = function(joined, task_type, positive, threshold) {
  joined[, {
    current = .SD
    out = if (identical(task_type, "classif")) {
      .binary_group_metrics(current, positive, threshold)
    } else {
      .regression_group_metrics(current)
    }
    out
  }, by = .(audit_variable, subgroup)]
}

.subgroup_metric_names = function(task_type) {
  if (identical(task_type, "classif")) {
    c("prevalence", "auc", "logloss", "brier", "accuracy", "sensitivity", "specificity")
  } else {
    c("outcome_mean", "prediction_mean", "rmse", "mae", "r_squared")
  }
}

.subgroup_cluster_map = function(cluster, predictions) {
  if (is.null(cluster)) {
    return(data.table::data.table(
      row_id = predictions$row_id,
      bootstrap_cluster = paste0("row_", predictions$row_id)
    ))
  }
  if (is.data.frame(cluster) || data.table::is.data.table(cluster)) {
    cluster_map = .as_dt(cluster)
    if (!"row_id" %in% names(cluster_map) || ncol(cluster_map) != 2L) {
      .csdg_stop("Cluster data must contain `row_id` and exactly one cluster column.")
    }
    cluster_column = setdiff(names(cluster_map), "row_id")
    data.table::setnames(cluster_map, cluster_column, "bootstrap_cluster")
  } else {
    if (length(cluster) != nrow(predictions)) {
      .csdg_stop("An atomic `cluster` must be row-aligned with the collapsed prediction table.")
    }
    cluster_map = data.table::data.table(
      row_id = predictions$row_id,
      bootstrap_cluster = cluster
    )
  }
  if (anyDuplicated(cluster_map$row_id) || anyNA(cluster_map$row_id) || anyNA(cluster_map$bootstrap_cluster)) {
    .csdg_stop("Cluster mappings must be complete and contain one non-missing cluster per row id.")
  }
  cluster_map[, bootstrap_cluster := as.character(bootstrap_cluster)]
  cluster_map
}

.subgroup_strata_map = function(strata, predictions) {
  if (is.null(strata)) {
    return(data.table::data.table(
      row_id = predictions$row_id,
      bootstrap_stratum = "all rows"
    ))
  }
  if (is.data.frame(strata) || data.table::is.data.table(strata)) {
    strata_map = .as_dt(strata)
    if (!"row_id" %in% names(strata_map) || ncol(strata_map) != 2L) {
      .csdg_stop("Bootstrap-strata data must contain `row_id` and exactly one strata column.")
    }
    strata_column = setdiff(names(strata_map), "row_id")
    data.table::setnames(strata_map, strata_column, "bootstrap_stratum")
  } else {
    if (length(strata) != nrow(predictions)) {
      .csdg_stop("Atomic `bootstrap_strata` must be row-aligned with the collapsed prediction table.")
    }
    strata_map = data.table::data.table(
      row_id = predictions$row_id,
      bootstrap_stratum = strata
    )
  }
  if (anyDuplicated(strata_map$row_id) || anyNA(strata_map$row_id) || anyNA(strata_map$bootstrap_stratum)) {
    .csdg_stop("Bootstrap-strata mappings must be complete and contain one non-missing stratum per row id.")
  }
  strata_map[, bootstrap_stratum := as.character(bootstrap_stratum)]
  strata_map
}

.subgroup_bootstrap_sample = function(current) {
  cluster_strata = unique(current[, .(bootstrap_stratum, bootstrap_cluster)])
  cluster_membership = cluster_strata[, .(
    n_strata = data.table::uniqueN(bootstrap_stratum)
  ), by = bootstrap_cluster]
  if (any(cluster_membership$n_strata != 1L)) {
    .csdg_stop("Each bootstrap cluster must belong to exactly one bootstrap stratum.")
  }
  sampled = cluster_strata[, .(
    bootstrap_cluster = sample(bootstrap_cluster, .N, replace = TRUE),
    bootstrap_draw = seq_len(.N)
  ), by = bootstrap_stratum]
  current[sampled, on = c("bootstrap_stratum", "bootstrap_cluster"), allow.cartesian = TRUE, nomatch = 0L]
}

.empty_subgroup_uncertainty = function() {
  list(
    uncertainty = data.table::data.table(
      audit_variable = character(), subgroup = character(), metric = character(),
      estimate = numeric(), ci_low = numeric(), ci_high = numeric(),
      n_bootstrap = integer(), confidence_level = numeric(), bootstrap_unit = character(),
      interpretation = character()
    ),
    contrasts = data.table::data.table(
      audit_variable = character(), subgroup_1 = character(), subgroup_2 = character(), metric = character(),
      estimate_difference = numeric(), ci_low = numeric(), ci_high = numeric(),
      n_bootstrap = integer(), confidence_level = numeric(), bootstrap_unit = character(),
      interpretation = character()
    )
  )
}

.subgroup_bootstrap = function(
    joined,
    metrics,
    task_type,
    positive,
    threshold,
    repetitions,
    confidence_level,
    seed,
    clustered,
    stratified,
    cluster_label,
    strata_label) {
  if (repetitions == 0L) return(.empty_subgroup_uncertainty())
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
  set.seed(seed)

  metric_names = .subgroup_metric_names(task_type)
  metric_names = intersect(metric_names, names(metrics))
  bootstrap = vector("list", repetitions * data.table::uniqueN(joined$audit_variable))
  index = 0L
  for (audit_name in unique(joined$audit_variable)) {
    current = joined[audit_variable == audit_name]
    for (iteration in seq_len(repetitions)) {
      sample_rows = .subgroup_bootstrap_sample(current)
      current_metrics = .subgroup_metric_table(sample_rows, task_type, positive, threshold)
      current_long = data.table::melt(
        current_metrics,
        id.vars = c("audit_variable", "subgroup"),
        measure.vars = metric_names,
        variable.name = "metric",
        value.name = "estimate"
      )
      current_long[, iteration := iteration]
      index = index + 1L
      bootstrap[[index]] = current_long
    }
  }
  bootstrap = data.table::rbindlist(bootstrap[seq_len(index)], use.names = TRUE)
  bootstrap[, subgroup := as.character(subgroup)]
  alpha = (1 - confidence_level) / 2
  interval = bootstrap[, {
    finite = estimate[is.finite(estimate)]
    list(
      ci_low = if (length(finite)) stats::quantile(finite, alpha, names = FALSE) else NA_real_,
      ci_high = if (length(finite)) stats::quantile(finite, 1 - alpha, names = FALSE) else NA_real_,
      n_bootstrap = length(finite)
    )
  }, by = .(audit_variable, subgroup, metric)]
  point = data.table::melt(
    metrics,
    id.vars = c("audit_variable", "subgroup"),
    measure.vars = metric_names,
    variable.name = "metric",
    value.name = "estimate"
  )
  point[, subgroup := as.character(subgroup)]
  uncertainty = merge(point, interval, by = c("audit_variable", "subgroup", "metric"), all.x = TRUE)

  point_pairs = merge(
    point,
    point,
    by = c("audit_variable", "metric"),
    suffixes = c("_1", "_2"),
    allow.cartesian = TRUE
  )[subgroup_1 < subgroup_2]
  point_pairs[, estimate_difference := estimate_1 - estimate_2]
  bootstrap_pairs = merge(
    bootstrap,
    bootstrap,
    by = c("audit_variable", "metric", "iteration"),
    suffixes = c("_1", "_2"),
    allow.cartesian = TRUE
  )[subgroup_1 < subgroup_2]
  bootstrap_pairs[, difference := estimate_1 - estimate_2]
  contrast_interval = bootstrap_pairs[, {
    finite = difference[is.finite(difference)]
    list(
      ci_low = if (length(finite)) stats::quantile(finite, alpha, names = FALSE) else NA_real_,
      ci_high = if (length(finite)) stats::quantile(finite, 1 - alpha, names = FALSE) else NA_real_,
      n_bootstrap = length(finite)
    )
  }, by = .(audit_variable, subgroup_1, subgroup_2, metric)]
  contrasts = merge(
    point_pairs[, .(audit_variable, subgroup_1, subgroup_2, metric, estimate_difference)],
    contrast_interval,
    by = c("audit_variable", "subgroup_1", "subgroup_2", "metric"),
    all.x = TRUE
  )
  bootstrap_unit = if (clustered) cluster_label else "analytic row"
  if (stratified) {
    bootstrap_unit = paste0(bootstrap_unit, " within ", strata_label)
  }
  uncertainty[, `:=`(
    confidence_level = confidence_level,
    bootstrap_unit = bootstrap_unit,
    interpretation = paste(
      "Percentile bootstrap interval conditional on stored out-of-fold predictions;",
      "model-training variation is not included"
    )
  )]
  contrasts[, `:=`(
    confidence_level = confidence_level,
    bootstrap_unit = bootstrap_unit,
    interpretation = paste(
      "Pairwise subgroup contrast with a percentile bootstrap interval conditional on stored",
      "out-of-fold predictions; model-training variation is not included"
    )
  )]
  data.table::setorder(uncertainty, audit_variable, subgroup, metric)
  data.table::setorder(contrasts, audit_variable, metric, subgroup_1, subgroup_2)
  list(uncertainty = uncertainty, contrasts = contrasts)
}

#' @rdname csdg_diagnostics
#' @export
csdg_subgroup_metrics = function(
    predictions,
    subgroup,
    row_id = NULL,
    task_type = NULL,
    positive = NULL,
    threshold = 0.5,
    min_n = 1L,
    collapse_repeats = TRUE,
    cluster = NULL,
    bootstrap_strata = NULL,
    bootstrap_repetitions = 0L,
    confidence_level = 0.95,
    seed = 20260201L) {
  checkmate::assert_number(threshold, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_int(min_n, lower = 1)
  checkmate::assert_flag(collapse_repeats)
  checkmate::assert_int(bootstrap_repetitions, lower = 0)
  checkmate::assert_number(confidence_level, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_int(seed, lower = 0)
  if (confidence_level <= 0 || confidence_level >= 1) {
    .csdg_stop("`confidence_level` must be strictly between zero and one.")
  }
  if (!is.null(task_type)) checkmate::assert_choice(task_type, c("classif", "regr"))
  if (!is.null(positive)) checkmate::assert_string(positive, min.chars = 1L)
  checkmate::assert_true(
    inherits(predictions, "CSDGResample") || is.data.frame(predictions),
    .var.name = "predictions"
  )
  checkmate::assert_true(is.atomic(subgroup) || is.data.frame(subgroup), .var.name = "subgroup")
  if (!is.null(row_id)) checkmate::assert_atomic(row_id, any.missing = FALSE)
  if (inherits(predictions, "CSDGResample")) {
    x = predictions
    task_type = task_type %||% x$task_type
    positive = positive %||% x$positive
    predictions = x$predictions
  }
  dt = .as_dt(predictions)
  if (isTRUE(collapse_repeats)) {
    dt = .aggregate_repeated_predictions(dt, positive)
  }
  if (!"row_id" %in% names(dt) && "row_ids" %in% names(dt)) {
    data.table::setnames(dt, "row_ids", "row_id")
  }
  if (!"row_id" %in% names(dt)) {
    .csdg_stop("Predictions must contain `row_id`.")
  }
  cluster_columns = if (is.data.frame(cluster) || data.table::is.data.table(cluster)) {
    setdiff(names(cluster), "row_id")
  } else {
    character()
  }
  strata_columns = if (is.data.frame(bootstrap_strata) || data.table::is.data.table(bootstrap_strata)) {
    setdiff(names(bootstrap_strata), "row_id")
  } else {
    character()
  }
  cluster_label = if (length(cluster_columns) == 1L) {
    cluster_columns[[1L]]
  } else if (is.null(cluster)) {
    "analytic row"
  } else {
    "caller-supplied cluster"
  }
  strata_label = if (length(strata_columns) == 1L) {
    strata_columns[[1L]]
  } else if (is.null(bootstrap_strata)) {
    "all rows"
  } else {
    "caller-supplied stratum"
  }
  cluster_map = .subgroup_cluster_map(cluster, dt)
  strata_map = .subgroup_strata_map(bootstrap_strata, dt)

  if (is.data.frame(subgroup) || data.table::is.data.table(subgroup)) {
    sg = .as_dt(subgroup)
    if (!"row_id" %in% names(sg)) {
      .csdg_stop("Subgroup data must contain `row_id`.")
    }
    group_cols = setdiff(names(sg), "row_id")
    if (!length(group_cols)) {
      .csdg_stop("Subgroup data must contain at least one audit variable.")
    }
    long_sg = data.table::melt(
      sg, id.vars = "row_id", measure.vars = group_cols,
      variable.name = "audit_variable", value.name = "subgroup"
    )
  } else {
    if (is.null(row_id)) {
      if (length(subgroup) != nrow(dt)) {
        .csdg_stop("Provide `row_id` when subgroup values are not row-aligned.")
      }
      row_id = dt$row_id
    }
    long_sg = data.table::data.table(
      row_id = row_id,
      audit_variable = "subgroup",
      subgroup = subgroup
    )
  }
  joined = merge(dt, long_sg, by = "row_id", allow.cartesian = TRUE)
  joined = joined[!is.na(subgroup)]
  joined[, subgroup := as.character(subgroup)]
  joined = merge(joined, cluster_map, by = "row_id", all.x = TRUE)
  joined = merge(joined, strata_map, by = "row_id", all.x = TRUE)
  if (anyNA(joined$bootstrap_cluster)) {
    .csdg_stop("Cluster mappings are incomplete for the subgroup prediction rows.")
  }
  if (anyNA(joined$bootstrap_stratum)) {
    .csdg_stop("Bootstrap-strata mappings are incomplete for the subgroup prediction rows.")
  }
  task_type = task_type %||% if ("response" %in% names(joined) &&
                                   is.numeric(joined$response)) "regr" else "classif"

  metrics = .subgroup_metric_table(joined, task_type, positive, threshold)
  metrics[, below_minimum_n := n < min_n]
  metrics[, evidence_scope :=
            "descriptive audit of selected-model out-of-fold predictions; no subgroup refitting"]
  data.table::setorder(metrics, audit_variable, subgroup)
  uncertainty = .subgroup_bootstrap(
    joined = joined,
    metrics = metrics,
    task_type = task_type,
    positive = positive,
    threshold = threshold,
    repetitions = bootstrap_repetitions,
    confidence_level = confidence_level,
    seed = seed,
    clustered = !is.null(cluster),
    stratified = !is.null(bootstrap_strata),
    cluster_label = cluster_label,
    strata_label = strata_label
  )
  list(
    metrics = metrics,
    counts = joined[, .N, by = .(audit_variable, subgroup)],
    uncertainty = uncertainty$uncertainty,
    contrasts = uncertainty$contrasts,
    limitations = c(
      "Subgroup estimates are descriptive unless an explicit inferential design is supplied.",
      "Small groups can yield unstable discrimination and calibration estimates.",
      "The audit evaluates one selected model and does not fit group-specific models.",
      if (bootstrap_repetitions > 0L) {
        paste(Filter(nzchar, c(
          "Bootstrap intervals are conditional on the stored out-of-fold predictions and do not include",
          "variation from refitting the prediction pipeline.",
          if (!is.null(bootstrap_strata)) {
            "Clusters are resampled independently within each supplied stratum."
          } else {
            ""
          }
        )), collapse = " ")
      }
    )
  )
}
