.calibration_bootstrap_vector = function(value, data, name) {
  if (is.null(value)) return(NULL)
  if (is.character(value) && length(value) == 1L && value %in% names(data)) {
    return(data[[value]])
  }
  if (!is.null(names(value)) && "row_id" %in% names(data)) {
    index = match(as.character(data$row_id), names(value))
    if (anyNA(index)) {
      .csdg_stop("Named `%s` does not cover every prediction row id.", name)
    }
    return(unname(value[index]))
  }
  if (length(value) != nrow(data)) {
    .csdg_stop("`%s` has length %d; expected %d.", name, length(value), nrow(data))
  }
  unname(value)
}

.calibration_bootstrap_index_sampler = function(n, cluster, strata) {
  if (is.null(cluster)) {
    return(function() sample.int(n, n, replace = TRUE))
  }
  cluster = as.character(cluster)
  strata = if (is.null(strata)) rep("all", n) else as.character(strata)
  if (anyNA(cluster) || anyNA(strata)) {
    .csdg_stop("`cluster` and `strata` must not contain missing values.")
  }
  cluster_stratum = unique(data.table(stratum = strata, cluster = cluster))
  nesting = cluster_stratum[, .(n_strata = uniqueN(stratum)), by = cluster]
  if (any(nesting$n_strata > 1L)) {
    .csdg_stop("Every cluster must be nested in exactly one stratum.")
  }
  cluster_stratum[, cluster_id := seq_len(.N)]
  row_membership = data.table(stratum = strata, cluster = cluster)
  row_groups = cluster_stratum[
    row_membership,
    on = c("stratum", "cluster"),
    cluster_id
  ]
  rows_by_cluster = split(seq_len(n), row_groups)
  stratum_order = unique(cluster_stratum$stratum)
  clusters_by_stratum = lapply(stratum_order, function(current_stratum) {
    cluster_stratum$cluster_id[cluster_stratum$stratum == current_stratum]
  })
  function() {
    sampled_cluster_ids = unlist(lapply(clusters_by_stratum, function(cluster_ids) {
      sample(cluster_ids, length(cluster_ids), replace = TRUE)
    }), use.names = FALSE)
    unlist(rows_by_cluster[as.character(sampled_cluster_ids)], use.names = FALSE)
  }
}

.calibration_bootstrap_indices = function(n, cluster, strata) {
  .calibration_bootstrap_index_sampler(n, cluster, strata)()
}

.calibration_flexible_curve = function(
    data,
    task_type,
    positive,
    grid,
    span,
    curve_method = "loess",
    spline_df = 4L) {
  if (identical(task_type, "classif")) {
    probability_column = .get_probability_column(data, positive)
    prediction = .clip_probability(data[[probability_column]])
    truth = .truth_to_event(data$truth, positive)
  } else {
    prediction = as.numeric(data$response)
    truth = as.numeric(data$truth)
  }
  keep = is.finite(prediction) & is.finite(truth)
  prediction = prediction[keep]
  truth = truth[keep]
  if (length(unique(prediction)) < 4L) {
    return(rep(NA_real_, length(grid)))
  }
  estimate = if (identical(curve_method, "loess")) {
    fit = tryCatch(
      stats::loess(
        truth ~ prediction,
        span = span,
        degree = 1L,
        surface = "direct",
        control = stats::loess.control(iterations = 2L)
      ),
      error = function(error) NULL
    )
    if (is.null(fit)) rep(NA_real_, length(grid)) else {
      suppressWarnings(as.numeric(stats::predict(fit, newdata = data.frame(prediction = grid))))
    }
  } else {
    basis = splines::ns(prediction, df = spline_df)
    grid_basis = predict(basis, newx = grid)
    design = cbind(intercept = 1, basis)
    grid_design = cbind(intercept = 1, grid_basis)
    fit = tryCatch(
      if (identical(task_type, "classif")) {
        stats::glm.fit(design, truth, family = stats::binomial())
      } else {
        stats::lm.fit(design, truth)
      },
      error = function(error) NULL
    )
    if (is.null(fit) || anyNA(fit$coefficients)) rep(NA_real_, length(grid)) else {
      linear_predictor = as.numeric(grid_design %*% fit$coefficients)
      if (identical(task_type, "classif")) stats::plogis(linear_predictor) else linear_predictor
    }
  }
  if (identical(task_type, "classif")) pmin(pmax(estimate, 0), 1) else estimate
}

.calibration_bootstrap_summary = function(
    data,
    task_type,
    positive,
    bins,
    grid,
    span,
    curve_method,
    spline_df) {
  result = csdg_calibration(
    data,
    task_type = task_type,
    positive = positive,
    bins = bins,
    collapse_repeats = FALSE
  )
  curve = .calibration_flexible_curve(
    data,
    task_type,
    positive,
    grid,
    span,
    curve_method = curve_method,
    spline_df = spline_df
  )
  absolute_error = abs(curve - grid)
  finite_error = absolute_error[is.finite(absolute_error)]
  summary = copy(result$summary)
  summary[, `:=`(
    integrated_absolute_calibration_error = if (length(finite_error)) mean(finite_error) else NA_real_,
    maximum_absolute_calibration_error = if (length(finite_error)) max(finite_error) else NA_real_
  )]
  list(summary = summary, curve = curve)
}

#' Bootstrap calibration with a flexible curve
#'
#' Estimates calibration-in-the-large, the free calibration intercept and slope, prediction-error summaries,
#' and a flexible calibration curve from out-of-fold predictions.
#' Percentile intervals can use a respondent bootstrap or a cluster bootstrap, optionally sampling clusters
#' independently within strata.
#' The bootstrap conditions on the supplied predictions unless the complete fitting pipeline is rerun by the caller.
#'
#' @param predictions A `CSDGResample` or prediction data frame accepted by [csdg_calibration()].
#' @param task_type Optional task type, either `"classif"` or `"regr"`.
#' @param positive Positive class label for binary classification.
#' @param bins Number of equal-frequency bins retained as a descriptive diagnostic.
#' @param cluster Optional cluster vector, named vector indexed by `row_id`, or column name in `predictions`.
#' @param strata Optional stratum vector, named vector indexed by `row_id`, or column name in `predictions`.
#'   When supplied with `cluster`, clusters are sampled independently within strata.
#' @param repetitions Number of bootstrap replicates.
#' @param confidence Confidence level for pointwise percentile intervals.
#' @param grid_points Number of prediction values used for the flexible calibration curve.
#' @param span Loess span for the flexible curve.
#' @param curve_method Flexible-curve estimator, either direct local-linear `"loess"` or a natural cubic
#'   `"regression_spline"`.
#' @param spline_df Degrees of freedom for `curve_method = "regression_spline"`.
#' @param seed Random seed.
#' @param workers Number of local worker processes used to evaluate already drawn bootstrap samples.
#'   Values greater than one use forked processes where supported and otherwise fall back to serial evaluation.
#' @param collapse_repeats Whether repeated predictions for the same row are averaged before calibration.
#'
#' @return A list with point estimates and percentile intervals in `summary`, a flexible calibration curve with
#'   pointwise intervals in `curve`, descriptive bins in `bins`, bootstrap replicates, estimand definitions,
#'   resampling metadata, and limitations.
#' @export
csdg_calibration_bootstrap = function(
    predictions,
    task_type = NULL,
    positive = NULL,
    bins = 10L,
    cluster = NULL,
    strata = NULL,
    repetitions = 1000L,
    confidence = 0.95,
    grid_points = 101L,
    span = 0.75,
    curve_method = c("loess", "regression_spline"),
    spline_df = 4L,
    seed = 20260201L,
    workers = 1L,
    collapse_repeats = TRUE) {
  assert_true(inherits(predictions, "CSDGResample") || is.data.frame(predictions), .var.name = "predictions")
  assert_int(bins, lower = 2L)
  assert_int(repetitions, lower = 20L)
  assert_number(confidence, lower = 0.50, upper = 0.999, finite = TRUE)
  assert_int(grid_points, lower = 20L)
  assert_number(span, lower = 0.10, upper = 1, finite = TRUE)
  curve_method = match.arg(curve_method)
  assert_int(spline_df, lower = 3L, upper = 10L)
  assert_int(seed, lower = 0L)
  assert_int(workers, lower = 1L)
  assert_flag(collapse_repeats)
  if (!is.null(task_type)) assert_choice(task_type, c("classif", "regr"))
  if (!is.null(positive)) assert_string(positive, min.chars = 1L)

  if (inherits(predictions, "CSDGResample")) {
    task_type = task_type %||% predictions$task_type
    positive = positive %||% predictions$positive
    data = .as_dt(predictions$predictions)
  } else {
    data = .as_dt(predictions)
  }
  cluster = .calibration_bootstrap_vector(cluster, data, "cluster")
  strata = .calibration_bootstrap_vector(strata, data, "strata")
  if (!is.null(strata) && is.null(cluster)) {
    .csdg_stop("`strata` requires `cluster`; use a respondent bootstrap when `cluster` is NULL.")
  }
  if (!is.null(cluster)) data[, calibration_cluster__ := cluster]
  if (!is.null(strata)) data[, calibration_stratum__ := strata]
  if (isTRUE(collapse_repeats)) data = .aggregate_repeated_predictions(data, positive)
  task_type = task_type %||% if ("response" %in% names(data) && is.numeric(data$response)) "regr" else "classif"

  prediction = if (identical(task_type, "classif")) {
    .clip_probability(data[[.get_probability_column(data, positive)]])
  } else {
    as.numeric(data$response)
  }
  finite_prediction = prediction[is.finite(prediction)]
  if (length(finite_prediction) < 20L) {
    .csdg_stop("At least 20 finite predictions are required.")
  }
  probabilities = if (identical(task_type, "classif")) c(0.01, 0.99) else c(0.005, 0.995)
  limits = as.numeric(stats::quantile(finite_prediction, probabilities, names = FALSE, type = 8))
  grid = seq(limits[[1L]], limits[[2L]], length.out = grid_points)
  point = .calibration_bootstrap_summary(
    data,
    task_type,
    positive,
    bins,
    grid,
    span,
    curve_method,
    spline_df
  )

  had_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) caller_seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      remove(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  index_sampler = .calibration_bootstrap_index_sampler(
    nrow(data),
    data$calibration_cluster__ %||% NULL,
    data$calibration_stratum__ %||% NULL
  )
  bootstrap_indices = lapply(seq_len(repetitions), function(replicate) index_sampler())
  evaluate_bootstrap = function(replicate) {
    index = bootstrap_indices[[replicate]]
    estimate = .calibration_bootstrap_summary(
      data[index],
      task_type,
      positive,
      bins,
      grid,
      span,
      curve_method,
      spline_df
    )
    estimate$summary[, bootstrap_replicate := replicate]
    list(summary = estimate$summary, curve = estimate$curve)
  }
  use_forks = workers > 1L && identical(.Platform$OS.type, "unix")
  bootstrap = if (use_forks) {
    parallel::mclapply(
      seq_len(repetitions),
      evaluate_bootstrap,
      mc.cores = workers,
      mc.preschedule = TRUE,
      mc.set.seed = FALSE
    )
  } else {
    lapply(seq_len(repetitions), evaluate_bootstrap)
  }
  bootstrap_summary = rbindlist(lapply(bootstrap, `[[`, "summary"), fill = TRUE)
  bootstrap_curve = do.call(rbind, lapply(bootstrap, `[[`, "curve"))
  alpha = (1 - confidence) / 2
  numeric_metrics = setdiff(
    names(point$summary)[vapply(point$summary, is.numeric, logical(1L))],
    "n"
  )
  summary = rbindlist(lapply(numeric_metrics, function(metric) {
    values = bootstrap_summary[[metric]]
    data.table(
      estimand = metric,
      estimate = point$summary[[metric]][[1L]],
      lower = as.numeric(stats::quantile(values, alpha, na.rm = TRUE, names = FALSE, type = 8)),
      upper = as.numeric(stats::quantile(values, 1 - alpha, na.rm = TRUE, names = FALSE, type = 8))
    )
  }))
  curve = data.table(
    prediction = grid,
    estimate = point$curve,
    lower = apply(bootstrap_curve, 2L, stats::quantile, probs = alpha, na.rm = TRUE, names = FALSE, type = 8),
    upper = apply(bootstrap_curve, 2L, stats::quantile, probs = 1 - alpha, na.rm = TRUE, names = FALSE, type = 8),
    curve_method = curve_method,
    spline_df = if (identical(curve_method, "regression_spline")) spline_df else NA_integer_
  )
  list(
    summary = summary,
    curve = curve,
    bins = csdg_calibration(data, task_type, positive, bins, collapse_repeats = FALSE)$curve,
    bootstrap_summary = bootstrap_summary,
    estimands = data.table(
      estimand = c(
        "calibration_in_the_large",
        "calibration_intercept",
        "calibration_slope",
        "integrated_absolute_calibration_error",
        "maximum_absolute_calibration_error"
      ),
      definition = c(
        if (identical(task_type, "classif")) {
          "Intercept with logit prediction as offset"
        } else {
          "Mean truth minus prediction"
        },
        "Free intercept in the calibration model",
        "Free slope in the calibration model",
        "Mean absolute flexible-curve deviation from the identity line over the displayed prediction grid",
        "Maximum absolute flexible-curve deviation from the identity line over the displayed prediction grid"
      )
    ),
    resampling = list(
      unit = if (is.null(cluster)) "respondent" else "cluster",
      stratified = !is.null(strata),
      repetitions = repetitions,
      confidence = confidence,
      seed = seed,
      curve_method = curve_method,
      spline_df = if (identical(curve_method, "regression_spline")) spline_df else NA_integer_,
      workers_requested = workers,
      workers_used = if (use_forks) workers else 1L
    ),
    limitations = c(
      "Intervals are pointwise percentile intervals and are not simultaneous bands.",
      "The bootstrap conditions on supplied predictions and omits model-training variation unless models are refit.",
      "The flexible curve depends on its declared estimator settings and displayed prediction range."
    )
  )
}
