
.calibration_binary = function(dt, positive, bins) {
  prob_col = .get_probability_column(dt, positive)
  p = .clip_probability(dt[[prob_col]])
  y = .truth_to_event(dt$truth, positive)
  ok = is.finite(p) & is.finite(y)
  p = p[ok]
  y = y[ok]
  if (!length(y)) .csdg_stop("No finite predictions available for calibration.")

  lp = stats::qlogis(p)
  intercept_fit = tryCatch(
    suppressWarnings(stats::glm(y ~ 1, family = stats::binomial(), offset = lp)),
    error = function(e) NULL
  )
  slope_fit = tryCatch(
    suppressWarnings(stats::glm(y ~ lp, family = stats::binomial())),
    error = function(e) NULL
  )
  intercept = if (is.null(intercept_fit)) NA_real_ else unname(stats::coef(intercept_fit)[[1L]])
  slope_coefs = if (is.null(slope_fit)) c(NA_real_, NA_real_) else stats::coef(slope_fit)
  brier = mean((y - p)^2)
  logloss = -mean(y * log(p) + (1 - y) * log(1 - p))

  probs = unique(stats::quantile(
    p, probs = seq(0, 1, length.out = bins + 1L),
    na.rm = TRUE, names = FALSE
  ))
  if (length(probs) < 2L) {
    bin = factor(rep(1L, length(p)))
  } else {
    probs[[1L]] = -Inf
    probs[[length(probs)]] = Inf
    bin = cut(p, breaks = probs, include.lowest = TRUE, ordered_result = TRUE)
  }
  curve = data.table::data.table(y = y, p = p, bin = bin)[, .(
    n = .N,
    predicted = mean(p),
    observed = mean(y)
  ), by = bin]
  data.table::setorder(curve, predicted)
  ece = sum(curve$n / sum(curve$n) * abs(curve$observed - curve$predicted))

  list(
    summary = data.table::data.table(
      n = length(y),
      prevalence = mean(y),
      brier = brier,
      logloss = logloss,
      calibration_in_the_large = intercept,
      calibration_intercept = unname(slope_coefs[[1L]]),
      calibration_slope = unname(slope_coefs[[2L]]),
      expected_calibration_error = ece
    ),
    curve = curve,
    observation_level = data.table::data.table(truth = y, probability = p)
  )
}

.calibration_regression = function(dt) {
  y = as.numeric(dt$truth)
  p = as.numeric(dt$response)
  ok = is.finite(y) & is.finite(p)
  y = y[ok]
  p = p[ok]
  if (length(y) < 3L) .csdg_stop("Too few finite predictions for calibration.")
  fit = stats::lm(y ~ p)
  coefs = stats::coef(fit)
  list(
    summary = data.table::data.table(
      n = length(y),
      outcome_mean = mean(y),
      prediction_mean = mean(p),
      rmse = sqrt(mean((y - p)^2)),
      mae = mean(abs(y - p)),
      calibration_in_the_large = mean(y - p),
      calibration_intercept = unname(coefs[[1L]]),
      calibration_slope = unname(coefs[[2L]])
    ),
    curve = data.table::data.table(truth = y, prediction = p),
    observation_level = data.table::data.table(truth = y, prediction = p)
  )
}

#' @rdname csdg_diagnostics
#' @export
csdg_calibration = function(
    predictions,
    task_type = NULL,
    positive = NULL,
    bins = 10L,
    collapse_repeats = TRUE) {
  checkmate::assert_int(bins, lower = 2)
  checkmate::assert_flag(collapse_repeats)
  if (!is.null(task_type)) checkmate::assert_choice(task_type, c("classif", "regr"))
  if (!is.null(positive)) checkmate::assert_string(positive, min.chars = 1L)
  checkmate::assert_true(
    inherits(predictions, "CSDGResample") || is.data.frame(predictions),
    .var.name = "predictions"
  )
  if (inherits(predictions, "CSDGResample")) {
    x = predictions
    task_type = x$task_type
    positive = positive %||% x$positive
    predictions = x$predictions
  }
  dt = .as_dt(predictions)
  if (isTRUE(collapse_repeats)) {
    dt = .aggregate_repeated_predictions(dt, positive)
  }
  task_type = task_type %||% if ("response" %in% names(dt) &&
                                   is.numeric(dt$response)) "regr" else "classif"
  if (identical(task_type, "classif")) {
    .calibration_binary(dt, positive, as.integer(bins))
  } else if (identical(task_type, "regr")) {
    .calibration_regression(dt)
  } else {
    .csdg_stop("Unsupported task type: %s.", task_type)
  }
}

#' @rdname csdg_diagnostics
#' @export
csdg_decision_curve = function(
    predictions,
    positive = NULL,
    thresholds = seq(0.05, 0.95, by = 0.05),
    collapse_repeats = TRUE) {
  checkmate::assert_flag(collapse_repeats)
  if (!is.null(positive)) checkmate::assert_string(positive, min.chars = 1L)
  checkmate::assert_true(
    inherits(predictions, "CSDGResample") || is.data.frame(predictions),
    .var.name = "predictions"
  )
  checkmate::assert_numeric(
    thresholds,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    min.len = 1L,
    unique = TRUE
  )
  if (inherits(predictions, "CSDGResample")) {
    positive = positive %||% predictions$positive
    predictions = predictions$predictions
  }
  dt = .as_dt(predictions)
  if (isTRUE(collapse_repeats)) {
    dt = .aggregate_repeated_predictions(dt, positive)
  }
  prob_col = .get_probability_column(dt, positive)
  p = .clip_probability(dt[[prob_col]])
  y = .truth_to_event(dt$truth, positive)
  ok = is.finite(p) & is.finite(y)
  p = p[ok]
  y = y[ok]
  thresholds = sort(unique(as.numeric(thresholds)))
  if (any(!is.finite(thresholds)) || any(thresholds <= 0 | thresholds >= 1)) {
    .csdg_stop("`thresholds` must be finite and strictly between zero and one.")
  }
  n = length(y)
  prevalence = mean(y)
  data.table::rbindlist(lapply(thresholds, function(t) {
    positive_prediction = p >= t
    tp = sum(positive_prediction & y == 1)
    fp = sum(positive_prediction & y == 0)
    weight = t / (1 - t)
    data.table::data.table(
      threshold = t,
      n = n,
      prevalence = prevalence,
      net_benefit_model = tp / n - fp / n * weight,
      net_benefit_treat_all = prevalence - (1 - prevalence) * weight,
      net_benefit_treat_none = 0,
      true_positive_rate = if (sum(y == 1)) tp / sum(y == 1) else NA_real_,
      false_positive_rate = if (sum(y == 0)) fp / sum(y == 0) else NA_real_
    )
  }))
}
