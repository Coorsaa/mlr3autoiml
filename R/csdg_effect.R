#' Accumulated local effects of fitted learners in held-out folds
#'
#' @description
#' Computes, for each learner and fold of a [csdg_fit()] result, the accumulated local effect (ALE) of numeric
#' predictors in the held-out fold and its change between two percentiles of the predictor (default 10th and 90th).
#' Look at the result with `print()` and `plot()` before stating a direction with [claim_direction()].
#'
#' The ALE is computed on `intervals` quantile intervals of the predictor in the held-out fold: within each interval,
#' the predictions at its upper and lower boundary are differenced and averaged over the rows in the interval, and
#' the averages are accumulated.
#' For classification, the predictions are probabilities of the positive class.
#' The change is the difference of the accumulated effect between the two percentiles; the summary averages it over
#' folds and reports the corrected resampled 95% interval ([csdg_learner_pfi_interval()]) descriptively.
#'
#' @param fits A [csdg_fit()] result.
#' @param features Numeric predictors; defaults to all numeric predictors.
#' @param probs Two increasing probabilities strictly between 0 and 1: the percentiles between which the change is
#'   measured.
#' @param intervals Number of quantile intervals of the ALE.
#'
#' @return A `CSDGEffect` list with `fits`, `features`, `probs`, `intervals`, `changes` (one row per learner,
#'   predictor, and fold), `curves` (the ALE per fold), and `summary` (mean change, corrected interval, and the
#'   number of folds with a positive change per learner and predictor).
#'   `as.data.table()` returns one of the tables.
#' @seealso [claim_direction()], [csdg_check()]
#' @examples
#' set.seed(1)
#' n = 200
#' x = data.frame(a = rnorm(n), b = rnorm(n))
#' x$y = 2 * x$a + rnorm(n)
#' task = mlr3::as_task_regr(x, target = "y", id = "toy")
#' fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3, seed = 1)
#' eff = csdg_effect(fits)
#' eff
#' @export
csdg_effect = function(fits, features = NULL, probs = c(0.1, 0.9), intervals = 20L) {
  assert_class(fits, "CSDGFits", .var.name = "fits")
  .csdg_check_fits(fits)
  task = fits$task
  numeric_features = fits$features[vapply(task$data(cols = fits$features), is.numeric, logical(1L))]
  features = features %||% numeric_features
  assert_character(features, any.missing = FALSE, min.len = 1L, unique = TRUE, .var.name = "features")
  bad = setdiff(features, numeric_features)
  if (length(bad)) .csdg_stop("ALE needs numeric predictors of the task; %s %s not.", .csdg_and(bad),
    if (length(bad) == 1L) "is" else "are")
  .csdg_check_probs(probs)
  assert_int(intervals, lower = 2L, upper = 100L, .var.name = "intervals")
  parts = lapply(features, function(feature) .csdg_direction_effects(fits, feature, probs, intervals))
  changes = rbindlist(lapply(parts, `[[`, "changes"))
  curves = rbindlist(lapply(parts, `[[`, "curves"))
  summary = changes[, {
    ci = csdg_learner_pfi_interval(change[order(iteration)], n_train = fits$n_train, n_test = fits$n_test)
    list(mean_change = mean(change), lower = ci$lower, upper = ci$upper, folds_positive = sum(change > 0), K = .N)
  }, by = .(learner, feature)]
  structure(
    list(fits = fits, features = features, probs = probs, intervals = as.integer(intervals), changes = changes,
      curves = curves, summary = summary),
    class = c("CSDGEffect", "list")
  )
}

.csdg_check_probs = function(probs) {
  assert_numeric(probs, len = 2L, any.missing = FALSE, .var.name = "probs")
  if (probs[[1L]] <= 0 || probs[[2L]] >= 1 || probs[[1L]] >= probs[[2L]]) {
    .csdg_stop("`probs` must be two increasing numbers strictly between 0 and 1.")
  }
  invisible(TRUE)
}

# ALE of `feature` on the rows of `newdata`: the accumulated curve and its change between two quantiles.
.csdg_ale_change = function(model, task, newdata, feature, probs, intervals, positive) {
  newdata = as.data.table(newdata)
  x = newdata[[feature]]
  keep = !is.na(x)
  dat = newdata[keep]
  x = x[keep]
  quantiles = stats::quantile(x, probs, type = 7L, names = FALSE)
  breaks = unique(stats::quantile(x, seq(0, 1, length.out = intervals + 1L), type = 1L, names = FALSE))
  if (length(breaks) < 2L) {
    return(list(change = 0, q_low = quantiles[[1L]], q_high = quantiles[[2L]], n = length(x),
      curve = data.table(x = breaks, ale = 0)))
  }
  interval = pmax(1L, findInterval(x, breaks, rightmost.closed = TRUE, left.open = TRUE))
  ftype = .autoiml_feature_type(task, feature)
  low = copy(dat)
  high = copy(dat)
  data.table::set(low, j = feature, value = .autoiml_cast_like_feature(breaks[interval], ftype))
  data.table::set(high, j = feature, value = .autoiml_cast_like_feature(breaks[interval + 1L], ftype))
  prediction = .prediction_vector(.predict_newdata(model, rbindlist(list(low, high)), task = task),
    task$task_type, positive)
  difference = prediction[nrow(dat) + seq_len(nrow(dat))] - prediction[seq_len(nrow(dat))]
  effects = vapply(seq_len(length(breaks) - 1L), function(i) {
    rows = interval == i
    if (any(rows)) mean(difference[rows]) else 0
  }, numeric(1L))
  accumulated = c(0, cumsum(effects))
  at = stats::approx(breaks, accumulated, xout = quantiles, rule = 2L, ties = "ordered")$y
  list(change = at[[2L]] - at[[1L]], q_low = quantiles[[1L]], q_high = quantiles[[2L]], n = length(x),
    curve = data.table(x = breaks, ale = accumulated - mean(accumulated)))
}

.csdg_direction_effects = function(fits, feature, probs, intervals) {
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  task = fits$task
  out = lapply(names(fits$resamples), function(learner) {
    resample = fits$resamples[[learner]]
    lapply(seq_along(resample$models), function(i) {
      newdata = task$data(rows = resample$test_sets[[i]], cols = task$feature_names)
      ale = .csdg_ale_change(resample$models[[i]], task, newdata, feature, probs, intervals, fits$positive)
      curve = ale$curve
      curve[, `:=`(learner = learner, feature = feature, iteration = i)]
      list(
        changes = data.table(learner = learner, feature = feature, iteration = i, change = ale$change,
          q_low = ale$q_low, q_high = ale$q_high, n_test = ale$n),
        curves = curve
      )
    })
  })
  out = unlist(out, recursive = FALSE)
  list(changes = rbindlist(lapply(out, `[[`, "changes")),
    curves = rbindlist(lapply(out, `[[`, "curves"))[, .(learner, feature, iteration, x, ale)])
}

#' @rdname csdg_effect
#' @param x A `CSDGEffect` object.
#' @param learner For `print()` and `plot()`: names or labels of the learners shown; defaults to all.
#' @param ... Ignored.
#' @export
format.CSDGEffect = function(x, learner = NULL, ...) {
  fits = x$fits
  learners = .csdg_resolve_learner_names(fits, learner %||% names(fits$resamples))
  scale = if (identical(fits$task_type, "classif")) "predicted probability" else "prediction"
  lines = sprintf("<CSDGEffect> ALE change of the %s between the %s and %s percentiles in held-out folds; %d folds",
    scale, .csdg_ordinal(x$probs[[1L]]), .csdg_ordinal(x$probs[[2L]]), fits$K)
  for (l in learners) {
    rows = x$summary[x$summary$learner == l][order(-abs(mean_change))]
    width = max(nchar(c("predictor", rows$feature)))
    lines = c(lines, sprintf("%s:", .csdg_learner_label(fits, l)),
      sprintf("  %s   mean change   corrected 95%% interval   folds with a positive change",
        formatC("predictor", width = -width)),
      sprintf("  %s %13s %24s %30s", formatC(rows$feature, width = -width), .csdg_num(rows$mean_change),
        sprintf("[%s, %s]", .csdg_num(rows$lower), .csdg_num(rows$upper)), sprintf("%d/%d", rows$folds_positive,
          rows$K)))
  }
  c(lines, "The interval is reported descriptively; claim_direction() states a direction and csdg_check() checks it.")
}

#' @rdname csdg_effect
#' @export
print.CSDGEffect = function(x, learner = NULL, ...) {
  cat(format(x, learner = learner, ...), sep = "\n")
  invisible(x)
}

#' @rdname csdg_effect
#' @param keep.rownames Ignored.
#' @param level For `as.data.table()`: `"summary"`, `"change"` (per fold), or `"curve"` (ALE per fold).
#' @export
as.data.table.CSDGEffect = function(x, keep.rownames = FALSE, ..., level = c("summary", "change", "curve")) {
  level = match.arg(level)
  copy(switch(level, summary = x$summary, change = x$changes, curve = x$curves))
}

#' @rdname csdg_effect
#' @param features For `plot()`: predictors shown; defaults to all.
#' @export
plot.CSDGEffect = function(x, learner = NULL, features = NULL, ...) {
  fits = x$fits
  learners = .csdg_resolve_learner_names(fits, learner %||% names(fits$resamples))
  features = features %||% x$features
  assert_subset(features, x$features, .var.name = "features")
  all_curves = x$curves
  curves = all_curves[all_curves$learner %in% learners & all_curves$feature %in% features]
  curves[, learner_label__ := factor(unname(fits$labels[learner]), levels = unname(fits$labels[learners]))]
  curves[, group__ := paste(learner, iteration)]
  ylab = if (identical(fits$task_type, "classif")) "ALE (predicted probability, centered)" else
    "ALE (prediction, centered)"
  ggplot(curves, aes(x = x, y = ale, group = group__)) +
    geom_line(alpha = 0.5, linewidth = 0.4, color = autoiml_palette()$metric[["primary"]]) +
    ggplot2::facet_grid(learner_label__ ~ feature, scales = "free_x") +
    labs(x = NULL, y = ylab)
}
