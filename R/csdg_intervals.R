#' Corrected resampled t interval for a learner-level mean
#'
#' @description
#' Computes the corrected resampled t interval of Nadeau and Bengio (2003) for the mean of a learner-level quantity
#' over K cross-validation folds, such as the per-fold PFI of an item or the per-fold difference between the PFI of
#' two items. Molnar et al. (2023) recommend this correction for learner PFI.
#' The spread across folds understates the uncertainty of a learner average, because the fits share training data;
#' the correction inflates the variance of the mean:
#' \deqn{\hat\sigma^2_{corr} = (1/K + n_{test}/n_{train})\, s^2,}
#' where \eqn{s^2} is the sample variance of the K per-fold values (denominator K - 1).
#' The interval uses the t distribution with K - 1 degrees of freedom.
#'
#' Reported descriptively: with K-fold cross-validation the corrected interval is still too lenient, so a property
#' that hinges on it is open when the interval includes zero or excludes it by less than the smallest relevant
#' difference fixed in the criterion (Step 4 of the article). When error control matters, the claim needs new data.
#'
#' @param values Numeric vector of per-fold values (length K >= 2, finite), or a result of [csdg_fold_pfi()]. For a
#'   PFI result, supply `feature_group`; the per-fold values are then the fold importance of that group, minus
#'   `factor` times the fold importance of the group `minus` if given.
#' @param n_train,n_test Numbers of training and test observations per fold: scalars or vectors of length K. Vectors
#'   are summarized by the ratio of their means. For a PFI result, `n_test` defaults to the fold sizes stored in it
#'   (`n_assessment`); `n_train` must be supplied.
#' @param level Confidence level.
#' @param alternative `"two.sided"`, `"greater"` (lower limit only), or `"less"` (upper limit only).
#' @param feature_group,minus,factor Used only when `values` is a [csdg_fold_pfi()] result.
#'
#' @return A one-row `data.table` with `estimate` (mean of the values), `se` (corrected standard error), `lower`,
#'   `upper`, `df`, `K`, `level`, `alternative`, `se_uncorrected` (\eqn{s/\sqrt{K}}), `correction_factor`
#'   (\eqn{1 + K n_{test}/n_{train}}, the ratio of the corrected to the uncorrected variance), `n_train`, and
#'   `n_test`; the attribute `note` states how to report the interval. If all values are equal, `se` is 0 and the
#'   interval is degenerate.
#' @references
#' Nadeau, C., and Bengio, Y. (2003). Inference for the generalization error. Machine Learning, 52, 239-281.
#'
#' Molnar, C., Freiesleben, T., König, G., Herbinger, J., Reisinger, T., Casalicchio, G., Wright, M. N., and
#' Bischl, B. (2023). Relating the partial dependence plot and permutation feature importance to the data generating
#' process. In World Conference on Explainable Artificial Intelligence (pp. 456-479). Springer.
#' @examples
#' csdg_learner_pfi_interval(c(0.030, 0.034, 0.028, 0.036, 0.032), n_train = 800, n_test = 200)
#' @export
csdg_learner_pfi_interval = function(values, n_train = NULL, n_test = NULL, level = 0.95,
                                     alternative = c("two.sided", "greater", "less"),
                                     feature_group = NULL, minus = NULL, factor = 1) {
  alternative = match.arg(alternative)
  if (is.list(values) && !is.null(values$per_iteration)) {
    extracted = .csdg_pfi_fold_values(values, feature_group, minus, factor)
    values = extracted$values
    n_train = n_train %||% extracted$n_train
    n_test = n_test %||% extracted$n_test
  }
  assert_numeric(values, finite = TRUE, any.missing = FALSE, min.len = 2L, .var.name = "values")
  if (is.null(n_train) || is.null(n_test)) {
    .csdg_stop("Supply `n_train` and `n_test`, the training and test sizes of the folds.")
  }
  k = length(values)
  assert_numeric(n_train, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L, .var.name = "n_train")
  assert_numeric(n_test, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L, .var.name = "n_test")
  for (name in c("n_train", "n_test")) {
    value = get(name)
    if (!length(value) %in% c(1L, k) || any(value <= 0)) {
      .csdg_stop("`%s` must be positive, with length 1 or %d.", name, k)
    }
  }
  assert_number(level, lower = 0, upper = 1, .var.name = "level")
  if (level <= 0 || level >= 1) .csdg_stop("`level` must lie strictly between 0 and 1.")
  ratio = mean(n_test) / mean(n_train)
  estimate = mean(values)
  s2 = stats::var(values)
  if (!is.finite(s2)) .csdg_stop("The variance of `values` is not finite.")
  se = sqrt((1 / k + ratio) * s2)
  df = k - 1L
  q = switch(alternative,
    two.sided = stats::qt(1 - (1 - level) / 2, df),
    stats::qt(level, df)
  )
  lower = if (identical(alternative, "less")) -Inf else estimate - q * se
  upper = if (identical(alternative, "greater")) Inf else estimate + q * se
  out = data.table(
    estimate = estimate,
    se = se,
    lower = lower,
    upper = upper,
    df = df,
    K = k,
    level = level,
    alternative = alternative,
    se_uncorrected = sqrt(s2 / k),
    correction_factor = 1 + k * ratio,
    n_train = mean(n_train),
    n_test = mean(n_test)
  )
  data.table::setattr(out, "note", paste(
    "Reported descriptively; with K-fold cross-validation the corrected interval is still too lenient, so a",
    "property that hinges on it is open when the interval includes zero or excludes it by less than the",
    "smallest relevant difference."
  ))
  out[]
}

.csdg_pfi_fold_values = function(pfi, feature_group, minus = NULL, factor = 1) {
  assert_string(feature_group, min.chars = 1L, .var.name = "feature_group")
  assert_string(minus, min.chars = 1L, null.ok = TRUE, .var.name = "minus")
  assert_number(factor, finite = TRUE, .var.name = "factor")
  tab = data.table::as.data.table(pfi$per_iteration)
  groups = unique(tab$feature_group)
  for (group in c(feature_group, minus)) {
    if (!group %in% groups) .csdg_stop("Feature group `%s` is not in the PFI result.", group)
  }
  first_rows = which(tab$feature_group == feature_group)
  first = tab[first_rows]
  first = first[order(first$iteration)]
  values = first$importance
  if (!is.null(minus)) {
    second_rows = which(tab$feature_group == minus)
    second = tab[second_rows]
    second = second[order(second$iteration)]
    if (!identical(first$iteration, second$iteration)) {
      .csdg_stop("The two feature groups do not cover the same folds.")
    }
    values = values - factor * second$importance
  }
  list(values = values, n_train = NULL, n_test = first$n_assessment)
}

#' Monte Carlo error of a difference between two PFI means
#'
#' @description
#' Applies the Monte Carlo rule of Step 4 of the article: an absolute difference between two permutation means is
#' beyond the error due to random permutation if it exceeds the .975 quantile (for `level = 0.95`) of the t
#' distribution with R - 1 degrees of freedom times its Monte Carlo standard error, where R is the number of
#' permutations. This rule screens out permutation noise, not sampling uncertainty.
#'
#' By default the repetitions of the two feature groups are treated as independent, as in [csdg_fold_pfi()], which
#' draws the permutations of each feature group with its own seed:
#' \eqn{se = \sqrt{var(x)/R_x + var(y)/R_y}} with \eqn{\min(R_x, R_y) - 1} degrees of freedom.
#' With `paired = TRUE` (or `y = NULL`, when `x` holds the per-repetition differences), \eqn{se = sd(d)/\sqrt{R}}
#' with R - 1 degrees of freedom.
#'
#' @param x Per-repetition importance values of the first feature group in one fold, or the per-repetition
#'   differences if `y` is `NULL`. Alternatively, a [csdg_fold_pfi()] result; then `y` and `z` name the two feature
#'   groups (see Examples), and one row per fold is returned.
#' @param y Per-repetition importance values of the second feature group (same fold), or `NULL`. When `x` is a
#'   [csdg_fold_pfi()] result, the name of the first feature group.
#' @param z Only when `x` is a [csdg_fold_pfi()] result: the name of the second feature group.
#' @param level Level of the quantile (0.95 uses the .975 quantile).
#' @param paired Whether `x` and `y` come from the same permutations.
#' @param iteration Optional folds (resampling iterations) to keep when `x` is a [csdg_fold_pfi()] result.
#'
#' @return A `data.table` with `estimate`, `se`, `df`, `threshold` (quantile times `se`),
#'   `beyond_monte_carlo_error` (`abs(estimate) > threshold`), `level`, `paired`, `n_x`, and `n_y`; for a PFI result
#'   also `iteration`, `first`, and `second`.
#' @examples
#' csdg_pfi_mc_difference(c(2.00, 2.02, 1.98, 2.01, 1.99), c(0, 0.01, -0.01, 0.005, -0.005))
#' csdg_pfi_mc_difference(c(0.003, 0.001, 0.002, 0.004, 0.000))
#' @export
csdg_pfi_mc_difference = function(x, y = NULL, z = NULL, level = 0.95, paired = FALSE, iteration = NULL) {
  if (is.list(x) && !is.null(x$raw)) {
    return(.csdg_pfi_mc_difference_pfi(x, first = y, second = z, level = level, iteration = iteration))
  }
  assert_numeric(x, finite = TRUE, any.missing = FALSE, min.len = 2L, .var.name = "x")
  assert_numeric(y, finite = TRUE, any.missing = FALSE, min.len = 2L, null.ok = TRUE, .var.name = "y")
  assert_flag(paired, .var.name = "paired")
  assert_number(level, lower = 0, upper = 1, .var.name = "level")
  if (level <= 0 || level >= 1) .csdg_stop("`level` must lie strictly between 0 and 1.")
  if (is.null(y) || paired) {
    d = if (is.null(y)) x else {
      if (length(x) != length(y)) .csdg_stop("Paired repetitions require `x` and `y` of equal length.")
      x - y
    }
    estimate = mean(d)
    se = stats::sd(d) / sqrt(length(d))
    df = length(d) - 1L
    paired = TRUE
    n_y = if (is.null(y)) NA_integer_ else length(y)
  } else {
    estimate = mean(x) - mean(y)
    se = sqrt(stats::var(x) / length(x) + stats::var(y) / length(y))
    df = min(length(x), length(y)) - 1L
    n_y = length(y)
  }
  threshold = stats::qt(1 - (1 - level) / 2, df) * se
  data.table(
    estimate = estimate,
    se = se,
    df = as.integer(df),
    threshold = threshold,
    beyond_monte_carlo_error = abs(estimate) > threshold,
    level = level,
    paired = paired,
    n_x = length(x),
    n_y = as.integer(n_y)
  )
}

.csdg_pfi_mc_difference_pfi = function(pfi, first, second, level, iteration = NULL) {
  assert_string(first, min.chars = 1L, .var.name = "y (first feature group)")
  assert_string(second, min.chars = 1L, .var.name = "z (second feature group)")
  raw = data.table::as.data.table(pfi$raw)
  for (group in c(first, second)) {
    if (!group %in% raw$feature_group) .csdg_stop("Feature group `%s` is not in the PFI result.", group)
  }
  iterations = sort(unique(raw$iteration))
  if (!is.null(iteration)) iterations = intersect(iterations, iteration)
  rows = lapply(iterations, function(i) {
    a = raw$importance[raw$iteration == i & raw$feature_group == first]
    b = raw$importance[raw$iteration == i & raw$feature_group == second]
    out = csdg_pfi_mc_difference(a, b, level = level)
    out[, `:=`(iteration = as.integer(i), first = first, second = second)]
    data.table::setcolorder(out, c("iteration", "first", "second"))
    out
  })
  data.table::rbindlist(rows)
}
