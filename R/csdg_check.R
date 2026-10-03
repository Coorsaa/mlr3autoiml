#' Compute the default checks of a claim
#'
#' @description
#' Fourth step of the analysis-first interface: computes the properties that a claim derived with
#' [claim_relies_mainly()], [claim_top_k()], [claim_order()], or [claim_direction()] requires, with the default
#' criteria described in the article, and records each as a [csdg_evidence_record()].
#' [csdg_assess()] then applies the decision rule with [csdg_adjudicate_claim()].
#'
#' The checks are:
#' * G0a (specification): the claim and its six scope elements, generated from the analysis.
#' * G0b (measurement): supported when the claim names the analyzed variables; open when it names constructs, until
#'   validity evidence is entered with [csdg_judge()].
#' * G1 (predictive performance, context): held-out loss of each covered learner versus the model without
#'   predictors.
#' * G2 (content): the claim's words translated into a margin on the learner average, for example "PFI of each named
#'   predictor minus `factor` times the PFI of the largest other predictor".
#'   A claim that covers several learners records its content under G6a instead, one row per learner.
#' * G2 (minimum importance): each named predictor has a mean PFI of at least `min_importance` times the learner's
#'   improvement over the model without predictors, in the same loss (under G6a for several learners).
#' * G2 (procedure): the claim names marginal PFI; if grouped or conditional PFI was computed
#'   ([csdg_importance()] with `groups`, `conditional`, or `strata`), a reversal of the claim's content contradicts
#'   this property unless the claim was worded with `marginal_only = TRUE` ("Under marginal permutation").
#'   A comparison counts only if the marginal result holds for that learner; otherwise the perturbation changes
#'   nothing.
#' * G2 (correlation): if a named predictor correlates at least `max_cor` (in absolute value) with another
#'   predictor and no grouped or conditional PFI covers it, marginal PFI may be redistributed between the two; this
#'   is an unresolved threat that leaves G2 open, or context with `marginal_only = TRUE`.
#' * G5 (stability): in each fold, the claim's selection rule is applied again (for example, the top `k` and the
#'   ratio are recomputed; ties at the cutoff do not count as reproduced), and the share of folds that reproduce the
#'   result must be at least `stability`; neighboring cutoffs are reported.
#'   A failing fold counts only if it fails beyond Monte Carlo error (Holm-adjusted when every fold is required) and
#'   by at least the smallest relevant difference; otherwise it is undecided.
#'   The property is supported if enough folds hold, contradicted if too few would hold even if every undecided fold
#'   held, and open otherwise.
#' * G6a (models): when the claim covers several learners, its content and minimum importance are required for each;
#'   otherwise the other learners' results are reported as context.
#'
#' Status of a margin (content, and the grouped or conditional comparisons of the procedure): the margin is
#' supported if it holds beyond Monte Carlo error and, with `interval_rule = "open_if_inconclusive"`, the lower bound
#' of its corrected resampled t interval ([csdg_learner_pfi_interval()]) is at least the smallest relevant
#' difference; it is contradicted if it fails beyond Monte Carlo error and the upper bound is at most minus the
#' smallest relevant difference; otherwise it is open.
#' The rule is symmetric: the article treats a property that hinges on a difference whose corrected interval
#' includes zero, or excludes it by less than the smallest relevant difference, as open, in either direction.
#' With `interval_rule = "report_only"`, the interval is only reported.
#' The minimum importance is supported if the mean PFI is at least the minimum beyond Monte Carlo error, open within
#' Monte Carlo error, and contradicted if it is below beyond Monte Carlo error and (by default) the upper bound of its
#' corrected interval is below the minimum.
#' For direction claims, the change must reach `min_change` with its lower bound, and a contradiction needs a change
#' in the opposite direction whose upper bound is at most `-min_change`.
#'
#' The Monte Carlo rule compares a difference with the .975 quantile (for `level = 0.95`) of the t distribution with
#' R - 1 degrees of freedom times its Monte Carlo standard error ([csdg_pfi_mc_difference()]); a decisive difference
#' within Monte Carlo error leaves the property open.
#'
#' Sources of the default criteria: 1% of the improvement (minimum importance), 9 of 10 folds (stability), and a
#' factor of 2 for "relies mainly on" are conventions of the article; the corrected interval rule is its
#' recommendation; `min_change` and `max_cor` are package defaults.
#' Each record states the source of its criterion.
#'
#' @param claim A claim from [claim_relies_mainly()], [claim_top_k()], [claim_order()], or [claim_direction()].
#' @param x For `csdg_check()`: a [csdg_importance()] result on the same data, with the same loss and the learners
#'   the claim covers.
#'   It may differ from the result the claim was derived from, for example by added conditional PFI.
#'   For direction claims, a [csdg_effect()] or [csdg_fit()] result also works.
#'   Use [csdg_confirm()] for new data.
#'   For the methods: a `CSDGCheck` object.
#' @param min_importance Minimum importance as a share of each covered learner's mean improvement over the model
#'   without predictors (default 1%).
#' @param stability Required share of folds; the criterion is "in at least `ceiling(stability * K)` of the K folds".
#' @param min_difference Smallest relevant difference in loss units; defaults to `min_importance` times each
#'   learner's improvement.
#' @param min_change For direction claims: smallest relevant change of the prediction between the two percentiles;
#'   defaults to 0.02 on the probability scale (classification) or 0.1 times the standard deviation of the outcome
#'   (regression).
#' @param max_cor Absolute correlation from which a named predictor without grouped or conditional PFI is flagged
#'   (default 0.7).
#' @param level Confidence level of the corrected interval and of the Monte Carlo quantile.
#' @param interval_rule `"open_if_inconclusive"` (default; see Description) or `"report_only"`.
#' @param judgments Optional [csdg_judge()] result or list of them, for properties that cannot be computed.
#' @param note Optional note stored with the check.
#'
#' @return A `CSDGCheck` list with `claim`, `criteria` (fixed values, their sources, and the date they were fixed),
#'   `table` (one row per property or context entry: `gate`, `gate_name`, `check`, `learner`, `role`, `property`,
#'   `criterion`, `observation`, `relevance`, `status`, and `summary`), `details` (the computed numbers per check),
#'   `records` (one [csdg_evidence_record()] per row), `plan` ([csdg_gate_plan()]), `measurement`, `explanation`,
#'   `judgments`, `note`, `origin`, and `source`.
#'   `print()` shows gate, check, status, and observation (`details = TRUE` adds criterion and relevance), `plot()`
#'   draws the fold values against the criterion, and `as.data.table()` returns `table`.
#' @seealso [csdg_assess()], [csdg_judge()], [csdg_confirm()]
#' @examples
#' set.seed(1)
#' n = 200
#' x = data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
#' x$y = 2 * x$a + x$b + rnorm(n)
#' task = mlr3::as_task_regr(x, target = "y", id = "toy")
#' fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3, seed = 1)
#' imp = csdg_importance(fits, repetitions = 5, seed = 2)
#' claim = claim_relies_mainly(imp, k = 1)
#' chk = csdg_check(claim, imp)
#' chk
#' csdg_assess(chk)
#' @export
csdg_check = function(claim, x, min_importance = 0.01, stability = 0.9, min_difference = NULL, min_change = NULL,
                      max_cor = 0.7, level = 0.95, interval_rule = c("open_if_inconclusive", "report_only"),
                      judgments = NULL, note = NULL) {
  assert_class(claim, "CSDGDerivedClaim", .var.name = "claim")
  imp = .csdg_check_input(x, claim)
  assert_number(min_importance, lower = 0, upper = 1, .var.name = "min_importance")
  if (min_importance <= 0 || min_importance >= 1) .csdg_stop("`min_importance` must lie strictly between 0 and 1.")
  assert_number(stability, lower = 0, upper = 1, .var.name = "stability")
  if (stability <= 0) .csdg_stop("`stability` must be greater than 0.")
  assert_number(min_difference, lower = 0, finite = TRUE, null.ok = TRUE, .var.name = "min_difference")
  assert_number(min_change, lower = 0, finite = TRUE, null.ok = TRUE, .var.name = "min_change")
  assert_number(max_cor, lower = 0, upper = 1, .var.name = "max_cor")
  assert_number(level, lower = 0, upper = 1, .var.name = "level")
  if (level <= 0 || level >= 1) .csdg_stop("`level` must lie strictly between 0 and 1.")
  interval_rule = match.arg(interval_rule)
  judgments = .csdg_normalize_judgments(judgments)
  assert_string(note, min.chars = 1L, null.ok = TRUE, .var.name = "note")
  .csdg_check_compatible(claim, imp)
  is_default = function(value, default) isTRUE(all.equal(value, default))
  analyst = "set by the analyst"
  direction = identical(claim$kind, "direction")
  criteria = list(
    min_importance = min_importance,
    stability = stability,
    min_difference_supplied = !is.null(min_difference),
    min_difference_value = min_difference,
    min_change = if (direction) min_change %||% .csdg_default_min_change(imp$fits) else NULL,
    max_cor = max_cor,
    factor = if (identical(claim$kind, "relies_mainly")) claim$factor,
    level = level,
    interval_rule = interval_rule,
    fixed = format(Sys.Date()),
    sources = list(
      min_importance = if (is_default(min_importance, 0.01)) "article convention" else analyst,
      stability = if (is_default(stability, 0.9)) "article convention" else analyst,
      factor = if (identical(claim$kind, "relies_mainly")) {
        if (is_default(claim$factor, 2)) "article convention" else analyst
      },
      min_difference = if (is.null(min_difference)) "article recommendation (equal to the minimum importance)" else
        analyst,
      interval_rule = if (identical(interval_rule, "open_if_inconclusive") && is_default(level, 0.95)) {
        "article recommendation"
      } else {
        analyst
      },
      min_change = if (direction) if (is.null(min_change)) "package default" else analyst,
      max_cor = if (is_default(max_cor, 0.7)) "package default" else analyst
    )
  )
  .csdg_check_impl(claim, imp, criteria, judgments, note, origin = "retrospective_exploratory")
}

# Normalizes the results passed to csdg_check(): a CSDGImportance, or for direction claims also fits or effects.
.csdg_check_input = function(x, claim) {
  if (inherits(x, "CSDGImportance")) return(x)
  if (inherits(x, c("CSDGFits", "CSDGEffect"))) {
    if (!identical(claim$kind, "direction")) {
      .csdg_stop("A %s claim is checked on PFI; pass a csdg_importance() result.", gsub("_", " ", claim$kind))
    }
    return(.csdg_performance_only(if (inherits(x, "CSDGEffect")) x$fits else x))
  }
  .csdg_stop("`x` must be a csdg_importance() result%s.", if (identical(claim$kind, "direction")) {
    ", a csdg_effect() result, or a csdg_fit() result"
  } else "")
}

# An importance-like object without PFI, with the held-out losses and the improvement (for direction claims).
.csdg_performance_only = function(fits) {
  .csdg_check_fits(fits)
  learners = names(fits$resamples)
  losses = .csdg_all_fold_losses(fits)
  fold_of = unique(as.data.table(fits$resamples[[1L]]$fold_scores)[, .(iteration, fold)])
  tables = .csdg_performance_tables(fits, losses, learners, fold_of)
  structure(list(fits = fits, learners = learners, loss = fits$loss, measure = fits$measure$id,
    repetitions = NA_integer_, groups = NULL, strata_features = character(), strata_definition = character(),
    conditional_on = list(), pfi = NULL, conditional = NULL, performance = tables$performance, fold_pfi = NULL,
    summary = NULL, improvement = tables$improvement), class = c("CSDGPerformance", "list"))
}

#' Enter a judgment on a property that cannot be computed
#'
#' @description
#' Records the analyst's judgment on a property of a claim that the package cannot compute, such as validity evidence
#' for a construct named by the claim (G0b) or a coding error found in the data.
#' Pass the result to [csdg_check()] (`judgments`).
#' A judgment on G0b replaces the package's G0b entry; a judgment on any other gate is added as a further required
#' property.
#' Because the decision rule keeps the worst status per gate, a judgment can make a computed property open or
#' contradicted but never offsets a computed contradiction.
#'
#' @param gate Gate identifier, for example `"G0b"`.
#' @param status `supported()`, `contradicted()`, or `open_status()`.
#' @param note The observation and its basis, for example "Reliability .86 and scalar invariance across waves
#'   (Table S4)".
#' @param property Optional property text; defaults to the package's text for the gate.
#'
#' @return A `CSDGJudgment` list with `gate`, `status`, `note`, `property`, and `date`.
#' @examples
#' csdg_judge("G0b", supported(), note = "Reliability .86 and scalar invariance across the two waves.")
#' @export
csdg_judge = function(gate, status, note, property = NULL) {
  assert_choice(gate, .csdg_requirement_ids, .var.name = "gate")
  assert_choice(status, .csdg_property_statuses, .var.name = "status")
  assert_string(note, min.chars = 1L, .var.name = "note")
  assert_string(property, min.chars = 1L, null.ok = TRUE, .var.name = "property")
  structure(
    list(gate = gate, status = status, note = note, property = property %||% .csdg_default_property(gate),
      date = format(Sys.Date())),
    class = c("CSDGJudgment", "list")
  )
}

#' @export
format.CSDGJudgment = function(x, ...) {
  sprintf("<CSDG judgment> %s %s: %s", x$gate, x$status, x$note)
}

#' @export
print.CSDGJudgment = function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

.csdg_default_property = function(gate) {
  switch(gate,
    G0a = "The claim and its six scope elements are stated.",
    G0b = "The data cover what the claim names.",
    G1 = "The learner predicts adequately on held-out data.",
    G2 = "The procedure computes the named quantity, and the result shows what the claim says.",
    G5 = "The result persists when the same quantity is estimated again.",
    G6a = "The result holds for all learners the claim covers.",
    sprintf("The property of %s holds.", gate)
  )
}

.csdg_normalize_judgments = function(judgments) {
  if (is.null(judgments)) return(list())
  if (inherits(judgments, "CSDGJudgment")) return(list(judgments))
  assert_list(judgments, .var.name = "judgments")
  if (!all(vapply(judgments, inherits, logical(1L), what = "CSDGJudgment"))) {
    .csdg_stop("`judgments` must be a csdg_judge() result or a list of them.")
  }
  unname(judgments)
}

.csdg_default_min_change = function(fits) {
  if (identical(fits$task_type, "classif")) return(0.02)
  if (is.null(fits$task)) return(NA_real_)
  0.1 * stats::sd(fits$task$data(cols = fits$target)[[fits$target]], na.rm = TRUE)
}

.csdg_check_compatible = function(claim, imp, new_data = FALSE) {
  fits = imp$fits
  if (!new_data && !identical(fits$data_hash, claim$source$data_hash)) {
    .csdg_stop(paste(
      "`x` was computed on other data than the claim was derived from;",
      "use csdg_confirm() to apply the claim's criteria to new data."
    ))
  }
  if (!is.na(claim$source$loss) && !identical(imp$loss, claim$source$loss)) {
    .csdg_stop("`x` uses the loss %s; the claim was derived with %s.", imp$loss, claim$source$loss)
  }
  missing_learners = setdiff(claim$covered, .csdg_learners(imp))
  if (length(missing_learners)) {
    .csdg_stop("`x` does not contain the learners the claim covers: %s.", paste(missing_learners, collapse = ", "))
  }
  named = c(claim$predictors, claim$first, claim$second, claim$feature)
  missing_features = setdiff(named, fits$features)
  if (length(missing_features)) {
    .csdg_stop("The predictors of the claim are not features of `x`: %s.", paste(missing_features, collapse = ", "))
  }
  if (identical(claim$kind, "direction") && length(setdiff(claim$covered, claim$effect$learner))) {
    .csdg_stop("The claim stores no ALE changes for the learners it covers.")
  }
  invisible(TRUE)
}

# Per-learner criteria derived from the fixed criteria and the improvement in `imp`.
.csdg_resolve_criteria = function(criteria, imp) {
  learners = .csdg_learners(imp)
  improvement = .csdg_improvement(imp, learners)
  criteria$need = as.integer(ceiling(criteria$stability * imp$fits$K - 1e-8))
  criteria$min_difference = stats::setNames(if (criteria$min_difference_supplied) {
    rep(criteria$min_difference_value, length(learners))
  } else {
    criteria$min_importance * pmax(improvement, 0)
  }, learners)
  criteria$tau = stats::setNames(criteria$min_importance * improvement, learners)
  criteria
}

# Split the raw permutation values of a csdg_fold_pfi() result into K x R matrices per feature group.
.csdg_raw_all = function(result) {
  raw = as.data.table(result$raw)
  groups = unique(raw$feature_group)
  stats::setNames(lapply(groups, function(group) {
    rows = raw[raw$feature_group == group]
    rows = rows[order(rows$iteration, rows$permutation_repetition)]
    iterations = sort(unique(rows$iteration))
    matrix(rows$importance, nrow = length(iterations), byrow = TRUE, dimnames = list(iterations, NULL))
  }), groups)
}

.csdg_view = function(imp, learner, criteria) {
  fits = imp$fits
  improvement_row = match(learner, imp$improvement$learner)
  has_pfi = !is.null(imp$pfi)
  P = if (has_pfi) .csdg_fold_matrix(imp, learner, "marginal")
  raw_conditional = if (is.null(imp$conditional[[learner]])) list() else {
    lapply(imp$conditional[[learner]], function(result) .csdg_raw_all(result)[[1L]])
  }
  list(
    learner = learner,
    label = .csdg_learner_label(fits, learner),
    P = P, avg = if (has_pfi) colMeans(P),
    G = if (has_pfi) .csdg_fold_matrix(imp, learner, "grouped"),
    C = if (has_pfi) .csdg_fold_matrix(imp, learner, "conditional"),
    raw = if (has_pfi) .csdg_raw_all(imp$pfi[[learner]]),
    raw_conditional = raw_conditional,
    improvement = imp$improvement[improvement_row],
    I = .csdg_improvement(imp, learner),
    tau = criteria$tau[[learner]],
    delta = criteria$min_difference[[learner]],
    K = fits$K, R = imp$repetitions,
    n_train = fits$n_train, n_test = fits$n_test,
    level = criteria$level
  )
}

.csdg_q = function(level, R) stats::qt(1 - (1 - level) / 2, R - 1L)

# Monte Carlo error of a learner average: of a margin a - factor * b, or of a single value (b = NULL).
.csdg_mc_average = function(raw_a, raw_b = NULL, factor = 1, level = 0.95) {
  K = nrow(raw_a)
  R = ncol(raw_a)
  se_k = vapply(seq_len(K), function(k) {
    if (is.null(raw_b)) {
      stats::sd(raw_a[k, ]) / sqrt(R)
    } else {
      csdg_pfi_mc_difference(raw_a[k, ], factor * raw_b[k, ], level = level)$se
    }
  }, numeric(1L))
  se = sqrt(sum(se_k^2)) / K
  list(se = se, threshold = .csdg_q(level, R) * se, se_k = se_k)
}

.csdg_interval = function(values, view) {
  csdg_learner_pfi_interval(values, n_train = view$n_train, n_test = view$n_test, level = view$level)
}

# Status of one margin (see ?csdg_check): symmetric in the Monte Carlo rule and in the interval rule.
.csdg_margin_status = function(estimate, lower, upper, mc_threshold, delta, rule, strict = FALSE) {
  point = if (strict) estimate > 0 else estimate >= 0
  beyond = is.na(mc_threshold) || abs(estimate) > mc_threshold
  interval = identical(rule, "open_if_inconclusive")
  if (!point && beyond && (!interval || upper <= -delta)) return("contradicted")
  if (point && beyond && (!interval || lower >= delta)) return("supported")
  "open"
}

# Why a margin is open: "within_mc" or "interval"; "" otherwise.
.csdg_margin_reason = function(status, estimate, mc_threshold) {
  if (!identical(status, "open")) return("")
  if (!is.na(mc_threshold) && abs(estimate) <= mc_threshold) "within_mc" else "interval"
}

# Status of a floor (minimum importance): open within Monte Carlo error; a shortfall must be established.
.csdg_floor_status = function(estimate, tau, mc_threshold, upper, rule, has_scale = TRUE) {
  if (!has_scale) return("open")
  if (abs(estimate - tau) <= mc_threshold) return("open")
  if (estimate >= tau) return("supported")
  if (!identical(rule, "open_if_inconclusive") || upper < tau) return("contradicted")
  "open"
}

.csdg_worst = function(statuses) {
  if (any(statuses == "contradicted")) "contradicted" else if (any(statuses == "open")) "open" else "supported"
}

# One margin on the learner average: fold values va - factor * vb with permutation values ra, rb.
.csdg_margin = function(view, predictor, versus, va, vb, ra, rb, factor, strict, rule) {
  values = va - factor * vb
  estimate = mean(values)
  interval = .csdg_interval(values, view)
  mc = .csdg_mc_average(ra, rb, factor, view$level)
  status = .csdg_margin_status(estimate, interval$lower, interval$upper, mc$threshold, view$delta, rule, strict)
  data.table(
    predictor = predictor, versus = versus, factor = factor, estimate = estimate, lower = interval$lower,
    upper = interval$upper, se = interval$se, mc_se = mc$se, mc_threshold = mc$threshold,
    beyond_mc = abs(estimate) > mc$threshold,
    point_holds = if (strict) estimate > 0 else estimate >= 0,
    status = status, reason = .csdg_margin_reason(status, estimate, mc$threshold)
  )
}

# Content of a ranking or order claim on the learner average.
.csdg_content = function(claim, view, criteria) {
  if (identical(claim$kind, "direction")) return(.csdg_content_direction(claim, view, criteria))
  P = view$P
  avg = view$avg
  rule = criteria$interval_rule
  if (identical(claim$kind, "order")) {
    a = claim$first
    b = claim$second
    margins = .csdg_margin(view, a, b, P[, a], P[, b], view$raw[[a]], view$raw[[b]], 1, strict = TRUE, rule)
    return(list(kind = "order", margins = margins, averages = avg[c(a, b)], largest_other = b,
      ratios = NULL, margins_per_fold_max = NULL, status = .csdg_worst(margins$status)))
  }
  S = claim$predictors
  f = if (identical(claim$kind, "relies_mainly")) claim$factor else 1
  others = setdiff(colnames(P), S)
  other_avg = avg[others]
  largest = others[order(-other_avg, seq_along(other_avg))][[1L]]
  strict = identical(claim$kind, "top_k")
  margins = rbindlist(lapply(S, function(j) {
    .csdg_margin(view, j, largest, P[, j], P[, largest], view$raw[[j]], view$raw[[largest]], f, strict, rule)
  }))
  fold_max = apply(P[, others, drop = FALSE], 1L, max)
  per_fold = rbindlist(lapply(S, function(j) {
    values = P[, j] - f * fold_max
    interval = .csdg_interval(values, view)
    data.table(predictor = j, estimate = mean(values), lower = interval$lower, upper = interval$upper)
  }))
  list(kind = claim$kind, margins = margins, averages = avg[c(S, largest)], largest_other = largest,
    ratios = if (avg[[largest]] > 0) avg[S] / avg[[largest]] else stats::setNames(rep(NA_real_, length(S)), S),
    margins_per_fold_max = per_fold, status = .csdg_worst(margins$status))
}

.csdg_direction_values = function(claim, learner) {
  rows = which(claim$effect$learner == learner)
  effect = claim$effect[rows][order(iteration)]
  (if (identical(claim$direction, "rise")) 1 else -1) * effect$change
}

.csdg_content_direction = function(claim, view, criteria) {
  values = .csdg_direction_values(claim, view$learner)
  interval = .csdg_interval(values, view)
  estimate = mean(values)
  minimum = criteria$min_change
  check_interval = identical(criteria$interval_rule, "open_if_inconclusive")
  status = if (estimate >= minimum && (!check_interval || interval$lower >= minimum)) {
    "supported"
  } else if (estimate < minimum && (!check_interval || interval$upper <= -minimum)) {
    "contradicted"
  } else {
    "open"
  }
  margins = data.table(predictor = claim$feature, versus = NA_character_, factor = 1, estimate = estimate,
    lower = interval$lower, upper = interval$upper, se = interval$se, mc_se = NA_real_, mc_threshold = NA_real_,
    beyond_mc = TRUE, point_holds = estimate >= minimum, status = status,
    reason = if (identical(status, "open")) "interval" else "")
  list(kind = "direction", margins = margins, averages = NULL, largest_other = NULL, ratios = NULL,
    margins_per_fold_max = NULL, status = status, values = values)
}

# Minimum importance of the named predictors.
.csdg_minimum_importance = function(claim, view, criteria) {
  named = if (identical(claim$kind, "order")) claim$first else claim$predictors
  has_scale = view$I > 0
  tab = rbindlist(lapply(named, function(j) {
    mc = .csdg_mc_average(view$raw[[j]], NULL, 1, view$level)
    estimate = view$avg[[j]]
    interval = .csdg_interval(view$P[, j], view)
    status = .csdg_floor_status(estimate, view$tau, mc$threshold, interval$upper, criteria$interval_rule,
      has_scale)
    data.table(predictor = j, mean_importance = estimate, share = estimate / view$I, tau = view$tau,
      lower = interval$lower, upper = interval$upper, mc_se = mc$se, mc_threshold = mc$threshold,
      holds = estimate >= view$tau, beyond_mc = abs(view$tau - estimate) > mc$threshold, status = status)
  }))
  status = if (!has_scale) "open" else .csdg_worst(tab$status)
  list(table = tab, status = status, no_scale = !has_scale)
}

# Holm step-down over failing folds when every fold is required; otherwise the unadjusted quantile.
.csdg_classify_failing = function(folds, view, criteria) {
  failing = which(!folds$holds & !is.na(folds$se))
  folds[, beyond := NA]
  if (!length(failing)) return(folds)
  K = view$K
  q = .csdg_q(view$level, view$R)
  d = abs(folds$d[failing])
  se = folds$se[failing]
  if (criteria$need >= K) {
    ratio = ifelse(se > 0, d / se, ifelse(d > 0, Inf, 0))
    ordering = order(-ratio)
    beyond_new = logical(length(failing))
    threshold_new = numeric(length(failing))
    still = TRUE
    for (i in seq_along(ordering)) {
      idx = ordering[[i]]
      q_i = stats::qt(max(1 - (1 - view$level) / 2, 1 - (1 - view$level) / (K - i + 1L)), view$R - 1L)
      threshold_new[[idx]] = q_i * se[[idx]]
      if (still && d[[idx]] > threshold_new[[idx]]) beyond_new[[idx]] = TRUE else still = FALSE
    }
  } else {
    threshold_new = q * se
    beyond_new = d > threshold_new
  }
  folds[failing, `:=`(threshold = threshold_new, beyond = beyond_new)]
  folds
}

# The selection rule in one fold: the named predictors must be the top |S| without a tie at the cutoff.
.csdg_fold_ranking = function(Pk, S, f, kind) {
  ordered = names(Pk)[order(-Pk, seq_along(Pk))]
  top = ordered[seq_along(S)]
  others = setdiff(names(Pk), S)
  j = S[which.min(Pk[S])]
  l = others[which.max(Pk[others])]
  separated = min(Pk[S]) > max(Pk[others])
  holds = setequal(top, S) && separated &&
    (!identical(kind, "relies_mainly") || min(Pk[S]) >= f * max(Pk[others]))
  list(holds = holds, j = j, l = l, selected = paste(top, collapse = ", "))
}

# Selection-aware stability (G5): the claim's selection rule applied again in each fold.
.csdg_stability = function(claim, view, criteria) {
  P = view$P
  K = view$K
  kind = claim$kind
  rows = lapply(seq_len(K), function(k) {
    if (identical(kind, "direction")) {
      v = .csdg_direction_values(claim, view$learner)[[k]]
      return(data.table(iteration = k, holds = v >= criteria$min_change, selected = NA_character_, d = v,
        se = NA_real_, threshold = NA_real_))
    }
    Pk = P[k, ]
    if (identical(kind, "order")) {
      a = claim$first
      b = claim$second
      holds = Pk[[a]] > Pk[[b]]
      mc = csdg_pfi_mc_difference(view$raw[[a]][k, ], view$raw[[b]][k, ], level = view$level)
      return(data.table(iteration = k, holds = holds, selected = if (holds) a else b, d = Pk[[a]] - Pk[[b]],
        se = mc$se, threshold = mc$threshold))
    }
    f = if (identical(kind, "relies_mainly")) claim$factor else 1
    fr = .csdg_fold_ranking(Pk, claim$predictors, f, kind)
    f_eff = if (identical(kind, "relies_mainly") && Pk[[fr$l]] >= 0) f else 1
    mc = csdg_pfi_mc_difference(view$raw[[fr$j]][k, ], f_eff * view$raw[[fr$l]][k, ], level = view$level)
    data.table(iteration = k, holds = fr$holds, selected = fr$selected, d = Pk[[fr$j]] - f_eff * Pk[[fr$l]],
      se = mc$se, threshold = mc$threshold)
  })
  folds = rbindlist(rows)
  if (!identical(kind, "direction")) folds[, iteration := as.integer(rownames(P) %||% seq_len(K))]
  if (!identical(kind, "direction")) folds = .csdg_classify_failing(folds, view, criteria)
  if (identical(kind, "direction")) folds[, beyond := !holds]
  folds[, class := fifelse(holds, "holds", fifelse(beyond %in% TRUE & abs(d) >= view$delta, "fails", "undecided"))]
  if (identical(kind, "direction")) folds[, class := fifelse(holds, "holds", "fails")]
  n_h = sum(folds$class == "holds")
  n_u = sum(folds$class == "undecided")
  status = if (n_h >= criteria$need) "supported" else if (n_h + n_u < criteria$need) "contradicted" else "open"
  list(folds = folds, neighbors = .csdg_neighbors(claim, view), n_holds = n_h, n_undecided = n_u,
    n_fails = sum(folds$class == "fails"), need = criteria$need, K = K, status = status)
}

.csdg_neighbors = function(claim, view) {
  if (!claim$kind %in% c("relies_mainly", "top_k")) return(data.table())
  P = view$P
  p = ncol(P)
  m = length(claim$predictors)
  cutoffs = intersect(c(m - 1L, m + 1L), seq_len(p - 1L))
  f = if (identical(claim$kind, "relies_mainly")) claim$factor else 1
  rbindlist(lapply(cutoffs, function(cutoff) {
    S_c = .csdg_top(view$avg, cutoff)
    n = sum(vapply(seq_len(nrow(P)), function(k) .csdg_fold_ranking(P[k, ], S_c, f, claim$kind)$holds,
      logical(1L)))
    data.table(cutoff = cutoff, predictors = paste(S_c, collapse = ", "), folds = n, K = nrow(P))
  }))
}

# Outcome of a grouped or conditional comparison: it counts only if the marginal result holds.
.csdg_outcome = function(status, marginal_holds) {
  if (!marginal_holds) return("unchanged")
  switch(status, supported = "holds", contradicted = "reversed", "inconclusive")
}

# Procedure (G2): does grouped or conditional PFI change the conclusion?
.csdg_procedure = function(claim, imp, view, criteria, content, mi) {
  kind = claim$kind
  rule = criteria$interval_rule
  comparisons = list()
  context = character()
  add = function(...) comparisons[[length(comparisons) + 1L]] <<- data.table(...)
  f = if (identical(kind, "relies_mainly")) claim$factor else 1
  strict = !identical(kind, "relies_mainly")
  marginal_holds = function(j) {
    row = content$margins[content$margins$predictor == j]
    nrow(row) > 0L && all(row$point_holds)
  }
  add_margin = function(perturbation, j, versus, value, versus_value, m) {
    add(perturbation = perturbation, comparison = "content", predictor = j, versus = versus, value = value,
      versus_value = versus_value, estimate = m$estimate, lower = m$lower, upper = m$upper,
      threshold = m$mc_threshold, status = m$status, outcome = .csdg_outcome(m$status, marginal_holds(j)))
  }
  groups = imp$groups
  if (length(groups) && kind %in% c("relies_mainly", "top_k")) {
    S = claim$predictors
    free = names(groups)[vapply(groups, function(g) !length(intersect(g, S)), logical(1L))]
    if (length(free)) {
      grouped_members = unique(unlist(groups[free]))
      singles = setdiff(setdiff(colnames(view$P), S), grouped_members)
      unit_values = c(lapply(free, function(g) view$G[, g]), lapply(singles, function(j) view$P[, j]))
      names(unit_values) = c(free, singles)
      unit_avg = vapply(unit_values, mean, numeric(1L))
      largest = names(unit_avg)[order(-unit_avg, seq_along(unit_avg))][[1L]]
      for (j in S) {
        m = .csdg_margin(view, j, largest, view$P[, j], unit_values[[largest]], view$raw[[j]], view$raw[[largest]],
          f, strict, rule)
        add_margin("grouped", j, largest, view$avg[[j]], unit_avg[[largest]], m)
      }
    }
  }
  if (length(groups) && identical(kind, "order")) {
    for (g in names(groups)) {
      if (length(intersect(groups[[g]], c(claim$first, claim$second)))) {
        context = c(context, sprintf(
          "grouped PFI of %s (%s) %s measures reliance on the group; it does not order its members",
          g, paste(groups[[g]], collapse = ", "), .csdg_num(mean(view$G[, g]))
        ))
      }
    }
  }
  conditioned = imp$strata_features
  if (length(conditioned) && !identical(kind, "direction")) {
    value_of = function(j) if (j %in% conditioned) view$C[, j] else view$P[, j]
    raw_of = function(j) if (j %in% conditioned) view$raw_conditional[[j]] else view$raw[[j]]
    named = if (identical(kind, "order")) claim$first else claim$predictors
    if (identical(kind, "order")) {
      a = claim$first
      b = claim$second
      if (a %in% conditioned && b %in% conditioned) {
        m = .csdg_margin(view, a, b, value_of(a), value_of(b), raw_of(a), raw_of(b), 1, strict = TRUE, rule)
        add_margin("conditional", a, b, mean(value_of(a)), mean(value_of(b)), m)
      }
    } else if (length(intersect(c(claim$predictors, colnames(view$P)), conditioned))) {
      S = claim$predictors
      others = setdiff(colnames(view$P), S)
      other_avg = vapply(others, function(j) mean(value_of(j)), numeric(1L))
      largest = others[order(-other_avg, seq_along(other_avg))][[1L]]
      for (j in S) {
        if (!j %in% conditioned && !largest %in% conditioned) next
        m = .csdg_margin(view, j, largest, value_of(j), value_of(largest), raw_of(j), raw_of(largest), f, strict,
          rule)
        add_margin("conditional", j, largest, mean(value_of(j)), other_avg[[largest]], m)
      }
    }
    for (j in intersect(named, conditioned)) {
      mc = .csdg_mc_average(raw_of(j), NULL, 1, view$level)
      value = mean(value_of(j))
      interval = .csdg_interval(value_of(j), view)
      status = .csdg_floor_status(value, view$tau, mc$threshold, interval$upper, rule, view$I > 0)
      marginal = mi$table[mi$table$predictor == j]
      add(perturbation = "conditional", comparison = "minimum_importance", predictor = j, versus = NA_character_,
        value = value, versus_value = view$tau, estimate = value - view$tau, lower = interval$lower,
        upper = interval$upper, threshold = mc$threshold, status = status,
        outcome = .csdg_outcome(status, nrow(marginal) > 0L && all(marginal$holds)))
    }
  }
  comparisons = rbindlist(comparisons)
  counted = if (nrow(comparisons)) comparisons$outcome[comparisons$outcome != "unchanged"] else character()
  status = if (!length(counted) || isTRUE(claim$marginal_only)) {
    "supported"
  } else if (any(counted == "reversed")) {
    "contradicted"
  } else if (any(counted == "inconclusive")) {
    "open"
  } else {
    "supported"
  }
  list(comparisons = comparisons, context = context, status = status)
}

# Correlation (G2): named predictors that correlate at least `max_cor` with another predictor and that no grouped
# or conditional PFI covers.
.csdg_correlation = function(claim, imp, max_cor) {
  task = imp$fits$task
  if (is.null(task) || identical(claim$kind, "direction")) return(NULL)
  named = c(claim$predictors, claim$first, claim$second)
  covered = c(unlist(imp$groups), imp$strata_features)
  named = setdiff(named, covered)
  if (!length(named)) return(NULL)
  dat = task$data(cols = task$feature_names)
  numeric_features = names(dat)[vapply(dat, is.numeric, logical(1L))]
  named = intersect(named, numeric_features)
  if (!length(named) || length(numeric_features) < 2L) return(NULL)
  r = suppressWarnings(stats::cor(as.matrix(dat[, numeric_features, with = FALSE]),
    use = "pairwise.complete.obs"))
  pairs = data.table()
  for (j in named) {
    values = r[j, ]
    values = values[setdiff(names(values), j)]
    values = values[is.finite(values)]
    if (length(values) && max(abs(values)) >= max_cor) {
      partner = names(values)[which.max(abs(values))]
      pair = paste(sort(c(j, partner)), collapse = "|")
      if (nrow(pairs) && pair %in% pairs$pair) next
      pairs = rbind(pairs, data.table(predictor = j, partner = partner, r = values[[partner]], pair = pair))
    }
  }
  if (!nrow(pairs)) return(NULL)
  pairs
}

.csdg_check_order = c("specification", "measurement", "performance", "content", "minimum_importance", "models",
  "procedure", "correlation", "stability", "judgment")

# Short labels of the checks, used in printouts and reports.
.csdg_check_labels = c(specification = "specification", measurement = "measurement", performance = "performance",
  content = "content", minimum_importance = "minimum importance", models = "other learner",
  procedure = "procedure", correlation = "correlation", stability = "stability", judgment = "judgment")

.csdg_check_label = function(check, gate = NULL) {
  label = unname(.csdg_check_labels[check])
  label[is.na(label)] = check[is.na(label)]
  if (is.null(gate)) label else sprintf("%s (%s)", label, gate)
}

.csdg_criterion_rationales = c(
  specification = "The article requires the claim and its six scope elements (G0a).",
  measurement = "The article requires measures that cover what the claim names (G0b).",
  content = "Translates the claim's words into a margin on the learner average, as the article recommends.",
  minimum_importance = paste("Ranking criteria add a minimum importance relative to the improvement over a model",
    "without predictors, as the article recommends."),
  stability = "Selection-aware stability with the share of folds, as the article recommends.",
  procedure = "The reported wording may not be broader than the scope.",
  correlation = "A strong correlation can redistribute marginal PFI between predictors.",
  models = "A claim that covers several learners holds only if the result holds in each (G6a)."
)

.csdg_prefix = function(label, text) {
  if (grepl("^(Held-out|Selected|The|Change|Whether|No|Not|Once|With|On) ", text)) text = .csdg_lower_first(text)
  paste0(label, ": ", text)
}

.csdg_check_impl = function(claim, imp, criteria, judgments, note, origin) {
  fits = imp$fits
  criteria = .csdg_resolve_criteria(criteria, imp)
  learners = .csdg_learners(imp)
  covered = claim$covered
  several = length(covered) > 1L
  direction = identical(claim$kind, "direction")
  content_gate = if (several) "G6a" else "G2"
  views = lapply(stats::setNames(learners, learners), function(l) .csdg_view(imp, l, criteria))
  for (l in covered) {
    if (views[[l]]$I <= 0) {
      .csdg_warn("%s does not improve on the model without predictors; the minimum importance has no scale.",
        .csdg_sentence(views[[l]]$label))
    }
  }
  rows = list()
  add_row = function(gate, check, learner, role, texts, status, variation = NULL, judgment = NULL) {
    rows[[length(rows) + 1L]] <<- list(gate = gate, check = check, learner = learner, role = role,
      property = texts$property, criterion = texts$criterion, observation = texts$observation,
      relevance = texts$relevance, status = status, summary = texts$summary, variation = variation,
      judgment = judgment, source = texts$source)
  }
  details = list(content = list(), minimum_importance = list(), procedure = list(), stability = list(),
    models = list(), correlation = NULL, improvement = imp$improvement,
    fold_pfi = lapply(views, `[[`, "P"))

  add_row("G0a", "specification", NA_character_, "required_property", .csdg_text_specification(), "supported")
  if (!any(vapply(judgments, function(j) identical(j$gate, "G0b"), logical(1L)))) {
    add_row("G0b", "measurement", NA_character_, "required_property", .csdg_text_measurement(claim, fits),
      if (identical(claim$refers_to, "constructs")) "open" else "supported")
  }
  for (l in covered) {
    add_row("G1", "performance", l, "context", .csdg_text_performance(views[[l]], imp$loss, prefix = several),
      "context")
  }

  models_variation = list(varied_component = "learner", held_constant = c("data", "splits", "loss", "perturbation"),
    same_estimand = FALSE, invariance_claimed = TRUE, same_estimand_rationale = sprintf(
      "Each learner has its own %s; the claim asserts the result for every covered learner.",
      if (direction) "ALE" else "PFI"))
  content_holds = stats::setNames(logical(length(covered)), covered)
  for (l in covered) {
    view = views[[l]]
    content = .csdg_content(claim, view, criteria)
    details$content[[l]] = content
    content_holds[[l]] = all(content$margins$point_holds)
    texts = .csdg_text_content(claim, view, content, criteria, fits, origin)
    mi = if (direction) NULL else .csdg_minimum_importance(claim, view, criteria)
    details$minimum_importance[[l]] = mi
    mi_texts = if (is.null(mi)) NULL else .csdg_text_minimum(claim, view, mi, criteria, imp$loss)
    variation = NULL
    if (several) {
      texts$property = sprintf("The result holds for %s, which the claim covers.", view$label)
      texts$observation = .csdg_prefix(view$label, texts$observation)
      texts$summary = .csdg_prefix(view$label, texts$summary)
      texts$relevance = paste("Data, splits, and perturbation are fixed while the learner varies, and the claim",
        "covers each learner.")
      variation = models_variation
      if (!is.null(mi)) {
        mi_texts$observation = .csdg_prefix(view$label, mi_texts$observation)
        mi_texts$summary = .csdg_prefix(view$label, mi_texts$summary)
      }
    }
    add_row(content_gate, "content", l, "required_property", texts, content$status, variation = variation)
    if (!is.null(mi)) {
      add_row(content_gate, "minimum_importance", l, "required_property", mi_texts, mi$status, variation = variation)
    }
  }
  procedure_variation = list(varied_component = "perturbation (grouped or conditional)",
    held_constant = c("learner", "data", "splits", "loss"), same_estimand = FALSE, invariance_claimed = TRUE,
    same_estimand_rationale = paste("Grouped and conditional PFI compute other quantities; a wording without the",
      "qualifier asserts the result independently of the perturbation."))
  evaluated = if (several) covered[content_holds] else covered
  for (l in covered) {
    view = views[[l]]
    if (!l %in% evaluated) {
      text = sprintf("%s: not evaluated, because the result does not hold on average for this learner (see G6a).",
        view$label)
      add_row("G2", "procedure", l, "context", list(property = NA_character_, criterion = NA_character_,
        observation = text, relevance = "The content (G6a) decides; the failure is not counted twice.",
        summary = sub("\\.$", "", text)), "context")
      next
    }
    proc = .csdg_procedure(claim, imp, view, criteria, details$content[[l]], details$minimum_importance[[l]])
    details$procedure[[l]] = proc
    texts = .csdg_text_procedure(claim, imp, view, proc)
    if (several) {
      texts$observation = .csdg_prefix(view$label, texts$observation)
      texts$summary = .csdg_prefix(view$label, texts$summary)
    }
    add_row("G2", "procedure", l, "required_property", texts, proc$status,
      variation = if (identical(proc$status, "contradicted")) procedure_variation)
  }
  if (several && !length(evaluated)) {
    add_row("G2", "procedure", NA_character_, "required_property", list(
      property = "The procedure computes the named quantity, and the wording is not broader than the scope.",
      criterion = "Held-out marginal PFI as named in the scope.",
      observation = paste("Held-out marginal PFI, as the scope names; no learner satisfies the claim on average, so",
        "no grouped or conditional comparison bears on the wording."),
      relevance = "The content (G6a) decides.",
      summary = "held-out marginal PFI as the scope names"), "supported")
  }
  pairs = .csdg_correlation(claim, imp, criteria$max_cor)
  details$correlation = pairs
  if (!is.null(pairs)) {
    threat = !isTRUE(claim$marginal_only)
    add_row("G2", "correlation", NA_character_, if (threat) "unresolved_threat" else "context",
      .csdg_text_correlation(claim, pairs, criteria, threat), if (threat) "open" else "context")
  }
  stability_variation = list(
    varied_component = if (direction) "training and held-out folds" else c("training and held-out folds",
      "permutations"),
    held_constant = c("learner settings", if (direction) "ALE intervals" else c("loss", "perturbation")),
    same_estimand = TRUE,
    same_estimand_rationale = sprintf("Refits on other folds%s estimate the same learner %s again.",
      if (direction) "" else " and new permutations", if (direction) "ALE" else "PFI"))
  for (l in covered) {
    view = views[[l]]
    if (!l %in% evaluated) {
      text = sprintf("%s: not evaluated, because the result does not hold on average for this learner (see G6a).",
        view$label)
      add_row("G5", "stability", l, "context", list(property = NA_character_, criterion = NA_character_,
        observation = text, relevance = "The content (G6a) decides; the failure is not counted twice.",
        summary = sub("\\.$", "", text)), "context")
      next
    }
    st = .csdg_stability(claim, view, criteria)
    details$stability[[l]] = st
    texts = .csdg_text_stability(claim, view, st, criteria, origin)
    if (several) {
      texts$observation = .csdg_prefix(view$label, texts$observation)
      texts$summary = .csdg_prefix(view$label, texts$summary)
    }
    add_row("G5", "stability", l, "required_property", texts, st$status, variation = stability_variation)
  }
  if (several && !length(evaluated)) {
    add_row("G5", "stability", NA_character_, "required_property", list(
      property = "The result recurs when the learners are refitted on other folds, with the selection repeated.",
      criterion = sprintf("Selected anew in each fold, in at least %d of %d folds.", criteria$need, fits$K),
      observation = "Not evaluated: the result does not hold on average for any covered learner; see G6a.",
      relevance = "Stability is evaluated only for learners whose average satisfies the claim.",
      summary = "not evaluated, because the result does not hold on average for any covered learner"), "open")
  }
  for (l in setdiff(learners, covered)) {
    view = views[[l]]
    content = .csdg_content(claim, view, criteria)
    mi = if (direction) NULL else .csdg_minimum_importance(claim, view, criteria)
    details$models[[l]] = list(content = content, minimum_importance = mi)
    add_row("G6a", "models", l, "context", .csdg_text_models_context(claim, view, content, mi), "context")
  }
  .csdg_finish_check(claim, imp, criteria, rows, details, judgments, note, origin)
}

.csdg_check_cards = function(claim, imp) {
  fits = imp$fits
  direction = identical(claim$kind, "direction")
  missing_features = .csdg_missing_features(fits)
  measurement = csdg_measurement(
    outcome = fits$target,
    predictors = fits$features,
    data_source = paste("mlr3 task", fits$task_id),
    sample_definition = sprintf("all %s rows of the task", .csdg_count(fits$n)),
    unit = "row of the task",
    outcome_scale = if (identical(fits$task_type, "classif")) sprintf("binary, positive class %s", fits$positive) else
      "continuous",
    missingness = if (length(missing_features)) {
      sprintf("missing values in %s, handled by the learners", .csdg_and(missing_features))
    } else {
      "none"
    },
    preprocessing = "as defined in the learners"
  )
  explanation = csdg_explanation(
    method_ids = if (direction) "ale" else "pfi",
    scope = "global",
    target = if (!direction) {
      sprintf("held-out %s", .csdg_loss_label(imp$loss))
    } else if (identical(fits$task_type, "classif")) {
      "predicted probability"
    } else {
      "prediction"
    },
    feature_groups = imp$groups,
    perturbation = list(type = "marginal", distribution = "held-out fold")
  )
  list(measurement = measurement, explanation = explanation)
}

.csdg_finish_check = function(claim, imp, criteria, rows, details, judgments, note, origin) {
  fits = imp$fits
  cards = .csdg_check_cards(claim, imp)
  plan = csdg_gate_plan(claim$record, cards$measurement, cards$explanation)
  required_gates = plan$gate_id[plan$required %in% TRUE]
  for (j in judgments) {
    required = j$gate %in% required_gates
    if (!required) {
      .csdg_warn("The judgment on %s is reported as context: the claim does not require %s.", j$gate, j$gate)
    }
    rows[[length(rows) + 1L]] = list(gate = j$gate, check = "judgment", learner = NA_character_,
      role = if (required) "required_property" else "context", property = j$property,
      criterion = "Judged by the analyst with csdg_judge().", observation = j$note,
      relevance = "Recorded by the analyst with csdg_judge().",
      status = if (required) j$status else "context", summary = j$note, variation = NULL, judgment = j,
      source = "judgment of the analyst")
  }
  learner_order = c(NA_character_, claim$covered, setdiff(.csdg_learners(imp), claim$covered))
  ordering = order(
    match(vapply(rows, `[[`, character(1L), "gate"), .csdg_requirement_ids),
    match(vapply(rows, `[[`, character(1L), "learner"), learner_order),
    match(vapply(rows, `[[`, character(1L), "check"), .csdg_check_order)
  )
  rows = rows[ordering]
  registry = .gate_registry()
  table = rbindlist(lapply(rows, function(row) {
    data.table(gate = row$gate, gate_name = registry$gate_name[match(row$gate, registry$gate_id)] %||% NA_character_,
      check = row$check, learner = row$learner, role = row$role, property = row$property,
      criterion = row$criterion, observation = row$observation, relevance = row$relevance, status = row$status,
      summary = row$summary)
  }))
  records = lapply(rows, .csdg_row_record, criteria = criteria)
  structure(
    list(
      claim = claim,
      criteria = criteria,
      table = table,
      details = details,
      records = records,
      plan = plan,
      measurement = cards$measurement,
      explanation = cards$explanation,
      judgments = judgments,
      note = note,
      origin = origin,
      refit = list(learners = fits$learners, resampling = fits$resampling_template, measure = fits$measure,
        labels = fits$labels, seed = fits$seed %||% NA_integer_),
      source = list(task_id = fits$task_id, n = fits$n, K = fits$K, R = imp$repetitions, loss = imp$loss,
        data_hash = fits$data_hash, row_hashes = fits$row_hashes, groups = imp$groups,
        strata_features = imp$strata_features, conditional_on = imp$conditional_on,
        strata_definition = imp$strata_definition, batch_size = imp$batch_size, seed = imp$seed,
        resampling_label = fits$resampling_label)
    ),
    class = c("CSDGCheck", "list")
  )
}

# Source of the criterion of one row, from the per-criterion sources.
.csdg_row_source = function(row, criteria) {
  s = criteria$sources
  parts = switch(row$check,
    content = c(if (!is.null(s$factor)) sprintf("factor %s: %s", format(criteria$factor), s$factor),
      if (!is.null(s$min_change)) sprintf("minimum change: %s", s$min_change),
      sprintf("corrected interval rule: %s", s$interval_rule),
      sprintf("smallest relevant difference: %s", s$min_difference)),
    minimum_importance = sprintf("minimum importance %s: %s", .csdg_pct_value(criteria$min_importance),
      s$min_importance),
    stability = c(sprintf("share of folds: %s", s$stability),
      if (!is.null(s$min_change)) sprintf("minimum change: %s", s$min_change)),
    procedure = "the scope of the claim (article recommendation)",
    "package default"
  )
  paste(parts, collapse = "; ")
}

.csdg_row_record = function(row, criteria) {
  if (identical(row$role, "context")) {
    return(csdg_evidence_record(row$gate, TRUE, "context", result_direction = "descriptive",
      rationale = row$observation))
  }
  if (identical(row$role, "unresolved_threat")) {
    return(csdg_evidence_record(row$gate, TRUE, "unresolved_threat", rationale = row$observation,
      required_property = row$property, observation = row$observation, relevance_to_proposition = row$relevance))
  }
  if (!is.null(row$judgment)) {
    return(csdg_evidence_record(row$gate, TRUE, "required_property", status = row$status, rationale = row$observation,
      required_property = row$property, observation = row$observation,
      relevance_to_proposition = row$relevance))
  }
  variation = row$variation %||% list()
  csdg_evidence_record(
    row$gate, TRUE, "required_property",
    status = row$status,
    criterion = list(value = row$criterion, direction = "qualitative"),
    criterion_source = row$source %||% .csdg_row_source(row, criteria),
    criterion_rationale = unname(.csdg_criterion_rationales[[row$check]]),
    rationale = row$observation,
    varied_component = variation$varied_component,
    held_constant = variation$held_constant,
    same_estimand = variation$same_estimand %||% NA,
    same_estimand_rationale = variation$same_estimand_rationale,
    invariance_claimed = variation$invariance_claimed %||% NA,
    required_property = row$property,
    observation = row$observation,
    relevance_to_proposition = row$relevance
  )
}

.csdg_criteria_text = function(criteria, claim, K) {
  sources = unique(unlist(criteria$sources))
  source = if (all(sources %in% c("article convention", "article recommendation", "package default",
    "article recommendation (equal to the minimum importance)"))) "defaults, see ?csdg_check" else
    "partly set by the analyst"
  first = if (identical(claim$kind, "direction")) {
    sprintf("minimum change %s", .csdg_crit(criteria$min_change))
  } else {
    sprintf("minimum importance %s of the improvement", .csdg_pct_value(criteria$min_importance))
  }
  interval = if (identical(criteria$interval_rule, "report_only")) {
    sprintf("corrected %s interval reported only", .csdg_pct_value(criteria$level))
  } else {
    sprintf("corrected %s interval must clear the smallest relevant difference", .csdg_pct_value(criteria$level))
  }
  sprintf("Criteria (%s): %s; at least %d of %d folds; Monte Carlo rule; %s.", source, first, criteria$need, K,
    interval)
}

# Rows of the printed table; the default G0a and G0b rows are collapsed into one line.
.csdg_print_rows = function(table) {
  rows = data.table(gate = table$gate, check = .csdg_check_label(table$check), status = table$status,
    observation = table$observation)
  default_scope = which(table$gate %in% c("G0a", "G0b") & table$check %in% c("specification", "measurement"))
  if (length(default_scope) == 2L && all(table$status[default_scope] == "supported")) {
    merged = data.table(gate = "G0a/b", check = "scope", status = "supported", observation = paste(
      "The six scope elements are generated from the analysis;",
      .csdg_lower_first(sub("^The claim names", "the claim names", table$summary[default_scope[[2L]]])), "."))
    merged$observation = sub(" \\.$", ".", merged$observation)
    rows = rbind(merged, rows[-default_scope])
  }
  rows
}

#' @rdname csdg_check
#' @param width For `print()`: line width.
#' @param details For `print()`: if `TRUE`, also prints the criterion and the relevance of each row.
#' @param ... Ignored.
#' @export
format.CSDGCheck = function(x, width = getOption("width", 80L), details = FALSE, ...) {
  assert_flag(details, .var.name = "details")
  rows = .csdg_print_rows(x$table)
  lines = c(
    .csdg_wrap(sprintf("<CSDG check> %s", x$claim$statement), width = width),
    .csdg_wrap(.csdg_criteria_text(x$criteria, x$claim, x$source$K), width = width)
  )
  check_width = max(nchar(rows$check), 5L) + 2L
  status_width = 14L
  exdent = 6L + check_width + status_width
  lines = c(lines, sprintf("%-6s%-*s%-*s%s", "gate", check_width, "check", status_width, "status", "observation"))
  full = x$table
  for (i in seq_len(nrow(rows))) {
    row = rows[i]
    body = strwrap(row$observation, width = max(20L, width - exdent))
    prefix = c(sprintf("%-6s%-*s%-*s", row$gate, check_width, row$check, status_width, row$status),
      rep(strrep(" ", exdent), length(body) - 1L))
    lines = c(lines, paste0(prefix, body))
    if (details) {
      match_row = which(full$gate == row$gate & .csdg_check_label(full$check) == row$check &
        full$observation == row$observation)
      for (field in c("criterion", "relevance")) {
        value = if (length(match_row)) full[[field]][[match_row[[1L]]]] else NA_character_
        if (!is.na(value)) {
          lines = c(lines, strwrap(sprintf("%s: %s", .csdg_sentence(field), value), width = max(20L, width - 2L),
            indent = exdent, exdent = exdent + 2L))
        }
      }
    }
  }
  lines
}

#' @rdname csdg_check
#' @export
print.CSDGCheck = function(x, width = getOption("width", 80L), details = FALSE, ...) {
  cat(format(x, width = width, details = details, ...), sep = "\n")
  invisible(x)
}

#' @rdname csdg_check
#' @param keep.rownames Ignored.
#' @export
as.data.table.CSDGCheck = function(x, keep.rownames = FALSE, ...) {
  copy(x$table)
}

#' @rdname csdg_check
#' @param object A `CSDGCheck` object.
#' @export
summary.CSDGCheck = function(object, ...) {
  out = object$table[, .(gate, check, learner, role, status)]
  out[]
}

#' @rdname csdg_check
#' @export
plot.CSDGCheck = function(x, ...) {
  folds = rbindlist(lapply(names(x$details$stability), function(l) {
    st = x$details$stability[[l]]
    st$folds[, .(learner = l, iteration, d, class)]
  }))
  if (!nrow(folds)) .csdg_stop("The check has no fold values to plot (stability was not evaluated).")
  labels = x$claim$labels_learner %||% stats::setNames(unique(folds$learner), unique(folds$learner))
  folds[, learner_label__ := factor(unname(labels[learner]), levels = unique(unname(labels[learner])))]
  folds[, class := factor(class, levels = c("holds", "undecided", "fails"))]
  direction = identical(x$claim$kind, "direction")
  reference = if (direction) x$criteria$min_change else 0
  ylab = switch(x$claim$kind,
    direction = sprintf("Change in the claimed direction (%s)", x$claim$direction),
    order = sprintf("PFI of %s minus PFI of %s", x$claim$first, x$claim$second),
    relies_mainly = sprintf("Smallest named PFI minus %s x largest other PFI", format(x$claim$factor)),
    "Smallest named PFI minus largest other PFI"
  )
  colors = autoiml_palette()$metric
  ggplot(folds, aes(x = factor(iteration), y = d, color = class)) +
    ggplot2::geom_hline(yintercept = reference, linetype = "dashed", linewidth = 0.3) +
    geom_point(size = 2) +
    facet_wrap(~learner_label__) +
    ggplot2::scale_color_manual(values = c(holds = colors[["primary"]], undecided = "grey60",
      fails = colors[["secondary"]]), drop = FALSE, name = NULL) +
    labs(x = "Fold", y = ylab, title = sprintf("Stability (G5): at least %d of %d folds must hold", x$criteria$need,
      x$source$K))
}
