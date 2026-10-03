#' Held-out permutation feature importance of fitted learners
#'
#' @description
#' Second step of the analysis-first interface: computes held-out permutation feature importance (PFI) per fold and
#' learner from a [csdg_fit()] result, together with the held-out loss of each learner and of the model without
#' predictors, and the improvement over that model.
#' The loss is the measure of [csdg_fit()], so PFI and the improvement are in the same units.
#' PFI values are not parts of the improvement; PFI relative to the improvement can exceed 100%.
#'
#' Marginal PFI permutes each predictor in the held-out fold ([csdg_fold_pfi()], one call per learner, single
#' predictors first so that their values do not change when groups are added).
#' Grouped PFI (`groups`) permutes the members of a group jointly.
#' Conditional PFI permutes a predictor within strata, one call per conditioned predictor:
#' * `conditional`: a named list, predictor -> names of conditioning predictors, for example
#'   `list(worry_a = "worry_b", worry_b = "worry_a")`.
#'   The strata are formed from the values of the conditioning predictors on the whole task: their levels for
#'   factor, character, and logical predictors and for numeric predictors with at most 10 distinct values, deciles
#'   otherwise; several conditioning predictors are crossed, and missing values form their own stratum.
#' * `strata`: strata supplied by the analyst, either an atomic vector with one value per task row (every predictor
#'   is permuted within these strata, for example within country) or a named list, predictor -> such a vector.
#'
#' A warning is issued when the median stratum size in the held-out folds is below 5; in strata of one row, the
#' permutation changes nothing and conditional PFI is exactly 0.
#'
#' Passing a `CSDGImportance` result as `x` extends it: marginal PFI is reused, and only added groups and
#' conditioned predictors are computed, with the repetitions and seed of `x`; the result equals a fresh call with all
#' settings.
#'
#' @param x A [csdg_fit()] result, or a `CSDGImportance` result to extend; for the methods, a `CSDGImportance`
#'   object.
#' @param repetitions Number of permutations per predictor and fold (at least 2, so that Monte Carlo errors exist).
#' @param groups Optional named list of character vectors of predictors permuted jointly; names must differ from the
#'   predictor names.
#' @param conditional Optional named list, predictor -> conditioning predictors; see Description.
#' @param strata Optional strata supplied by the analyst; see Description.
#' @param batch_size Number of permuted copies predicted together, passed to [csdg_fold_pfi()].
#'   The permutations do not depend on batching; use 1 for learners whose prediction is stochastic.
#' @param seed Seed of the permutations; with `NULL`, a seed is drawn from the session's random number generator
#'   and stored in the result.
#'
#' @return A `CSDGImportance` list with `fits`, `learners`, the settings (`loss`, `measure`, `repetitions`,
#'   `batch_size`, `seed`, `groups`, `strata_features`, `strata_definition`, `conditional_on`), `pfi` and
#'   `conditional` ([csdg_fold_pfi()] results per learner), and the tables `performance` (held-out loss per learner
#'   and fold, with the model without predictors and the improvement), `fold_pfi` (PFI per learner, fold, and
#'   predictor or group, with `type` `"marginal"`, `"grouped"`, or `"conditional"`), `summary` (means over folds with
#'   PFI relative to the improvement, the rank, and the number of folds with positive PFI), and `improvement` (per
#'   learner).
#' @seealso [csdg_fit()], [claim_relies_mainly()], [csdg_check()]
#' @examples
#' set.seed(1)
#' n = 200
#' x = data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
#' x$y = 2 * x$a + x$b + rnorm(n)
#' task = mlr3::as_task_regr(x, target = "y", id = "toy")
#' fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3, seed = 1)
#' imp = csdg_importance(fits, repetitions = 5, seed = 2)
#' imp
#' as.data.table(imp, interval = TRUE)
#' @export
csdg_importance = function(x, repetitions = 20L, groups = NULL, conditional = NULL, strata = NULL,
                           batch_size = 20L, seed = NULL) {
  assert_multi_class(x, c("CSDGFits", "CSDGImportance"), .var.name = "x")
  if (inherits(x, "CSDGImportance")) {
    supplied = c(repetitions = !missing(repetitions), batch_size = !missing(batch_size), seed = !missing(seed))
    if (any(supplied)) {
      .csdg_stop(paste("When extending a result, %s %s taken from it; call csdg_importance(imp$fits, ...) to",
        "change %s."), .csdg_and(names(supplied)[supplied]), if (sum(supplied) == 1L) "is" else "are",
        if (sum(supplied) == 1L) "it" else "them")
    }
    return(.csdg_extend_importance(x, groups, conditional, strata))
  }
  fits = x
  .csdg_check_fits(fits)
  assert_int(repetitions, lower = 2L, .var.name = "repetitions")
  assert_int(batch_size, lower = 1L, .var.name = "batch_size")
  assert_int(seed, lower = 0L, upper = 2e9, null.ok = TRUE, .var.name = "seed")
  seed = as.integer(seed %||% sample.int(1e8L, 1L))
  groups = .csdg_resolve_groups(groups, fits$features)
  cond = .csdg_resolve_conditioning(conditional, strata, fits$task)
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  pfi = .csdg_marginal_pfi(fits, groups, repetitions, batch_size, seed)
  conditional_pfi = .csdg_conditional_pfi(fits, cond, repetitions, batch_size, seed)
  .csdg_new_importance(fits, loss = fits$loss, repetitions = repetitions, batch_size = batch_size, seed = seed,
    groups = groups, strata_features = names(cond$strata), strata_definition = cond$definition, pfi = pfi,
    conditional = conditional_pfi, losses = .csdg_all_fold_losses(fits), conditional_on = cond$conditioning,
    strata_values = cond$strata)
}

.csdg_check_fits = function(fits) {
  if (is.null(fits$baseline) || is.null(fits$baseline$predictions)) {
    .csdg_stop("`fits` has no model without predictors; refit with csdg_fit().")
  }
  if (any(vapply(fits$resamples, function(r) is.null(r$models), logical(1L)))) {
    .csdg_stop("`fits` has no stored fold models; refit with csdg_fit().")
  }
  invisible(TRUE)
}

.csdg_marginal_pfi = function(fits, groups, repetitions, batch_size, seed) {
  singles = stats::setNames(as.list(fits$features), fits$features)
  lapply(fits$resamples, function(resample) {
    csdg_fold_pfi(resample, feature_groups = c(singles, groups), loss = fits$loss, repetitions = repetitions,
      batch_size = batch_size, seed = seed)
  })
}

# Seed of the conditional PFI of predictor number m; csdg_fold_pfi() adds fold * 1e5 + group * 1000 + repetition,
# so predictors 1e7 apart never share a seed.
.csdg_conditional_seed = function(seed, m) {
  as.integer((as.numeric(seed) + 1e7 * m) %% 2e9)
}

.csdg_conditional_pfi = function(fits, cond, repetitions, batch_size, seed) {
  if (!length(cond$strata)) return(NULL)
  .csdg_warn_small_strata(cond, fits)
  lapply(fits$resamples, function(resample) {
    out = lapply(names(cond$strata), function(feature) {
      csdg_fold_pfi(resample, feature_groups = stats::setNames(list(feature), feature),
        strata = cond$strata[[feature]], loss = fits$loss, repetitions = repetitions, batch_size = batch_size,
        seed = .csdg_conditional_seed(seed, match(feature, fits$features)))
    })
    stats::setNames(out, names(cond$strata))
  })
}

.csdg_all_fold_losses = function(fits) {
  learners = names(fits$resamples)
  losses = rbindlist(lapply(learners, function(learner) {
    out = .csdg_fold_losses(fits$resamples[[learner]], fits$loss)
    out[, learner := learner]
    out
  }))
  baseline = .csdg_fold_losses(fits$baseline, fits$loss)
  setnames(baseline, "loss", "loss_baseline")
  losses = merge(losses, baseline, by = "iteration", sort = FALSE)
  counts = losses[, .N, by = learner]
  if (nrow(counts) != length(learners) || any(counts$N != fits$K) || anyNA(losses$loss_baseline)) {
    .csdg_stop("The held-out losses do not cover every fold of every learner; refit with csdg_fit().")
  }
  losses
}

.csdg_extend_importance = function(imp, groups, conditional, strata) {
  fits = imp$fits
  groups = .csdg_resolve_groups(groups, fits$features)
  clash = intersect(names(groups), names(imp$groups))
  for (g in clash) {
    if (!setequal(groups[[g]], imp$groups[[g]])) .csdg_stop("Group `%s` is already defined differently.", g)
  }
  all_groups = c(imp$groups, groups[setdiff(names(groups), names(imp$groups))])
  cond = .csdg_resolve_conditioning(conditional, strata, fits$task)
  known = intersect(names(cond$strata), imp$strata_features)
  for (feature in known) {
    if (!identical(cond$strata[[feature]], imp$strata_values[[feature]])) {
      .csdg_stop("Conditional PFI of `%s` was already computed with other strata.", feature)
    }
  }
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  pfi = if (length(all_groups) > length(imp$groups)) {
    .csdg_marginal_pfi(fits, all_groups, imp$repetitions, imp$batch_size, imp$seed)
  } else {
    imp$pfi
  }
  new_features = setdiff(names(cond$strata), imp$strata_features)
  added = list(strata = cond$strata[new_features], definition = cond$definition[new_features],
    conditioning = cond$conditioning[intersect(new_features, names(cond$conditioning))])
  new_pfi = .csdg_conditional_pfi(fits, added, imp$repetitions, imp$batch_size, imp$seed)
  conditional_pfi = imp$conditional
  for (l in names(new_pfi)) conditional_pfi[[l]] = c(conditional_pfi[[l]], new_pfi[[l]])
  strata_features = intersect(fits$features, c(imp$strata_features, new_features))
  definition = c(imp$strata_definition, added$definition)[strata_features]
  values = c(imp$strata_values, added$strata)[strata_features]
  if (!is.null(conditional_pfi)) conditional_pfi = lapply(conditional_pfi, function(z) z[strata_features])
  .csdg_new_importance(fits, loss = imp$loss, repetitions = imp$repetitions, batch_size = imp$batch_size,
    seed = imp$seed, groups = if (length(all_groups)) all_groups, strata_features = strata_features,
    strata_definition = definition, pfi = pfi, conditional = conditional_pfi, losses = .csdg_all_fold_losses(fits),
    conditional_on = c(imp$conditional_on, added$conditioning), strata_values = values)
}

.csdg_resolve_groups = function(groups, features) {
  if (is.null(groups) || !length(groups)) return(NULL)
  groups = .normalize_feature_groups(features, groups)
  clash = intersect(names(groups), features)
  if (length(clash)) .csdg_stop("Group names must differ from predictor names: %s.", paste(clash, collapse = ", "))
  groups
}

.csdg_fold_losses = function(resample, loss) {
  predictions = resample$predictions
  iterations = sort(unique(predictions$iteration))
  values = vapply(iterations, function(i) {
    rows = predictions[predictions$iteration == i]
    .compute_loss(rows$truth, .prediction_vector(rows, resample$task_type, resample$positive),
      resample$task_type, loss, positive = resample$positive)
  }, numeric(1L))
  data.table(iteration = as.integer(iterations), loss = values)
}

# Strata of conditional PFI from `conditional` (conditioning predictors) and `strata` (supplied strata).
.csdg_resolve_conditioning = function(conditional, strata, task) {
  features = task$feature_names
  row_ids = task$row_ids
  out = list()
  definition = character()
  conditioning = list()
  if (!is.null(conditional)) {
    assert_list(conditional, types = "character", min.len = 1L, names = "unique", .var.name = "conditional")
    unknown = setdiff(c(names(conditional), unlist(conditional)), features)
    if (length(unknown)) .csdg_stop("Unknown predictors in `conditional`: %s.", paste(unknown, collapse = ", "))
    for (feature in names(conditional)) {
      given = unique(conditional[[feature]])
      if (!length(given)) .csdg_stop("`conditional$%s` names no conditioning predictor.", feature)
      if (feature %in% given) .csdg_stop("A predictor cannot condition on itself: %s.", feature)
      built = .csdg_conditioning_strata(task, given)
      out[[feature]] = built$strata
      definition[[feature]] = built$definition
      conditioning[[feature]] = given
    }
  }
  if (!is.null(strata)) {
    if (is.atomic(strata)) {
      vec = unname(.normalize_named_vector(strata, row_ids, "strata"))
      supplied = stats::setNames(rep(list(vec), length(features)), features)
      supplied = supplied[setdiff(features, names(out))]
    } else {
      assert_list(strata, min.len = 1L, names = "unique", .var.name = "strata")
      unknown = setdiff(names(strata), features)
      if (length(unknown)) .csdg_stop("Unknown predictors in `strata`: %s.", paste(unknown, collapse = ", "))
      supplied = lapply(stats::setNames(names(strata), names(strata)), function(feature) {
        v = strata[[feature]]
        if (!is.atomic(v) || is.null(v)) .csdg_stop("Each element of `strata` must be an atomic vector.")
        if (length(v) != length(row_ids) && is.null(names(v))) {
          .csdg_stop(paste("`strata$%s` must have one value per task row; use `conditional` to name",
            "conditioning predictors."), feature)
        }
        unname(.normalize_named_vector(v, row_ids, sprintf("strata$%s", feature)))
      })
    }
    both = intersect(names(supplied), names(out))
    if (length(both) && !is.atomic(strata)) {
      .csdg_stop("Predictors in both `conditional` and `strata`: %s.", paste(both, collapse = ", "))
    }
    out[names(supplied)] = supplied
    definition[names(supplied)] = "the supplied strata"
  }
  keep = intersect(features, names(out))
  list(strata = out[keep], definition = definition[keep], conditioning = conditioning)
}

.csdg_conditioning_strata = function(task, conditioning) {
  dat = task$data(cols = conditioning)
  parts = lapply(conditioning, function(col) {
    x = dat[[col]]
    discrete = is.factor(x) || is.character(x) || is.logical(x) || data.table::uniqueN(x[!is.na(x)]) <= 10L
    if (discrete) {
      values = as.character(x)
      text = sprintf("the levels of %s", col)
    } else {
      breaks = unique(stats::quantile(x, seq(0, 1, by = 0.1), type = 7L, na.rm = TRUE, names = FALSE))
      values = if (length(breaks) < 2L) as.character(x) else as.character(cut(x, breaks, include.lowest = TRUE))
      text = sprintf("deciles of %s", col)
    }
    values[is.na(values)] = "__missing__"
    list(values = values, text = text)
  })
  strata = if (length(parts) == 1L) {
    parts[[1L]]$values
  } else {
    as.character(interaction(lapply(parts, `[[`, "values"), drop = TRUE))
  }
  list(strata = strata, definition = .csdg_and(vapply(parts, `[[`, character(1L), "text")))
}

.csdg_warn_small_strata = function(cond, fits) {
  row_ids = fits$task$row_ids
  test_sets = fits$resamples[[1L]]$test_sets
  medians = vapply(names(cond$strata), function(feature) {
    values = cond$strata[[feature]]
    sizes = unlist(lapply(test_sets, function(rows) as.integer(table(values[match(rows, row_ids)]))))
    if (length(sizes)) stats::median(sizes) else NA_real_
  }, numeric(1L))
  small = which(!is.na(medians) & medians < 5)
  if (length(small)) {
    .csdg_warn(paste(
      "Conditional PFI of %s: the median stratum size in the held-out folds is %s, below 5;",
      "permutation within such small strata changes few values (none in strata of one row)."
    ), .csdg_and(names(medians)[small]), if (data.table::uniqueN(medians[small]) == 1L) {
      format(medians[small][[1L]])
    } else {
      .csdg_and(format(medians[small]))
    })
  }
  invisible(NULL)
}

# Builds a CSDGImportance object from csdg_fold_pfi() results and fold losses (also used by the test fixture).
.csdg_new_importance = function(fits, loss, repetitions, batch_size, seed, groups, strata_features, strata_definition,
                                pfi, conditional, losses, conditional_on = list(), strata_values = list()) {
  learners = names(pfi)
  features = fits$features
  fold_of = unique(as.data.table(pfi[[1L]]$per_iteration)[, .(iteration, fold)])
  tables = .csdg_performance_tables(fits, losses, learners, fold_of)
  performance = tables$performance
  improvement = tables$improvement

  fold_rows = function(result, learner, type_fun) {
    tab = as.data.table(result$per_iteration)[, .(iteration, fold, feature_group, features, importance,
      monte_carlo_se, n_permutations)]
    tab[, `:=`(learner = learner, type = type_fun(feature_group, features))]
    tab
  }
  marginal_type = function(group, members) fifelse(group %in% features & members == group, "marginal", "grouped")
  fold_pfi = rbindlist(c(
    lapply(learners, function(l) fold_rows(pfi[[l]], l, marginal_type)),
    unlist(lapply(learners, function(l) {
      lapply(conditional[[l]], function(result) fold_rows(result, l, function(group, members) "conditional"))
    }), recursive = FALSE)
  ))
  # Ties count against a predictor: with PFI exactly 0, several predictors are not all "in the top k".
  fold_pfi[, rank_in_fold := NA_integer_]
  fold_pfi[type == "marginal", rank_in_fold := as.integer(frank(-importance, ties.method = "max")),
    by = .(learner, iteration)]
  fold_pfi[, `:=`(learner_order__ = match(learner, learners),
    type_order__ = match(type, c("marginal", "grouped", "conditional")))]
  setorder(fold_pfi, learner_order__, type_order__, iteration, rank_in_fold)
  setcolorder(fold_pfi, c("learner", "iteration", "fold", "feature_group", "features", "type", "importance",
    "monte_carlo_se", "n_permutations", "rank_in_fold"))

  summary = fold_pfi[, .(
    features = features[[1L]],
    mean_importance = mean(importance),
    sd_importance = stats::sd(importance),
    positive_folds = sum(importance > 0),
    learner_order__ = learner_order__[[1L]],
    type_order__ = type_order__[[1L]]
  ), by = .(learner, feature_group, type)]
  summary[, relative_to_improvement := mean_importance /
    improvement$improvement[match(learner, improvement$learner)]]
  summary[, rank := NA_integer_]
  summary[type == "marginal", rank := as.integer(frank(-mean_importance, ties.method = "min")), by = learner]
  setorder(summary, learner_order__, type_order__, -mean_importance)
  summary[, c("learner_order__", "type_order__") := NULL]
  fold_pfi[, c("learner_order__", "type_order__") := NULL]
  setcolorder(summary, c("learner", "feature_group", "features", "type", "mean_importance", "sd_importance",
    "relative_to_improvement", "rank", "positive_folds"))

  structure(
    list(
      fits = fits,
      learners = learners,
      loss = loss,
      measure = if (is.null(fits$measure)) NA_character_ else fits$measure$id,
      repetitions = as.integer(repetitions),
      batch_size = as.integer(batch_size),
      seed = as.integer(seed),
      groups = groups,
      strata_features = strata_features %||% character(),
      strata_definition = strata_definition %||% character(),
      conditional_on = conditional_on %||% list(),
      strata_values = strata_values %||% list(),
      pfi = pfi,
      conditional = conditional,
      performance = performance[],
      fold_pfi = fold_pfi[],
      summary = summary[],
      improvement = improvement[]
    ),
    class = c("CSDGImportance", "list")
  )
}

# Held-out loss per learner and fold with the model without predictors, and the improvement per learner.
.csdg_performance_tables = function(fits, losses, learners, fold_of) {
  performance = merge(as.data.table(losses), fold_of, by = "iteration", all.x = TRUE, sort = FALSE)
  performance[, `:=`(
    n_train = as.integer(fits$n_train[iteration]),
    n_test = as.integer(fits$n_test[iteration]),
    improvement = loss_baseline - loss
  )]
  performance[, learner_order__ := match(learner, learners)]
  setorder(performance, learner_order__, iteration)
  performance[, learner_order__ := NULL]
  setcolorder(performance, c("learner", "iteration", "fold", "n_train", "n_test", "loss", "loss_baseline",
    "improvement"))
  improvement = performance[, .(
    loss = mean(loss),
    loss_baseline = mean(loss_baseline),
    improvement = mean(improvement),
    folds_improved = sum(improvement > 0),
    K = .N
  ), by = learner]
  improvement[, relative_improvement := improvement / loss_baseline]
  setcolorder(improvement, c("learner", "loss", "loss_baseline", "improvement", "relative_improvement",
    "folds_improved", "K"))
  list(performance = performance[], improvement = improvement[])
}

# K x R matrix of permutation values of one predictor or group (rows ordered by iteration).
.csdg_raw = function(imp, learner, group, type = "marginal") {
  result = if (identical(type, "conditional")) imp$conditional[[learner]][[group]] else imp$pfi[[learner]]
  raw = as.data.table(result$raw)
  rows = raw[raw$feature_group == group]
  rows = rows[order(rows$iteration, rows$permutation_repetition)]
  iterations = sort(unique(rows$iteration))
  matrix(rows$importance, nrow = length(iterations), byrow = TRUE, dimnames = list(iterations, NULL))
}

# K x p matrix of fold values of one type (columns: predictors or groups).
.csdg_fold_matrix = function(imp, learner, type = "marginal") {
  rows = which(imp$fold_pfi$learner == learner & imp$fold_pfi$type == type)
  tab = imp$fold_pfi[rows]
  if (!nrow(tab)) return(NULL)
  wide = data.table::dcast(tab, iteration ~ feature_group, value.var = "importance")
  setorder(wide, iteration)
  out = as.matrix(wide[, -1L, with = FALSE])
  rownames(out) = wide$iteration
  if (identical(type, "marginal")) out = out[, intersect(imp$fits$features, colnames(out)), drop = FALSE]
  out
}

.csdg_improvement = function(imp, learner) {
  imp$improvement$improvement[match(learner, imp$improvement$learner)]
}

.csdg_learners = function(imp) imp$learners %||% names(imp$pfi)

.csdg_conditional_text = function(imp, feature) {
  given = imp$conditional_on[[feature]]
  if (length(given)) sprintf("once %s %s known", .csdg_and(given), if (length(given) == 1L) "is" else "are") else
    sprintf("within %s", imp$strata_definition[[feature]])
}

#' @rdname csdg_importance
#' @param k For `print()`: the column "folds in top k" counts the folds in which a predictor has one of the `k`
#'   largest PFI values (at most the number of predictors minus 1).
#' @param n For `print()` and `plot()`: number of predictors shown per learner.
#' @param learner For `print()` and `plot()`: names of the learners shown; defaults to all.
#' @param ... Ignored.
#' @export
format.CSDGImportance = function(x, k = 2L, n = 6L, learner = NULL, ...) {
  learners = .csdg_resolve_learner_names(x$fits, learner %||% .csdg_learners(x))
  K = x$fits$K
  p = length(x$fits$features)
  k = max(1L, min(as.integer(k), p - 1L))
  loss_label = .csdg_loss_label(x$loss)
  lines = sprintf("<CSDGImportance> held-out PFI, increase in %s; %d folds x %d permutations", loss_label, K,
    x$repetitions)
  for (l in learners) {
    imp_row = x$improvement[x$improvement$learner == l]
    lines = c(lines, sprintf("%s: held-out %s %s versus %s for the model without predictors (improvement %s)",
      .csdg_learner_label(x$fits, l), loss_label, .csdg_num(imp_row$loss), .csdg_num(imp_row$loss_baseline),
      .csdg_num(imp_row$improvement)))
    marginal = x$summary[x$summary$learner == l & x$summary$type == "marginal"][order(rank)]
    top = utils::head(marginal, n)
    in_top = x$fold_pfi[x$fold_pfi$learner == l & x$fold_pfi$type == "marginal" & x$fold_pfi$rank_in_fold <= k,
      .N, by = feature_group]
    folds_top = in_top$N[match(top$feature_group, in_top$feature_group)]
    folds_top[is.na(folds_top)] = 0L
    width = max(nchar(c("predictor", top$feature_group)))
    lines = c(lines,
      sprintf("  %s   mean PFI   relative to improvement   folds in top %d", formatC("predictor", width = -width),
        k),
      sprintf("  %s %10s %25s %16s", formatC(top$feature_group, width = -width), .csdg_num(top$mean_importance),
        .csdg_pct(top$relative_to_improvement), sprintf("%d/%d", folds_top, K)))
    if (nrow(marginal) > n) lines = c(lines, sprintf("  ... %d more predictors", nrow(marginal) - n))
    grouped = x$summary[x$summary$learner == l & x$summary$type == "grouped"]
    for (i in seq_len(nrow(grouped))) {
      lines = c(lines, sprintf("  grouped: %s (%s) %s (%s)", grouped$feature_group[[i]],
        paste(strsplit(grouped$features[[i]], "|", fixed = TRUE)[[1L]], collapse = ", "),
        .csdg_num(grouped$mean_importance[[i]]), .csdg_pct(grouped$relative_to_improvement[[i]])))
    }
    conditional = x$summary[x$summary$learner == l & x$summary$type == "conditional"]
    for (i in seq_len(nrow(conditional))) {
      feature = conditional$feature_group[[i]]
      lines = c(lines, sprintf("  conditional: %s, %s: %s (%s)", feature, .csdg_conditional_text(x, feature),
        .csdg_num(conditional$mean_importance[[i]]), .csdg_pct(conditional$relative_to_improvement[[i]])))
    }
  }
  c(lines,
    if (k == 1L) "Folds in top 1: folds in which the predictor has the largest PFI value (print(imp, k = ))." else
      sprintf("Folds in top %d: folds in which the predictor has one of the %d largest PFI values (print(imp, k = )).",
        k, k),
    "PFI values are not parts of the improvement, so PFI relative to the improvement can exceed 100%.")
}

#' @rdname csdg_importance
#' @export
print.CSDGImportance = function(x, k = 2L, n = 6L, learner = NULL, ...) {
  cat(format(x, k = k, n = n, learner = learner, ...), sep = "\n")
  invisible(x)
}

#' @rdname csdg_importance
#' @param keep.rownames Ignored.
#' @param level For `as.data.table()`: `"summary"` (one row per learner, predictor or group, and type), `"fold"`
#'   (one row per fold), `"performance"` (held-out losses per fold), or `"improvement"` (per learner).
#' @param interval For `as.data.table()` with `level = "summary"`: if `TRUE`, adds the corrected resampled 95%
#'   interval of each mean PFI ([csdg_learner_pfi_interval()]), reported descriptively.
#' @export
as.data.table.CSDGImportance = function(x, keep.rownames = FALSE, ...,
                                        level = c("summary", "fold", "performance", "improvement"),
                                        interval = FALSE) {
  level = match.arg(level)
  assert_flag(interval, .var.name = "interval")
  out = copy(switch(level, summary = x$summary, fold = x$fold_pfi, performance = x$performance,
    improvement = x$improvement))
  if (interval && identical(level, "summary")) {
    fits = x$fits
    bounds = x$fold_pfi[, {
      ci = csdg_learner_pfi_interval(importance[order(iteration)], n_train = fits$n_train, n_test = fits$n_test)
      list(lower = ci$lower, upper = ci$upper)
    }, by = .(learner, feature_group, type)]
    out = merge(out, bounds, by = c("learner", "feature_group", "type"), sort = FALSE)
    setcolorder(out, c("learner", "feature_group", "features", "type", "mean_importance", "lower", "upper"))
  }
  out[]
}

#' @rdname csdg_importance
#' @param object A `CSDGImportance` object.
#' @export
summary.CSDGImportance = function(object, ...) {
  as.data.table(object, interval = TRUE)
}

#' @rdname csdg_importance
#' @param features For `plot()`: optional predictors (or group names) to show instead of the `n` leading predictors
#'   of each learner, for example two correlated items with small PFI.
#' @param scale For `plot()`: `"loss"` (PFI in loss units) or `"relative"` (PFI relative to the improvement).
#' @param min_importance For `plot()`: the dashed line marks this share of each learner's improvement (default 1%,
#'   the default minimum importance of [csdg_check()]).
#' @export
plot.CSDGImportance = function(x, learner = NULL, n = 10L, features = NULL, scale = c("loss", "relative"),
                               min_importance = 0.01, ...) {
  scale = match.arg(scale)
  learners = .csdg_resolve_learner_names(x$fits, learner %||% .csdg_learners(x))
  assert_int(n, lower = 1L, .var.name = "n")
  assert_number(min_importance, lower = 0, upper = 1, .var.name = "min_importance")
  units = unique(x$summary$feature_group)
  if (!is.null(features)) assert_subset(features, units, .var.name = "features")
  labels = x$fits$labels
  facet_levels = unname(labels[learners])
  shown = rbindlist(lapply(learners, function(l) {
    rows = x$summary[x$summary$learner == l]
    marginal = rows[rows$type == "marginal"][order(-mean_importance)]
    keep = features %||% c(utils::head(marginal$feature_group, n), rows$feature_group[rows$type == "grouped"])
    rows = rows[rows$feature_group %in% keep]
    order_value = rows[, .(value = mean_importance[type %in% c("marginal", "grouped")][1L]), by = feature_group]
    order_value = order_value[order(value)]
    rows[, key__ := factor(paste(feature_group, l, sep = "___"), levels = paste(order_value$feature_group, l,
      sep = "___"))]
    rows
  }))
  shown[, value__ := if (scale == "relative") relative_to_improvement else mean_importance]
  folds = merge(x$fold_pfi[x$fold_pfi$learner %in% learners], shown[, .(learner, feature_group, type, key__)],
    by = c("learner", "feature_group", "type"))
  folds[, value__ := if (scale == "relative") {
    importance / x$improvement$improvement[match(learner, x$improvement$learner)]
  } else importance]
  thresholds = data.table(learner = learners, value__ = if (scale == "relative") min_importance else {
    min_importance * x$improvement$improvement[match(learners, x$improvement$learner)]
  })
  thresholds[, label__ := sprintf("%s of improvement", .csdg_pct_value(min_importance))]
  shown[, learner_label__ := factor(unname(labels[learner]), levels = facet_levels)]
  folds[, learner_label__ := factor(unname(labels[learner]), levels = facet_levels)]
  thresholds[, learner_label__ := factor(unname(labels[learner]), levels = facet_levels)]
  shown[, type := factor(type, levels = c("marginal", "grouped", "conditional"))]
  folds[, type := factor(type, levels = c("marginal", "grouped", "conditional"))]
  colors = autoiml_palette()$metric
  fills = c(marginal = colors[["primary"]], grouped = colors[["tertiary"]], conditional = colors[["secondary"]])
  xlab = if (scale == "relative") {
    "PFI relative to the improvement over the model without predictors"
  } else {
    sprintf("Held-out PFI (increase in %s)", .csdg_loss_label(x$loss))
  }
  dodge = ggplot2::position_dodge(width = 0.8)
  legend = if (data.table::uniqueN(shown$type) > 1L) "right" else "none"
  ggplot(shown, aes(x = value__, y = key__, fill = type)) +
    geom_col(position = dodge, alpha = 0.7, width = 0.75) +
    geom_point(data = folds, aes(group = type), position = dodge, size = 0.8, alpha = 0.5,
      color = "grey25", show.legend = FALSE) +
    geom_vline(xintercept = 0, linewidth = 0.3) +
    geom_vline(data = thresholds, aes(xintercept = value__), linetype = "dashed", linewidth = 0.3) +
    geom_text(data = thresholds, aes(x = value__, y = Inf, label = label__), inherit.aes = FALSE, hjust = -0.05,
      vjust = 1.3, size = 2.8, color = "grey30") +
    facet_wrap(~learner_label__, scales = "free") +
    ggplot2::scale_y_discrete(labels = function(v) sub("___.*$", "", v)) +
    ggplot2::scale_fill_manual(values = fills, drop = TRUE, name = NULL) +
    ggplot2::theme(legend.position = legend) +
    labs(x = xlab, y = NULL)
}
