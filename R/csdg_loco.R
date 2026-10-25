#' Leave-one-covariate-out importance of fitted learners
#'
#' @description
#' Computes leave-one-covariate-out (LOCO) importance from a [csdg_fit()] result (Lei et al., 2018): in each fold,
#' every learner is refitted on the training set without a predictor, or without a group of predictors, and LOCO is
#' the increase in held-out loss over the fit with all predictors.
#' The refits use the splits, the learners with their settings, and the loss of [csdg_fit()]; they run through
#' [mlr3::benchmark()] on the stored splits, so that parallelization and encapsulation work as in [csdg_fit()], and
#' the seed of [csdg_fit()] is set before them.
#'
#' LOCO measures how much worse the learner predicts when it is trained without the predictor: the information about
#' the outcome that the predictor carries beyond the other predictors, for this learner and training size.
#' Unlike permutation feature importance (PFI), it creates no unrealistic inputs; a predictor that other predictors
#' can replace has LOCO near zero even if the fitted model uses it.
#'
#' The result reports the fold values and, per learner and predictor or group, their mean with the corrected
#' resampled t interval ([csdg_learner_pfi_interval()]).
#' It has the structure of a [csdg_importance()] result with one value per fold, so that it serves as evidence
#' directly: [claim_relies_mainly()], [claim_top_k()], and [claim_order()] derive claims from it, whose wording begins
#' with "In refits without each predictor (LOCO),", and [csdg_check()], [csdg_assess()], and [csdg_confirm()] check
#' and test them with the same criteria as claims about PFI.
#' LOCO is computed once per fold, so the Monte Carlo rule does not apply; for learners with random elements (for
#' example subsampling or internal tuning), the randomness of the refits enters the fold values and thus the
#' corrected interval.
#' The cost is one refit per learner, fold, and predictor or group.
#'
#' @param fits A [csdg_fit()] result.
#' @param predictors Predictors whose LOCO is computed; defaults to all.
#'   Claims that rank predictors ([claim_relies_mainly()], [claim_top_k()]) need all of them.
#' @param groups Optional named list of character vectors of predictors that are removed jointly; names must differ
#'   from the predictor names.
#'
#' @return A `CSDGLoco` object, which is also a `CSDGImportance`: a list with `fits`, `learners`, `loss`, `measure`,
#'   `quantity` (`"loco"`), `predictors`, `groups`, `repetitions` (1), `pfi` (per learner, the fold values in the
#'   form of [csdg_fold_pfi()] results), and the tables `performance` (held-out loss of the fits with all
#'   predictors and of the model without predictors), `fold_pfi` (LOCO per learner, fold, and predictor or group,
#'   with `type` `"loco"` or `"grouped"`), `summary` (means over folds), `improvement` (per learner), and
#'   `refit_losses` (per learner, fold, and predictor or group: the held-out loss with all predictors, `loss_full`,
#'   and after the refit, `loss_reduced`).
#'   `print()` shows the mean LOCO with its corrected 95% interval; `plot()`, `summary()`, and `as.data.table()` work
#'   as for [csdg_importance()].
#' @references
#' Lei, J., G'Sell, M., Rinaldo, A., Tibshirani, R. J., and Wasserman, L. (2018). Distribution-free predictive
#' inference for regression. Journal of the American Statistical Association, 113(523), 1094-1111.
#' @seealso [csdg_importance()], [claim_order()], [csdg_check()]
#' @examples
#' set.seed(1)
#' n = 200
#' x = data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
#' x$y = 2 * x$a + x$b + rnorm(n)
#' task = mlr3::as_task_regr(x, target = "y", id = "toy")
#' fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3, seed = 1)
#' loco = csdg_loco(fits)
#' loco
#' claim = claim_order(loco, "a", "b")
#' g0b = csdg_judge("G0b", supported(), note = "Simulated variables, measured without error.")
#' csdg_assess(csdg_check(claim, loco, judgments = g0b))
#' @export
csdg_loco = function(fits, predictors = NULL, groups = NULL) {
  assert_class(fits, "CSDGFits", .var.name = "fits")
  .csdg_check_fits(fits)
  if (is.null(fits$task)) .csdg_stop("`fits` holds no task; refit with csdg_fit().")
  features = fits$features
  if (length(features) < 2L) .csdg_stop("LOCO needs at least two predictors.")
  predictors = predictors %||% features
  assert_character(predictors, any.missing = FALSE, min.len = 1L, unique = TRUE, .var.name = "predictors")
  unknown = setdiff(predictors, features)
  if (length(unknown)) .csdg_stop("Unknown predictors: %s.", paste(unknown, collapse = ", "))
  predictors = intersect(features, predictors)
  groups = .csdg_resolve_groups(groups, features)
  complete = names(groups)[vapply(groups, function(g) setequal(g, features), logical(1L))]
  if (length(complete)) {
    .csdg_stop("A group cannot contain every predictor (%s); the model without predictors covers that case.",
      paste(complete, collapse = ", "))
  }
  units = c(stats::setNames(as.list(predictors), predictors), groups)
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  restore_log = .csdg_quiet_mlr3()
  on.exit(restore_log(), add = TRUE)

  task = fits$task
  first = fits$resamples[[1L]]
  learner_names = names(fits$resamples)
  design = rbindlist(lapply(names(units), function(unit) {
    reduced = task$clone(deep = TRUE)
    reduced$select(setdiff(features, units[[unit]]))
    reduced$id = paste0(task$id, "_without_", unit)
    splits = mlr3::rsmp("custom")
    splits$instantiate(reduced, train_sets = first$train_sets, test_sets = first$test_sets)
    data.table(
      task = list(reduced),
      learner = lapply(learner_names, function(l) {
        learner = fits$learners[[l]]$clone(deep = TRUE)
        learner$id = l
        learner
      }),
      resampling = list(splits),
      unit = unit,
      learner_name = learner_names
    )
  }))
  set.seed(if (is.na(fits$seed)) 1L else fits$seed)
  bmr = mlr3::benchmark(design[, c("task", "learner", "resampling"), with = FALSE], store_models = FALSE)
  rrs = bmr$resample_results$resample_result
  split_map = as.data.table(first$fold_scores)[, c("iteration", "repetition", "fold"), with = FALSE]
  full_losses = lapply(stats::setNames(learner_names, learner_names), function(l) {
    .csdg_fold_losses(fits$resamples[[l]], fits$loss)
  })
  refit = rbindlist(lapply(seq_along(rrs), function(i) {
    rr = rrs[[i]]
    unit = design$unit[[i]]
    l = design$learner_name[[i]]
    if (!identical(rr$learner$id, l) || !identical(rr$task$id, design$task[[i]]$id)) {
      .csdg_stop("The refits do not match the design; this is a bug in csdg_loco().")
    }
    resample = .csdg_rr_resample(rr, task, list(fits$measure), split_map, sprintf("%s without %s", l, unit),
      models = FALSE)
    reduced = .csdg_fold_losses(resample, fits$loss)
    full = full_losses[[l]]
    out = merge(full, reduced, by = "iteration", suffixes = c("_full", "_reduced"))
    out[, `:=`(learner = l, feature_group = unit, features = paste(units[[unit]], collapse = "|"),
      importance = loss_reduced - loss_full)]
    out
  }))
  refit = merge(refit, split_map, by = "iteration", sort = FALSE)
  refit[, n_assessment := lengths(first$test_sets)[iteration]]
  setorder(refit, iteration)
  pfi = lapply(stats::setNames(learner_names, learner_names), function(l) {
    rows = refit[refit$learner == l]
    raw = rows[, list(iteration, repetition, fold, feature_group, features, permutation_repetition = 1L,
      n_assessment, loss = fits$loss, baseline_loss = loss_full, permuted_loss = loss_reduced, importance)]
    per_iteration = raw[, list(importance, monte_carlo_sd = NA_real_, monte_carlo_se = NA_real_,
      n_permutations = 1L, baseline_loss, permuted_loss, n_assessment), by = .(iteration, repetition, fold,
      feature_group, features, loss)]
    list(raw = raw, per_iteration = per_iteration)
  })
  out = .csdg_new_importance(fits, loss = fits$loss, repetitions = 1L, batch_size = NA_integer_, seed = fits$seed,
    groups = groups, strata_features = character(), strata_definition = character(), pfi = pfi, conditional = NULL,
    losses = .csdg_all_fold_losses(fits), quantity = "loco")
  out$predictors = predictors
  out$refit_losses = refit[, c("learner", "iteration", "fold", "feature_group", "features", "loss_full",
    "loss_reduced", "importance"), with = FALSE][order(match(learner, learner_names), match(feature_group,
    names(units)), iteration)]
  class(out) = c("CSDGLoco", class(out))
  out
}

# The predictors a claim needs from a LOCO result: the pair of an order claim, every predictor otherwise.
.csdg_require_loco_units = function(imp, claim) {
  needed = if (identical(claim$kind, "order")) c(claim$first, claim$second) else imp$fits$features
  missing = setdiff(needed, imp$predictors)
  if (length(missing)) {
    .csdg_stop("The LOCO result has no values for %s; compute them with csdg_loco(fits, predictors = ...).",
      .csdg_and(missing))
  }
  invisible(TRUE)
}
