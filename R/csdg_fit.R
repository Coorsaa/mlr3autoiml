#' Fit learners and a model without predictors on common splits
#'
#' @description
#' First step of the analysis-first interface: fits every learner and a model without predictors (the featureless
#' learner of mlr3) on the same resampling splits with [mlr3::benchmark()] and stores the fold models.
#' The result is passed to [csdg_importance()], which computes held-out permutation feature importance (PFI) and the
#' improvement of each learner over the model without predictors, and to [csdg_effect()].
#'
#' Fitting runs through [mlr3::benchmark()], so parallelization with `future`, encapsulation and fallback learners,
#' and the methods of [mlr3::BenchmarkResult] (`$score()`, `$aggregate()`, `mlr3viz::autoplot()`) work as usual;
#' the benchmark result is stored as `bmr`.
#' An existing benchmark result with stored models can be passed instead of a task.
#' mlr3 log messages below warnings are suppressed while fitting.
#'
#' One measure is used throughout: for the fold scores, for PFI, and for the loss of each learner and of the model
#' without predictors, so that PFI and the improvement are in the same units.
#' PFI is computed for `classif.logloss` and `classif.bbrier` (binary classification) and for `regr.rmse`,
#' `regr.mse`, and `regr.mae`.
#'
#' The splits are instantiated once with `set.seed(seed)` (an instantiated resampling is used as given), and the
#' benchmark runs with the same seed.
#' With `seed = NULL`, a seed is drawn from the session's random number generator and stored in the result; with a
#' given seed, the random number generator state of the caller is left unchanged.
#'
#' @param task A [mlr3::TaskRegr] or a binary [mlr3::TaskClassif], or a [mlr3::BenchmarkResult] on one task with
#'   stored models (`store_models = TRUE`) and the same splits for all learners; a featureless learner in it is used
#'   as the model without predictors, otherwise one is fitted on the same splits.
#' @param learners A list of [mlr3::Learner] objects or one learner; unnamed elements are named by their learner id
#'   without the task type prefix (for example `"xgboost"`).
#'   Names must be unique, start with a letter, contain only letters, digits, `_`, and `.`, and must not be
#'   `"baseline"`.
#'   For classification, every learner must predict probabilities (`predict_type = "prob"`).
#' @param folds Number of cross-validation folds, used when `resampling` is `NULL`.
#' @param resampling Optional [mlr3::Resampling] (at least two iterations) or [csdg_grouped_resampling()] result
#'   (for clustered data); it overrides `folds`.
#' @param measure The loss: an [mlr3::Measure] or its id, with `minimize = TRUE`; defaults to `classif.logloss`
#'   and `regr.rmse`.
#' @param labels Optional named character vector mapping learner names to the labels used in all generated text, for
#'   example `c(xgboost = "XGBoost", ridge = "ridge logistic regression")`.
#'   By default, common learners are labeled by their method (`"XGBoost"`, `"random forest"`, `"ridge regression"`
#'   for `glmnet` with `alpha = 0`), and others as `"learner <name>"`.
#' @param seed Seed for the splits and the fits, or `NULL`.
#'
#' @return A `CSDGFits` list with `task`, `task_id`, `task_type`, `target`, `positive`, `features`, `n`, `labels`,
#'   `learner_ids`, `learners` (untrained copies), `resamples` (one fold-model summary per learner), `baseline` (the model without
#'   predictors), `bmr`, `resampling`, `resampling_label`, `K`, `n_train`, `n_test`, `measure`, `loss`, `seed`,
#'   `data_hash`, `row_hashes` (one hash per row of predictors and outcome, used by [csdg_confirm()]), and
#'   `created`.
#'   `print()` shows the mean fold scores, and `as.data.table()` returns the fold scores in long format.
#' @seealso [csdg_importance()], [csdg_effect()]
#' @examples
#' task = mlr3::as_task_regr(mtcars[, c("mpg", "wt", "hp", "qsec")], target = "mpg", id = "cars")
#' fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3, seed = 1)
#' fits
#' @export
csdg_fit = function(task, learners = NULL, folds = 10L, resampling = NULL, measure = NULL, labels = NULL,
                    seed = NULL) {
  if (inherits(task, "BenchmarkResult")) {
    return(.csdg_fit_from_bmr(task, measure = measure, labels = labels, seed = seed))
  }
  .require_task(task)
  .csdg_check_task_type(task)
  learners = .csdg_normalize_learners(learners, task)
  assert_int(folds, lower = 2L, .var.name = "folds")
  assert_int(seed, lower = 0L, upper = 2e9, null.ok = TRUE, .var.name = "seed")
  assert_true(
    is.null(resampling) || inherits(resampling, c("Resampling", "CSDGGroupedResampling")),
    .var.name = "resampling"
  )
  measure = .csdg_resolve_measure(measure, task$task_type)
  labels = .csdg_normalize_labels(labels, names(learners), "labels", defaults = .csdg_default_labels(learners))
  seed = as.integer(seed %||% sample.int(1e8L, 1L))
  random_seed = .csdg_diagnostic_random_seed()
  on.exit(.csdg_diagnostic_restore_random_seed(random_seed), add = TRUE)
  restore_log = .csdg_quiet_mlr3()
  on.exit(restore_log(), add = TRUE)

  grouped = inherits(resampling, "CSDGGroupedResampling")
  template = NULL
  if (grouped) {
    rs = resampling$resampling
  } else {
    rs = .clone_resampling(resampling %||% mlr3::rsmp("cv", folds = folds))
    if (!isTRUE(rs$is_instantiated)) {
      template = rs$clone(deep = TRUE)
      set.seed(seed)
      rs$instantiate(task)
    }
  }
  if (rs$iters < 2L) .csdg_stop("The resampling must have at least two iterations; it has %d.", rs$iters)
  featureless = .csdg_featureless(task)
  featureless$id = "baseline"
  # mlr3 requires unique learner ids within a benchmark; the names serve as ids there.
  bench = lapply(names(learners), function(nm) {
    learner = learners[[nm]]$clone(deep = TRUE)
    learner$id = nm
    learner
  })
  set.seed(seed)
  design = mlr3::benchmark_grid(task, c(bench, list(featureless)), rs)
  bmr = mlr3::benchmark(design, store_models = TRUE)
  rrs = bmr$resample_results$resample_result
  split_map = .csdg_split_map(rs, if (grouped) resampling$iteration_map)
  scores = .csdg_score_measures(measure, task)
  resamples = stats::setNames(lapply(seq_along(learners), function(i) {
    .csdg_rr_resample(rrs[[i]], task, scores, split_map, names(learners)[[i]])
  }), names(learners))
  baseline = .csdg_rr_resample(rrs[[length(rrs)]], task, scores, split_map, "baseline", models = FALSE)
  .csdg_new_fits(task, labels, learners, resamples, baseline, bmr, rs, template,
    .csdg_resampling_label(if (grouped) resampling else rs), measure, seed)
}

.csdg_check_task_type = function(task) {
  if (!task$task_type %in% c("regr", "classif")) .csdg_stop("Unsupported task type: %s.", task$task_type)
  if (identical(task$task_type, "classif") && length(task$class_names) != 2L) {
    .csdg_stop("CSDG classification diagnostics currently support binary classification only.")
  }
  invisible(TRUE)
}

# Suppresses mlr3 log messages below warnings; returns a function that restores the threshold.
.csdg_quiet_mlr3 = function() {
  if (!requireNamespace("lgr", quietly = TRUE)) return(function() invisible(NULL))
  logger = lgr::get_logger("mlr3")
  old = logger$threshold
  if (is.numeric(old) && old > 300) logger$set_threshold("warn")
  function() invisible(logger$set_threshold(old))
}

.csdg_featureless = function(task) {
  key = if (identical(task$task_type, "regr")) "regr.featureless" else "classif.featureless"
  mlr3::lrn(key, predict_type = if (identical(task$task_type, "classif")) "prob" else "response")
}

# The loss codes of csdg_fold_pfi() for the supported measures.
.csdg_measure_losses = c(classif.logloss = "logloss", classif.bbrier = "brier", regr.rmse = "rmse",
  regr.mse = "mse", regr.mae = "mae")

.csdg_resolve_measure = function(measure, task_type) {
  measure = measure %||% if (identical(task_type, "classif")) "classif.logloss" else "regr.rmse"
  if (is.character(measure)) {
    assert_string(measure, .var.name = "measure")
    measure = mlr3::msr(measure)
  }
  assert_class(measure, "Measure", .var.name = "measure")
  if (!isTRUE(measure$minimize)) .csdg_stop("`measure` must be a loss (minimize = TRUE); %s is not.", measure$id)
  if (!identical(measure$task_type, task_type) || !measure$id %in% names(.csdg_measure_losses)) {
    allowed = names(.csdg_measure_losses)[startsWith(names(.csdg_measure_losses), paste0(task_type, "."))]
    .csdg_stop("PFI is computed for the measures %s for this task type, not %s.",
      .csdg_and(allowed), measure$id)
  }
  list(measure = measure, loss = unname(.csdg_measure_losses[[measure$id]]))
}

.csdg_score_measures = function(measure, task) {
  defaults = .default_measures(task)
  ids = vapply(defaults, `[[`, character(1L), "id")
  c(list(measure$measure), defaults[ids != measure$measure$id])
}

.csdg_split_map = function(rs, iteration_map = NULL) {
  K = rs$iters
  if (!is.null(iteration_map)) {
    map = data.table::as.data.table(iteration_map)
    return(data.table(iteration = seq_len(K), repetition = as.integer(map$repetition[match(seq_len(K),
      map$iteration)]), fold = as.integer(map$fold[match(seq_len(K), map$iteration)])))
  }
  labels = lapply(seq_len(K), function(i) .resampling_repeat_fold(rs, i))
  data.table(iteration = seq_len(K), repetition = vapply(labels, `[[`, integer(1L), "repetition"),
    fold = vapply(labels, `[[`, integer(1L), "fold"))
}

# Fold-model summary of one mlr3 ResampleResult in the form used by csdg_fold_pfi() (a CSDGResample).
.csdg_rr_resample = function(rr, task, measures, split_map, name, models = TRUE) {
  rs = rr$resampling
  K = rs$iters
  errors = tryCatch(rr$errors, error = function(e) NULL)
  if (!is.null(errors) && nrow(errors)) {
    .csdg_warn("Learner `%s` raised errors in %d of %d folds; the fallback learner predicted there.", name,
      data.table::uniqueN(errors$iteration), K)
  }
  preds = rr$predictions()
  predictions = rbindlist(lapply(seq_len(K), function(i) {
    .extract_prediction_table(preds[[i]], iteration = i, repetition = split_map$repetition[[i]],
      fold = split_map$fold[[i]])
  }), fill = TRUE, use.names = TRUE)
  ids = vapply(measures, `[[`, character(1L), "id")
  score = data.table::as.data.table(rr$score(measures))
  fold_scores = cbind(split_map, score[order(score$iteration), ids, with = FALSE])
  learners = if (models) rr$learners else NULL
  if (models && any(vapply(learners, function(l) is.null(l$state), logical(1L)))) {
    .csdg_stop("Learner `%s` has no stored fold models; benchmark with store_models = TRUE.", name)
  }
  structure(
    list(
      task = task, task_id = task$id, task_type = task$task_type, target = task$target_names[[1L]],
      positive = .task_positive(task), learner = rr$learner, learner_id = rr$learner$id,
      predictions = predictions, fold_scores = fold_scores, models = learners, resampling = rs,
      train_sets = lapply(seq_len(K), rs$train_set), test_sets = lapply(seq_len(K), rs$test_set),
      measures = measures, split_metadata = NULL, seed = NA_integer_,
      uncertainty_scope = paste("Fold scores summarize resampling variation and are not treated as independent",
        "observations or as confidence intervals.")
    ),
    class = c("CSDGResample", "list")
  )
}

.csdg_new_fits = function(task, labels, learners, resamples, baseline, bmr, rs, template, resampling_label, measure,
                          seed) {
  first = resamples[[1L]]
  structure(
    list(
      task = task,
      task_id = task$id,
      task_type = task$task_type,
      target = task$target_names[[1L]],
      positive = .task_positive(task),
      features = task$feature_names,
      n = task$nrow,
      labels = labels,
      learner_ids = vapply(learners, `[[`, character(1L), "id"),
      learners = lapply(learners, function(l) {
        l = l$clone(deep = TRUE)
        l$reset()
        l
      }),
      resamples = resamples,
      baseline = baseline,
      bmr = bmr,
      resampling = rs,
      resampling_template = template,
      resampling_label = resampling_label,
      K = length(first$test_sets),
      n_train = lengths(first$train_sets),
      n_test = lengths(first$test_sets),
      measure = measure$measure,
      loss = measure$loss,
      seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
      data_hash = .hash_object(task$data()),
      row_hashes = .csdg_row_hashes(task),
      created = .now_utc()
    ),
    class = c("CSDGFits", "list")
  )
}

# One hash per row of predictors and outcome (columns in sorted order), so that overlapping rows between two data
# sets can be detected.
.csdg_row_hashes = function(task) {
  dat = task$data(cols = sort(c(task$target_names, task$feature_names)))
  text = do.call(paste, c(lapply(dat, function(x) {
    out = if (is.numeric(x)) sprintf("%.15g", x) else as.character(x)
    out[is.na(x)] = "<NA>"
    out
  }), sep = "\x1f"))
  digest::getVDigest(algo = "xxhash64")(text, serialize = FALSE)
}

.csdg_fit_from_bmr = function(bmr, measure, labels, seed) {
  tasks = bmr$tasks$task
  if (length(tasks) != 1L) .csdg_stop("The benchmark result must contain exactly one task.")
  task = tasks[[1L]]
  .csdg_check_task_type(task)
  assert_int(seed, lower = 0L, upper = 2e9, null.ok = TRUE, .var.name = "seed")
  measure = .csdg_resolve_measure(measure, task$task_type)
  rrs = bmr$resample_results$resample_result
  is_featureless = vapply(rrs, function(rr) inherits(rr$learner, c("LearnerRegrFeatureless",
    "LearnerClassifFeatureless")), logical(1L))
  rs = rrs[[1L]]$resampling
  if (rs$iters < 2L) .csdg_stop("The resampling must have at least two iterations; it has %d.", rs$iters)
  for (rr in rrs[-1L]) {
    same = rr$resampling$iters == rs$iters && all(vapply(seq_len(rs$iters), function(i) {
      setequal(rr$resampling$test_set(i), rs$test_set(i))
    }, logical(1L)))
    if (!same) .csdg_stop("All learners in the benchmark result must use the same splits.")
  }
  learner_rrs = rrs[!is_featureless]
  if (!length(learner_rrs)) .csdg_stop("The benchmark result contains no learner other than a featureless one.")
  learners = lapply(learner_rrs, function(rr) rr$learner)
  names(learners) = .csdg_learner_names(vapply(learners, `[[`, character(1L), "id"))
  learners = .csdg_normalize_learners(learners, task)
  labels = .csdg_normalize_labels(labels, names(learners), "labels", defaults = .csdg_default_labels(learners))
  split_map = .csdg_split_map(rs)
  scores = .csdg_score_measures(measure, task)
  resamples = stats::setNames(lapply(seq_along(learner_rrs), function(i) {
    .csdg_rr_resample(learner_rrs[[i]], task, scores, split_map, names(learners)[[i]])
  }), names(learners))
  baseline_rr = if (any(is_featureless)) rrs[[which(is_featureless)[[1L]]]] else {
    restore_log = .csdg_quiet_mlr3()
    on.exit(restore_log(), add = TRUE)
    mlr3::resample(task, .csdg_featureless(task), rs)
  }
  baseline = .csdg_rr_resample(baseline_rr, task, scores, split_map, "baseline", models = FALSE)
  .csdg_new_fits(task, labels, learners, resamples, baseline, bmr, rs, NULL, .csdg_resampling_label(rs), measure,
    seed)
}

.csdg_learner_names = function(ids) {
  out = sub("^(classif|regr)\\.", "", ids)
  out = gsub("[^A-Za-z0-9_.]", "_", out)
  out[!grepl("^[A-Za-z]", out)] = paste0("l_", out[!grepl("^[A-Za-z]", out)])
  make.unique(out, sep = "_")
}

.csdg_normalize_learners = function(learners, task) {
  if (is.null(learners)) .csdg_stop("`learners` must be a list of mlr3 learners.")
  if (inherits(learners, "Learner")) learners = list(learners)
  assert_list(learners, min.len = 1L, .var.name = "learners")
  if (!all(vapply(learners, inherits, logical(1L), what = "Learner"))) {
    .csdg_stop("`learners` must be a list of mlr3 learners.")
  }
  nms = names(learners) %||% rep("", length(learners))
  unnamed = is.na(nms) | !nzchar(nms)
  if (any(unnamed)) {
    nms[unnamed] = .csdg_learner_names(vapply(learners[unnamed], `[[`, character(1L), "id"))
    names(learners) = nms
  }
  if (anyDuplicated(nms)) .csdg_stop("Learner names must be unique; name the list elements.")
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_.]*$", nms))) {
    .csdg_stop("Learner names must start with a letter and contain only letters, digits, `_`, and `.`.")
  }
  if ("baseline" %in% nms) .csdg_stop("`baseline` is reserved for the model without predictors.")
  for (nm in nms) {
    learner = learners[[nm]]
    if (!identical(learner$task_type, task$task_type)) {
      .csdg_stop("Learner `%s` is for task type %s, not %s.", nm, learner$task_type, task$task_type)
    }
    if (identical(task$task_type, "classif") && !identical(learner$predict_type, "prob")) {
      .csdg_stop("Learner `%s` must predict probabilities; set predict_type = \"prob\".", nm)
    }
  }
  learners
}

# Method labels of common learners; others are labeled "learner <name>" so that identifiers are never capitalized.
.csdg_default_labels = function(learners) {
  out = vapply(names(learners), function(nm) {
    learner = learners[[nm]]
    base = sub("^(classif|regr)\\.", "", learner$id)
    classif = identical(learner$task_type, "classif")
    regression = if (classif) "logistic regression" else "regression"
    alpha = tryCatch(learner$param_set$values$alpha, error = function(e) NULL)
    switch(base,
      xgboost = "XGBoost",
      lightgbm = "LightGBM",
      ranger = "random forest",
      rpart = "decision tree",
      lm = "linear regression",
      log_reg = "logistic regression",
      glmnet = , cv_glmnet = paste(if (is.null(alpha) || isTRUE(all.equal(alpha, 1))) "lasso" else
        if (isTRUE(all.equal(alpha, 0))) "ridge" else "elastic net", regression),
      svm = "support vector machine",
      kknn = "k-nearest neighbors",
      nnet = "neural network",
      paste("learner", nm)
    )
  }, character(1L))
  dup = duplicated(out) | duplicated(out, fromLast = TRUE)
  out[dup] = sprintf("%s (%s)", out[dup], names(out)[dup])
  out
}

.csdg_normalize_labels = function(labels, names, var_name, defaults = NULL) {
  out = defaults %||% stats::setNames(paste("learner", names), names)
  out = stats::setNames(unname(out[names]), names)
  if (is.null(labels)) return(out)
  assert_character(labels, any.missing = FALSE, min.chars = 1L, names = "unique", .var.name = var_name)
  unknown = setdiff(names(labels), names)
  if (length(unknown)) .csdg_stop("Unknown names in `%s`: %s.", var_name, paste(unknown, collapse = ", "))
  out[names(labels)] = labels
  if (anyDuplicated(out)) .csdg_stop("The learner labels must be unique.")
  out
}

.csdg_resampling_label = function(rs) {
  if (inherits(rs, "CSDGGroupedResampling")) {
    if (rs$repeats > 1L) return(sprintf("%d repeats of grouped %d-fold cross-validation", rs$repeats, rs$folds))
    return(sprintf("grouped %d-fold cross-validation", rs$folds))
  }
  values = rs$param_set$values
  switch(rs$id,
    cv = sprintf("%d-fold cross-validation", values$folds),
    repeated_cv = sprintf("%d repeats of %d-fold cross-validation", values$repeats, values$folds),
    sprintf("%d resampling iterations (%s)", rs$iters, rs$id)
  )
}

#' @rdname csdg_fit
#' @param x A `CSDGFits` object.
#' @param ... Ignored.
#' @export
format.CSDGFits = function(x, ...) {
  learners = names(x$resamples)
  scores = function(resample) {
    cols = setdiff(names(resample$fold_scores), c("iteration", "repetition", "fold"))
    vapply(cols, function(col) mean(resample$fold_scores[[col]], na.rm = TRUE), numeric(1L))
  }
  rows = c(lapply(x$resamples, scores), list(baseline = scores(x$baseline)))
  measures = names(rows[[1L]])
  labels = c(unname(x$labels[learners]), "model without predictors")
  width = max(nchar(c("learner", labels)))
  header = paste0("  ", formatC("learner", width = -width), paste(formatC(measures, width = 17L), collapse = ""))
  body = vapply(seq_along(rows), function(i) {
    paste0("  ", formatC(labels[[i]], width = -width),
      paste(formatC(sprintf("%.3f", rows[[i]][measures]), width = 17L), collapse = ""))
  }, character(1L))
  outcome = if (identical(x$task_type, "classif")) {
    sprintf("outcome %s (positive class %s)", x$target, x$positive)
  } else {
    sprintf("outcome %s", x$target)
  }
  seed = if (is.na(x$seed)) "" else sprintf(" (seed %d)", x$seed)
  c(
    sprintf("<CSDGFits> %s: %s, %s rows, %d predictors, %s", x$task_id, .csdg_task_type_label(x$task_type),
      .csdg_count(x$n), length(x$features), outcome),
    sprintf("  %s; the same splits for all learners and the model without predictors%s", x$resampling_label, seed),
    sprintf("  Loss for PFI and improvement: %s (%s)", .csdg_loss_label(x$loss), x$measure$id),
    header, body
  )
}

#' @rdname csdg_fit
#' @export
print.CSDGFits = function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @rdname csdg_fit
#' @param keep.rownames Ignored.
#' @export
as.data.table.CSDGFits = function(x, keep.rownames = FALSE, ...) {
  long = function(resample, learner) {
    scores = as.data.table(resample$fold_scores)
    cols = setdiff(names(scores), c("iteration", "repetition", "fold"))
    out = melt(scores, id.vars = "iteration", measure.vars = cols, variable.name = "measure",
      value.name = "value", variable.factor = FALSE)
    out[, learner := learner]
    setcolorder(out, c("learner", "iteration", "measure", "value"))
    out[]
  }
  rbindlist(c(
    lapply(names(x$resamples), function(nm) long(x$resamples[[nm]], nm)),
    list(long(x$baseline, "baseline"))
  ))
}

# Learner names from names or labels (case-insensitive), in the order given.
.csdg_resolve_learner_names = function(fits, learners, var_name = "learner") {
  assert_character(learners, any.missing = FALSE, min.len = 1L, .var.name = var_name)
  known = names(fits$labels) %||% names(fits$resamples)
  out = vapply(learners, function(l) {
    if (l %in% known) return(l)
    hit = known[tolower(unname(fits$labels[known])) == tolower(l)]
    if (length(hit) == 1L) return(hit)
    .csdg_stop("Unknown learner `%s` in `%s`; use one of %s or its label.", l, var_name,
      .csdg_and(sprintf("\"%s\"", known)))
  }, character(1L), USE.NAMES = FALSE)
  if (anyDuplicated(out)) .csdg_stop("`%s` names a learner twice.", var_name)
  out
}
