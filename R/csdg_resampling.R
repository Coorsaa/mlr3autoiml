
#' @rdname csdg_resampling
#' @export
csdg_grouped_resampling = function(
    task,
    group,
    folds = 5L,
    repeats = 1L,
    strata = NULL,
    seed = 20260201L) {
  .require_task(task)
  checkmate::assert_int(folds, lower = 2)
  checkmate::assert_int(repeats, lower = 1)
  checkmate::assert_int(seed, lower = 0)
  row_ids = task$row_ids
  group = .normalize_named_vector(group, row_ids, "group")
  strata = .normalize_named_vector(strata, row_ids, "strata")
  if (anyNA(group)) .csdg_stop("`group` must not contain missing values.")

  row_map = data.table::data.table(
    row_id = row_ids,
    group = as.character(group),
    stratum = if (is.null(strata)) "__all__" else as.character(strata)
  )
  mixed_strata = row_map[!is.na(stratum), .(n_strata = data.table::uniqueN(stratum)), by = group][
    n_strata > 1L
  ]
  if (nrow(mixed_strata)) {
    .csdg_stop(
      "Each group must map to at most one nonmissing stratum; mixed groups: %s.",
      paste(mixed_strata$group, collapse = ", ")
    )
  }
  row_map[, stratum := {
    observed = unique(stratum[!is.na(stratum)])
    if (length(observed)) observed[[1L]] else "__missing__"
  }, by = group]

  group_map = row_map[, .(
    n = .N,
    stratum = unique(stratum)[[1L]]
  ), by = group]
  if (nrow(group_map) < folds) {
    .csdg_stop(
      "Grouped resampling needs at least as many unique groups (%d) as folds (%d).",
      nrow(group_map), folds
    )
  }

  assignments = vector("list", repeats)
  train_sets = vector("list", folds * repeats)
  test_sets = vector("list", folds * repeats)
  iteration_map = vector("list", folds * repeats)
  iter = 0L

  for (r in seq_len(repeats)) {
    set.seed(as.integer(seed) + r - 1L)
    gm = data.table::copy(group_map)
    gm[, tie := stats::runif(.N)]
    data.table::setorder(gm, stratum, -n, tie)
    gm[, fold := {
      load = rep(0, folds)
      ans = integer(.N)
      for (i in seq_len(.N)) {
        candidates = which(load == min(load))
        chosen = candidates[[sample.int(length(candidates), 1L)]]
        ans[[i]] = chosen
        load[[chosen]] = load[[chosen]] + n[[i]]
      }
      ans
    }, by = stratum]
    gm[, repetition := r]
    assignments[[r]] = gm[, .(repetition, fold, group, stratum, n)]

    for (f in seq_len(folds)) {
      iter = iter + 1L
      test_groups = gm[fold == f, group]
      test = row_map[group %in% test_groups, row_id]
      train = setdiff(row_ids, test)
      if (!length(test) || !length(train)) {
        .csdg_stop("An empty train or assessment set was generated.")
      }
      train_sets[[iter]] = train
      test_sets[[iter]] = test
      iteration_map[[iter]] = data.table::data.table(
        iteration = iter,
        repetition = r,
        fold = f,
        n_train = length(train),
        n_assessment = length(test)
      )
    }
  }

  rs = mlr3::rsmp("custom")
  rs$instantiate(task, train_sets = train_sets, test_sets = test_sets)
  structure(
    list(
      resampling = rs,
      assignments = data.table::rbindlist(assignments),
      iteration_map = data.table::rbindlist(iteration_map),
      train_sets = train_sets,
      test_sets = test_sets,
      row_map = row_map,
      folds = as.integer(folds),
      repeats = as.integer(repeats),
      seed = as.integer(seed)
    ),
    class = c("CSDGGroupedResampling", "list")
  )
}

#' @rdname csdg_resampling
#' @export
csdg_resample = function(
    task,
    learner,
    resampling = NULL,
    measures = NULL,
    store_models = TRUE,
    seed = 20260201L) {
  .require_task(task)
  .require_learner(learner)
  checkmate::assert_flag(store_models)
  checkmate::assert_int(seed, lower = 0)
  checkmate::assert_true(
    is.null(resampling) || inherits(resampling, c("Resampling", "CSDGGroupedResampling")),
    .var.name = "resampling"
  )
  if (identical(task$task_type, "classif") && length(task$class_names) != 2L) {
    .csdg_stop(
      "CSDG classification diagnostics currently support binary classification only."
    )
  }
  measures = .normalize_measures(measures, task)
  grouped_iteration_map = NULL
  if (inherits(resampling, "CSDGGroupedResampling")) {
    grouped_iteration_map = resampling$iteration_map
    resampling = resampling$resampling
  }
  if (is.null(resampling)) {
    resampling = mlr3::rsmp("cv", folds = 5L)
  }
  rs = .clone_resampling(resampling)
  if (!isTRUE(rs$is_instantiated)) rs$instantiate(task)

  n_iter = rs$iters
  models = if (store_models) vector("list", n_iter) else NULL
  predictions = vector("list", n_iter)
  scores = vector("list", n_iter)
  train_sets = vector("list", n_iter)
  test_sets = vector("list", n_iter)

  for (i in seq_len(n_iter)) {
    split_label = if (!is.null(grouped_iteration_map)) {
      c(
        repetition = grouped_iteration_map[iteration == i, repetition][[1L]],
        fold = grouped_iteration_map[iteration == i, fold][[1L]]
      )
    } else {
      .resampling_repeat_fold(rs, i)
    }
    set.seed(as.integer(seed) + i - 1L)
    fit = learner$clone(deep = TRUE)
    train = rs$train_set(i)
    test = rs$test_set(i)
    if (length(intersect(train, test))) {
      .csdg_stop("Resampling iteration %d has train/test overlap.", i)
    }
    train_sets[[i]] = train
    test_sets[[i]] = test
    fit$train(task, row_ids = train)
    pred = fit$predict(task, row_ids = test)
    pred_dt = .extract_prediction_table(
      pred,
      iteration = i,
      repetition = split_label[["repetition"]],
      fold = split_label[["fold"]]
    )
    predictions[[i]] = pred_dt

    score = pred$score(measures)
    score_dt = data.table::data.table(
      iteration = i,
      repetition = as.integer(split_label[["repetition"]]),
      fold = as.integer(split_label[["fold"]])
    )
    for (nm in names(score)) score_dt[[nm]] = unname(score[[nm]])
    scores[[i]] = score_dt
    if (store_models) models[[i]] = fit
  }

  pred_all = data.table::rbindlist(predictions, fill = TRUE, use.names = TRUE)
  score_all = data.table::rbindlist(scores, fill = TRUE, use.names = TRUE)
  structure(
    list(
      task = task,
      task_id = task$id,
      task_type = task$task_type,
      target = task$target_names[[1L]],
      positive = .task_positive(task),
      learner = learner,
      learner_id = learner$id,
      predictions = pred_all,
      fold_scores = score_all,
      models = models,
      resampling = rs,
      train_sets = train_sets,
      test_sets = test_sets,
      measures = measures,
      split_metadata = grouped_iteration_map,
      seed = as.integer(seed),
      uncertainty_scope = paste(
        "Fold scores summarize resampling variation and are not treated as",
        "independent observations or as confidence intervals."
      )
    ),
    class = c("CSDGResample", "list")
  )
}

#' @rdname csdg_resampling
#' @export
csdg_performance = function(x) {
  checkmate::assert_class(x, "CSDGResample")
  id_cols = c("iteration", "repetition", "fold")
  measure_cols = setdiff(names(x$fold_scores), id_cols)
  long = data.table::melt(
    x$fold_scores,
    id.vars = intersect(id_cols, names(x$fold_scores)),
    measure.vars = measure_cols,
    variable.name = "measure",
    value.name = "value"
  )
  summary = long[, .(
    n_iterations = sum(is.finite(value)),
    mean = mean(value, na.rm = TRUE),
    sd = stats::sd(value, na.rm = TRUE),
    median = stats::median(value, na.rm = TRUE),
    q10 = stats::quantile(value, 0.10, na.rm = TRUE, names = FALSE),
    q90 = stats::quantile(value, 0.90, na.rm = TRUE, names = FALSE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  ), by = measure]
  summary[, uncertainty_label :=
            "descriptive variation across resampling iterations; not a confidence interval"]
  list(per_iteration = long, summary = summary)
}

#' @rdname csdg_resampling
#' @export
csdg_oof_predictions = function(x, collapse_repeats = FALSE) {
  checkmate::assert_class(x, "CSDGResample")
  checkmate::assert_flag(collapse_repeats)
  if (!isTRUE(collapse_repeats)) return(data.table::copy(x$predictions))
  .aggregate_repeated_predictions(x$predictions, positive = x$positive)
}
