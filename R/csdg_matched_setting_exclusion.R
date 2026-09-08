.matched_setting_reference_vector = function(x) {
  predictions = .aggregate_repeated_predictions(x$predictions, x$positive)
  value = if (identical(x$task_type, "regr")) {
    as.numeric(predictions$response)
  } else {
    as.numeric(predictions[[.get_probability_column(predictions, x$positive)]])
  }
  stats::setNames(value, as.character(predictions$row_id))
}

#' Compare setting exclusion with training-size-matched removals
#'
#' For every observed setting, this function scores the supplied pooled out-of-fold reference predictions and
#' repeatedly refits the fixed learner after excluding that setting and sampling the remaining settings down to the
#' reference analysis-set size.
#' The comparison separates a training-size-matched setting-composition contrast from an unrestricted
#' leave-one-setting-out refit.
#'
#' @param x A `CSDGResample` containing pooled out-of-fold reference predictions and split sizes.
#' @param learner The fixed `mlr3` learner pipeline to refit.
#' @param group Setting identifiers aligned to `x$task$row_ids`, or a named vector indexed by row id.
#' @param loss Loss used for both reference and matched-exclusion predictions.
#' @param repetitions Number of independent matched training-row samples per setting.
#' @param seed Random seed.
#' @param store_models Whether to retain the fitted matched-exclusion models.
#'
#' @return A list with setting-by-repetition scores, setting summaries, optional fitted models, design metadata,
#'   and limitations.
#' @export
csdg_matched_setting_exclusion = function(
    x,
    learner,
    group,
    loss = NULL,
    repetitions = 5L,
    seed = 20260201L,
    store_models = FALSE) {
  assert_class(x, "CSDGResample")
  .require_learner(learner)
  assert_int(repetitions, lower = 2L)
  assert_int(seed, lower = 0L)
  assert_flag(store_models)
  task = x$task
  row_ids = task$row_ids
  group = .normalize_named_vector(group, row_ids, "group")
  if (anyNA(group)) {
    .csdg_stop("`group` must not contain missing values.")
  }
  group = as.character(group)
  groups = sort(unique(group))
  if (length(groups) < 2L) {
    .csdg_stop("At least two settings are required.")
  }
  loss = loss %||% if (identical(task$task_type, "regr")) "rmse" else "logloss"
  assert_string(loss, min.chars = 1L)
  reference_prediction = .matched_setting_reference_vector(x)
  if (!setequal(names(reference_prediction), as.character(row_ids))) {
    .csdg_stop("`x` must contain exactly one pooled out-of-fold prediction for every task row.")
  }
  reference_train_n = as.integer(round(stats::median(vapply(x$train_sets, length, integer(1L)))))
  model_store = if (store_models) vector("list", length(groups) * repetitions) else NULL
  rows = vector("list", length(groups) * repetitions)
  counter = 0L
  had_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) caller_seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      remove(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  for (group_index in seq_along(groups)) {
    held_out = groups[[group_index]]
    assessment = row_ids[group == held_out]
    candidates = row_ids[group != held_out]
    if (length(candidates) < reference_train_n) {
      .csdg_stop(
        "Setting `%s` leaves %d candidate analysis rows, fewer than the matched target of %d.",
        held_out,
        length(candidates),
        reference_train_n
      )
    }
    truth = .task_truth(task, assessment)
    reference = unname(reference_prediction[as.character(assessment)])
    reference_loss = .compute_loss(truth, reference, task$task_type, loss, positive = x$positive)
    for (replicate in seq_len(repetitions)) {
      counter = counter + 1L
      set.seed(as.integer(seed) + group_index * 10000L + replicate)
      analysis_rows = sample(candidates, reference_train_n, replace = FALSE)
      fit = learner$clone(deep = TRUE)
      fit$train(task, row_ids = analysis_rows)
      prediction = fit$predict(task, row_ids = assessment)
      prediction_value = .prediction_vector(prediction, task$task_type, x$positive)
      excluded_loss = .compute_loss(truth, prediction_value, task$task_type, loss, positive = x$positive)
      rows[[counter]] = data.table(
        held_out_group = held_out,
        matched_repetition = as.integer(replicate),
        n_reference_analysis = reference_train_n,
        n_candidate_other_setting = length(candidates),
        n_matched_exclusion_analysis = length(analysis_rows),
        n_assessment = length(assessment),
        loss = loss,
        reference_oof_loss = reference_loss,
        matched_exclusion_loss = excluded_loss,
        matched_exclusion_penalty = excluded_loss - reference_loss
      )
      if (store_models) model_store[[counter]] = fit
    }
  }
  per_repetition = rbindlist(rows)
  summary = per_repetition[, .(
    n_matched_repetitions = .N,
    n_assessment = unique(n_assessment),
    reference_oof_loss = unique(reference_oof_loss),
    mean_matched_exclusion_loss = mean(matched_exclusion_loss),
    standard_deviation_matched_exclusion_loss = sd(matched_exclusion_loss),
    minimum_matched_exclusion_loss = min(matched_exclusion_loss),
    maximum_matched_exclusion_loss = max(matched_exclusion_loss),
    mean_matched_exclusion_penalty = mean(matched_exclusion_penalty),
    standard_deviation_matched_exclusion_penalty = sd(matched_exclusion_penalty),
    minimum_matched_exclusion_penalty = min(matched_exclusion_penalty),
    maximum_matched_exclusion_penalty = max(matched_exclusion_penalty)
  ), by = .(held_out_group, loss)]
  list(
    per_repetition = per_repetition[],
    summary = summary[],
    models = model_store,
    design = list(
      assessment = "All pooled out-of-fold reference rows from the held-out observed setting.",
      matched_analysis_size = reference_train_n,
      composition = "Repeated simple random samples from all other observed settings.",
      repetitions = as.integer(repetitions),
      seed = as.integer(seed)
    ),
    limitations = c(
      "The comparison is descriptive and is restricted to the observed settings.",
      "Matched removals control analysis-set size but do not identify a causal setting effect.",
      "Out-of-fold reference predictions combine fold-specific fits, whereas each matched exclusion uses one refit.",
      "Generalization to an unobserved setting requires a separate prospective or external design."
    )
  )
}
