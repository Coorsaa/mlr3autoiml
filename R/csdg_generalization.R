
#' @rdname csdg_generalization
#' @export
csdg_rashomon = function(
    task,
    learners,
    resampling,
    primary_measure = NULL,
    tolerance_absolute = NULL,
    tolerance_relative = NULL,
    tolerance_source = NULL,
    tolerance_rationale = NULL,
    reference_learner = NULL,
    seed = 20260201L,
    store_models = TRUE) {
  .require_task(task)
  checkmate::assert_int(seed, lower = 0)
  checkmate::assert_flag(store_models)
  if (inherits(learners, "Learner")) learners = list(learners)
  checkmate::assert_list(learners, min.len = 1L)
  if (any(!vapply(learners, .is_mlr3_learner, logical(1)))) {
    .csdg_stop("`learners` must be a non-empty list of mlr3 Learners.")
  }
  if (is.null(names(learners)) || any(!nzchar(names(learners)))) {
    names(learners) = vapply(learners, function(x) x$id, character(1))
  }
  if (anyDuplicated(names(learners))) {
    .csdg_stop("Learner names must be unique.")
  }
  if (inherits(resampling, "CSDGGroupedResampling")) {
    resampling = resampling$resampling
  }
  checkmate::assert_class(resampling, "Resampling")
  rs = .clone_resampling(resampling)
  if (!isTRUE(rs$is_instantiated)) rs$instantiate(task)

  if (is.null(primary_measure)) {
    primary_measure = .default_measures(task)[[1L]]
  } else if (is.character(primary_measure)) {
    checkmate::assert_string(primary_measure, min.chars = 1L)
    primary_measure = mlr3::msr(primary_measure)
  }
  if (!inherits(primary_measure, "Measure")) {
    .csdg_stop("`primary_measure` must be a measure key or mlr3 Measure.")
  }
  if (!is.null(tolerance_absolute)) {
    assert_number(tolerance_absolute, lower = 0, finite = TRUE)
  }
  if (!is.null(tolerance_relative)) {
    assert_number(tolerance_relative, lower = 0, finite = TRUE)
  }
  has_tolerance = !is.null(tolerance_absolute) || !is.null(tolerance_relative)
  if (has_tolerance) {
    assert_string(tolerance_source, min.chars = 1L, .var.name = "tolerance_source")
    assert_string(tolerance_rationale, min.chars = 1L, .var.name = "tolerance_rationale")
  } else if (!is.null(tolerance_source) || !is.null(tolerance_rationale)) {
    .csdg_stop("Tolerance metadata requires `tolerance_absolute` or `tolerance_relative`.")
  }
  if (!is.null(reference_learner)) {
    assert_choice(reference_learner, names(learners))
  }

  resamples = vector("list", length(learners))
  rows = vector("list", length(learners))
  for (i in seq_along(learners)) {
    result = csdg_resample(
      task = task,
      learner = learners[[i]],
      resampling = rs,
      measures = list(primary_measure),
      store_models = store_models,
      seed = as.integer(seed) + i * 10000L
    )
    resamples[[i]] = result
    key = primary_measure$id
    vals = result$fold_scores[[key]]
    rows[[i]] = data.table::data.table(
      learner_name = names(learners)[[i]],
      learner_id = learners[[i]]$id,
      primary_measure = key,
      mean_score = mean(vals, na.rm = TRUE),
      sd_across_iterations = stats::sd(vals, na.rm = TRUE),
      median_score = stats::median(vals, na.rm = TRUE),
      n_iterations = sum(is.finite(vals))
    )
  }
  names(resamples) = names(learners)
  candidates = data.table::rbindlist(rows)
  direction = .measure_direction(primary_measure)
  reference_score = if (is.null(reference_learner)) {
    if (identical(direction, "minimize")) min(candidates$mean_score) else max(candidates$mean_score)
  } else {
    candidates[learner_name == reference_learner, mean_score][[1L]]
  }
  acceptance_basis = if (!has_tolerance) {
    "descriptive comparison; no acceptance rule"
  } else if (is.null(reference_learner)) {
    "best candidate"
  } else {
    "declared reference learner"
  }
  best = if (identical(direction, "minimize")) {
    min(candidates$mean_score, na.rm = TRUE)
  } else {
    max(candidates$mean_score, na.rm = TRUE)
  }
  tolerance_absolute_value = tolerance_absolute %||% 0
  tolerance_relative_value = tolerance_relative %||% 0
  limit = if (!has_tolerance) {
    NA_real_
  } else if (identical(direction, "minimize")) {
    reference_score + tolerance_absolute_value + abs(reference_score) * tolerance_relative_value
  } else {
    reference_score - tolerance_absolute_value - abs(reference_score) * tolerance_relative_value
  }
  if (identical(direction, "minimize")) {
    candidates[, accepted := if (has_tolerance) mean_score <= limit else NA]
    candidates[, distance_from_best := mean_score - best]
    candidates[, distance_from_reference := mean_score - reference_score]
  } else {
    candidates[, accepted := if (has_tolerance) mean_score >= limit else NA]
    candidates[, distance_from_best := best - mean_score]
    candidates[, distance_from_reference := reference_score - mean_score]
  }
  candidates[, `:=`(
    direction = direction,
    best_score = best,
    acceptance_reference_learner = reference_learner %||% candidates[which.min(distance_from_best), learner_name],
    acceptance_reference_score = reference_score,
    acceptance_basis = acceptance_basis,
    acceptance_limit = limit,
    tolerance_absolute = if (has_tolerance) tolerance_absolute_value else NA_real_,
    tolerance_relative = if (has_tolerance) tolerance_relative_value else NA_real_,
    tolerance_source = tolerance_source %||% NA_character_,
    tolerance_rationale = tolerance_rationale %||% NA_character_
  )]
  data.table::setorder(candidates, distance_from_best, learner_name)

  structure(
    list(
      candidates = candidates,
      resamples = resamples,
      primary_measure = primary_measure$id,
      direction = direction,
      best_score = best,
      acceptance_reference_learner = reference_learner %||%
        candidates[which.min(distance_from_best), learner_name],
      acceptance_reference_score = reference_score,
      acceptance_basis = acceptance_basis,
      acceptance_limit = limit,
      tolerance_source = tolerance_source,
      tolerance_rationale = tolerance_rationale,
      uncertainty_scope = paste(
        if (has_tolerance) {
          "Near-equivalence is defined by the supplied, documented performance tolerance."
        } else {
          "Candidate performance is descriptive because no near-equivalence tolerance was supplied."
        },
        "Fold scores are not treated as independent observations."
      )
    ),
    class = c("CSDGRashomon", "list")
  )
}

.csdg_recycle_tolerance = function(x, n, name) {
  checkmate::assert_numeric(x, lower = 0, any.missing = FALSE, finite = TRUE, min.len = 1L, .var.name = name)
  if (!length(x) %in% c(1L, n)) {
    .csdg_stop("`%s` must have length 1 or %d; got %d.", name, n, length(x))
  }
  rep_len(as.numeric(x), n)
}

#' @rdname csdg_generalization
#' @export
csdg_near_equivalence_sensitivity = function(
    x,
    tolerance_absolute = NULL,
    tolerance_relative = NULL,
    tolerance_source = NULL,
    tolerance_rationale = NULL,
    reference_learner = NULL,
    direction = c("auto", "minimize", "maximize"),
    scenario_id = NULL) {
  direction = match.arg(direction)
  if (is.null(tolerance_absolute) && is.null(tolerance_relative)) {
    .csdg_stop("Supply at least one explicit tolerance scenario.")
  }
  assert_string(tolerance_source, min.chars = 1L, .var.name = "tolerance_source")
  assert_string(tolerance_rationale, min.chars = 1L, .var.name = "tolerance_rationale")
  is_rashomon = inherits(x, "CSDGRashomon")
  candidates = if (is_rashomon) data.table::copy(x$candidates) else .as_dt(x)
  checkmate::assert_data_frame(candidates, min.rows = 1L, .var.name = "x")
  required = c("learner_name", "mean_score")
  missing = setdiff(required, names(candidates))
  if (length(missing)) {
    .csdg_stop("`x` is missing required columns: %s.", paste(missing, collapse = ", "))
  }
  checkmate::assert_character(
    candidates$learner_name,
    any.missing = FALSE,
    min.chars = 1L,
    unique = TRUE,
    .var.name = "x$learner_name"
  )
  checkmate::assert_numeric(
    candidates$mean_score,
    any.missing = FALSE,
    finite = TRUE,
    .var.name = "x$mean_score"
  )
  resolved_direction = if (!identical(direction, "auto")) {
    direction
  } else if (is_rashomon && x$direction %in% c("minimize", "maximize")) {
    x$direction
  } else if ("direction" %in% names(candidates) &&
      data.table::uniqueN(candidates$direction) == 1L &&
      candidates$direction[[1L]] %in% c("minimize", "maximize")) {
    candidates$direction[[1L]]
  } else {
    .csdg_stop("`direction` cannot be inferred; supply `\"minimize\"` or `\"maximize\"`.")
  }
  resolved_reference = reference_learner %||% if (is_rashomon) x$acceptance_reference_learner else NULL
  checkmate::assert_choice(resolved_reference, candidates$learner_name, .var.name = "reference_learner")
  reference_score = candidates[learner_name == resolved_reference, mean_score][[1L]]

  n_scenarios = max(
    length(tolerance_absolute %||% 0),
    length(tolerance_relative %||% 0),
    length(scenario_id %||% character())
  )
  tolerance_absolute = tolerance_absolute %||% 0
  tolerance_relative = tolerance_relative %||% 0
  tolerance_absolute = .csdg_recycle_tolerance(tolerance_absolute, n_scenarios, "tolerance_absolute")
  tolerance_relative = .csdg_recycle_tolerance(tolerance_relative, n_scenarios, "tolerance_relative")
  scenario_id = scenario_id %||% sprintf("scenario_%02d", seq_len(n_scenarios))
  checkmate::assert_character(
    scenario_id,
    any.missing = FALSE,
    len = n_scenarios,
    min.chars = 1L,
    unique = TRUE,
    .var.name = "scenario_id"
  )

  scenarios = data.table::data.table(
    scenario_order = seq_len(n_scenarios),
    scenario_id = scenario_id,
    tolerance_absolute = tolerance_absolute,
    tolerance_relative = tolerance_relative
  )
  result = scenarios[, {
    allowance = tolerance_absolute + abs(reference_score) * tolerance_relative
    limit = if (identical(resolved_direction, "minimize")) reference_score + allowance else reference_score - allowance
    distance = if (identical(resolved_direction, "minimize")) {
      candidates$mean_score - reference_score
    } else {
      reference_score - candidates$mean_score
    }
    accepted = if (identical(resolved_direction, "minimize")) {
      candidates$mean_score <= limit
    } else {
      candidates$mean_score >= limit
    }
    fraction = if (allowance > 0) {
      pmax(0, distance) / allowance
    } else {
      ifelse(distance <= 0, 0, Inf)
    }
    data.table::data.table(
      learner_name = candidates$learner_name,
      mean_score = candidates$mean_score,
      direction = resolved_direction,
      reference_learner = resolved_reference,
      reference_score = reference_score,
      acceptance_limit = limit,
      tolerance_allowance = allowance,
      distance_from_reference = distance,
      fraction_of_tolerance_consumed = fraction,
      accepted = accepted,
      tolerance_source = tolerance_source,
      tolerance_rationale = tolerance_rationale
    )
  }, by = .(scenario_order, scenario_id, tolerance_absolute, tolerance_relative)]
  data.table::setorder(result, scenario_order, mean_score, learner_name)
  result[, uncertainty_scope := paste(
    "Deterministic sensitivity to declared near-equivalence boundaries;",
    "fold scores are not treated as independent observations."
  )]
  result[]
}

#' @rdname csdg_generalization
#' @export
csdg_rashomon_agreement = function(
    rashomon,
    pfi,
    top_k = 10L) {
  checkmate::assert_class(rashomon, "CSDGRashomon")
  checkmate::assert_int(top_k, lower = 1)
  if (all(is.na(rashomon$candidates$accepted))) {
    .csdg_stop("`rashomon` has no accepted set because no near-equivalence tolerance was supplied.")
  }
  accepted = rashomon$candidates[accepted == TRUE, learner_name]
  checkmate::assert_list(pfi)
  if (is.null(names(pfi))) {
    .csdg_stop("`pfi` must be a named list of csdg_fold_pfi() results.")
  }
  missing = setdiff(accepted, names(pfi))
  if (length(missing)) {
    .csdg_stop(
      "Missing PFI results for accepted learners: %s.",
      paste(missing, collapse = ", ")
    )
  }

  ranks = data.table::rbindlist(lapply(accepted, function(nm) {
    tab = data.table::copy(pfi[[nm]]$summary)
    tab[, learner_name := nm]
    tab[, .(learner_name, feature_group, rank, mean_importance)]
  }))
  n_features = data.table::uniqueN(ranks$feature_group)
  if (!n_features) {
    .csdg_stop("Rashomon agreement requires at least one ranked feature group.")
  }
  effective_top_k = min(top_k, n_features)
  top_sets = lapply(split(ranks, ranks$learner_name), function(tab) {
    utils::head(tab[order(rank), feature_group], effective_top_k)
  })
  pairs = if (length(top_sets) >= 2L) {
    utils::combn(names(top_sets), 2L, simplify = FALSE)
  } else {
    list()
  }
  pairwise = if (length(pairs)) {
    data.table::rbindlist(lapply(pairs, function(pair) {
      a = top_sets[[pair[[1L]]]]
      b = top_sets[[pair[[2L]]]]
      union = union(a, b)
      data.table::data.table(
        learner_1 = pair[[1L]],
        learner_2 = pair[[2L]],
        top_k = effective_top_k,
        intersection = length(intersect(a, b)),
        union = length(union),
        jaccard = if (length(union)) length(intersect(a, b)) / length(union) else NA_real_
      )
    }))
  } else {
    data.table::data.table(
      learner_1 = character(), learner_2 = character(),
      top_k = integer(), intersection = integer(), union = integer(),
      jaccard = numeric()
    )
  }
  list(ranks = ranks, pairwise_top_k = pairwise)
}

#' @rdname csdg_generalization
#' @export
csdg_leave_one_group_out = function(
    task,
    learner,
    group,
    measures = NULL,
    seed = 20260201L,
    store_models = FALSE) {
  .require_task(task)
  .require_learner(learner)
  checkmate::assert_int(seed, lower = 0)
  checkmate::assert_flag(store_models)
  row_ids = task$row_ids
  group = .normalize_named_vector(group, row_ids, "group")
  if (anyNA(group)) .csdg_stop("`group` must not contain missing values.")
  group = as.character(group)
  groups = sort(unique(group))
  if (length(groups) < 2L) {
    .csdg_stop("At least two settings are required.")
  }
  measures = .normalize_measures(measures, task)

  scores = vector("list", length(groups))
  predictions = vector("list", length(groups))
  models = if (store_models) vector("list", length(groups)) else NULL
  for (i in seq_along(groups)) {
    held_out = groups[[i]]
    test = row_ids[group == held_out]
    train = row_ids[group != held_out]
    if (!length(test) || !length(train)) {
      .csdg_stop("Empty train or assessment set for setting `%s`.", held_out)
    }
    set.seed(as.integer(seed) + i - 1L)
    fit = learner$clone(deep = TRUE)
    fit$train(task, row_ids = train)
    pred = fit$predict(task, row_ids = test)
    pred_dt = data.table::as.data.table(pred)
    if ("row_ids" %in% names(pred_dt) && !"row_id" %in% names(pred_dt)) {
      data.table::setnames(pred_dt, "row_ids", "row_id")
    }
    pred_dt[, `:=`(held_out_group = held_out, iteration = i)]
    predictions[[i]] = pred_dt
    sc = pred$score(measures)
    row = data.table::data.table(
      held_out_group = held_out,
      n_train = length(train),
      n_assessment = length(test)
    )
    for (nm in names(sc)) row[[nm]] = unname(sc[[nm]])
    scores[[i]] = row
    if (store_models) models[[i]] = fit
  }
  list(
    scores = data.table::rbindlist(scores, fill = TRUE),
    predictions = data.table::rbindlist(predictions, fill = TRUE),
    models = models,
    group = stats::setNames(group, row_ids),
    limitations = c(
      "Leave-one-setting-out performance supports only the represented setting contrast.",
      "Refitting preserves the learner pipeline but cannot remove unmeasured setting differences.",
      "The analysis is predictive and does not identify causal effect transport."
    )
  )
}

#' @rdname csdg_generalization
#' @export
csdg_transport_status = function(
    scores,
    measure,
    direction = c("maximize", "minimize"),
    minimum_transport_score = NULL,
    maximum_transport_score = NULL,
    criterion_source = NULL,
    criterion_rationale = NULL) {
  checkmate::assert_character(direction, any.missing = FALSE, min.len = 1L)
  direction = match.arg(direction)
  checkmate::assert_string(measure, min.chars = 1L)
  checkmate::assert_data_frame(scores, min.rows = 1L)
  dt = .as_dt(scores)
  if (!measure %in% names(dt)) {
    .csdg_stop("Measure `%s` is absent from `scores`.", measure)
  }
  values = dt[[measure]]
  checkmate::assert_numeric(values, any.missing = TRUE, min.len = 1L)
  finite = is.finite(values)
  threshold = if (identical(direction, "maximize")) {
    minimum_transport_score
  } else {
    maximum_transport_score
  }
  if (!is.null(threshold)) {
    checkmate::assert_number(threshold, finite = TRUE)
  }
  criterion_complete = is.null(threshold) ||
    (.is_scalar_string(criterion_source) && .is_scalar_string(criterion_rationale))
  if (!any(finite)) {
    return(list(
      status = "unresolved",
      passed = NA,
      threshold = threshold,
      criterion_source = criterion_source,
      criterion_rationale = criterion_rationale,
      criterion_complete = criterion_complete,
      reason = "No finite transport scores were available."
    ))
  }
  if (!is.null(threshold) && !criterion_complete) {
    return(list(
      status = "unresolved",
      passed = NA,
      threshold = threshold,
      criterion_source = criterion_source,
      criterion_rationale = criterion_rationale,
      criterion_complete = FALSE,
      reason = "The supplied transport threshold lacks a recorded source or rationale."
    ))
  }
  if (identical(direction, "maximize")) {
    if (is.null(minimum_transport_score)) {
      return(list(
        status = "unresolved",
        passed = NA,
        threshold = NULL,
        criterion_source = NULL,
        criterion_rationale = NULL,
        criterion_complete = FALSE
      ))
    }
    passed = all(values[finite] >= minimum_transport_score)
    list(
      status = if (passed) "met" else "not_met",
      passed = passed,
      threshold = minimum_transport_score,
      criterion_source = criterion_source,
      criterion_rationale = criterion_rationale,
      criterion_complete = TRUE
    )
  } else {
    if (is.null(maximum_transport_score)) {
      return(list(
        status = "unresolved",
        passed = NA,
        threshold = NULL,
        criterion_source = NULL,
        criterion_rationale = NULL,
        criterion_complete = FALSE
      ))
    }
    passed = all(values[finite] <= maximum_transport_score)
    list(
      status = if (passed) "met" else "not_met",
      passed = passed,
      threshold = maximum_transport_score,
      criterion_source = criterion_source,
      criterion_rationale = criterion_rationale,
      criterion_complete = TRUE
    )
  }
}
