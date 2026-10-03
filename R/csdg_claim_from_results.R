#' Derive a claim from the results
#'
#' @description
#' Third step of the analysis-first interface: after inspecting the held-out permutation feature importance (PFI) of
#' [csdg_importance()] or the accumulated local effects (ALE) of [csdg_effect()], state what they show as a claim.
#' The package writes the claim statement, its six scope elements (quantity, model, procedure, data, meaning, and
#' use), and the selection basis, and records the claim with [csdg_claim()] as exploratory
#' (`provenance$origin = "retrospective_exploratory"`), because it was formulated after the results were seen.
#' [csdg_check()] then computes the default checks and [csdg_assess()] applies the decision rule; [csdg_confirm()]
#' applies the same criteria to new data.
#'
#' * `claim_relies_mainly()`: "XGBoost relies mainly on low_mood and fatigue": each named predictor has at least
#'   `factor` times the PFI of every other predictor.
#' * `claim_top_k()`: "The three predictors on which XGBoost relies most are ...": the named predictors have the `k`
#'   largest PFI values (`k = 1`: "relies most on").
#' * `claim_order()`: "XGBoost relies more on `first` than on `second`"; with two predictors in `first` and no
#'   `second`, the pair is ordered by the average PFI of the first learner.
#' * `claim_direction()`: "The predictions of XGBoost rise with `feature`": the ALE changes by at least a minimum
#'   change between two percentiles of the predictor (default 10th and 90th) in held-out data.
#'
#' The claim covers the learners in `learners` (names or labels); selections (the top `k`, the order of a pair, the
#' direction) are made on the first.
#' If it covers several learners, the result must hold for each, and its content is checked under G6a; learners in
#' the results that the claim does not cover are reported as context.
#' Arguments follow the order object, specifics, learners.
#'
#' @param imp A [csdg_importance()] result.
#' @param x For `claim_direction()`: a [csdg_effect()] result (recommended, after looking at it) or a [csdg_fit()]
#'   or [csdg_importance()] result, from which the ALE is computed.
#' @param k Number of named predictors.
#' @param factor Minimum ratio of the PFI of each named predictor to that of every other predictor (at least 1).
#' @param learners Names or labels of the learners the claim covers; the first suggested the claim.
#'   Defaults to the first learner; `names(imp$pfi)` covers all.
#' @param predictors Optional predictors to name; defaults to the `k` predictors with the largest average PFI.
#' @param first,second Two different predictors; the claim states that `first` matters more.
#'   Alternatively, `first` names both and `second` is omitted.
#' @param feature A numeric predictor.
#' @param direction `"rise"` or `"fall"`; defaults to the sign of the average change of the first learner.
#' @param probs Two increasing probabilities: the percentiles between which the change is measured (ignored for a
#'   [csdg_effect()] result, which fixes them).
#' @param intervals Number of quantile intervals of the ALE (ignored for a [csdg_effect()] result).
#' @param marginal_only If `TRUE`, the statement begins with "Under marginal permutation,"; grouped or conditional
#'   PFI that reverses the result then no longer contradicts the procedure property, and a strong correlation of a
#'   named predictor is context rather than an unresolved threat.
#' @param refers_to `"variables"` if the claim names the analyzed variables, `"constructs"` if it names constructs
#'   measured by them (then validity evidence must be entered with [csdg_judge()]).
#' @param labels Optional named character vector, predictor -> wording in the statement.
#' @param id Optional claim identifier; generated from the kind, the predictors, and the learners by default.
#' @param note Optional note stored and printed with the claim.
#'
#' @return A `CSDGDerivedClaim` list with `id`, `kind`, `statement`, `note`, `learner` (the first learner),
#'   `covered`, the claim's parameters, `selection` (how the claim arose), `scope` (six generated scope elements),
#'   `record` (the [csdg_claim()] card), `effect` (fold changes of the ALE for direction claims), and `source` (the
#'   results it was derived from).
#' @seealso [csdg_check()], [csdg_assess()], [csdg_confirm()]
#' @examples
#' set.seed(1)
#' n = 200
#' x = data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
#' x$y = 2 * x$a + x$b + rnorm(n)
#' task = mlr3::as_task_regr(x, target = "y", id = "toy")
#' fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3, seed = 1)
#' imp = csdg_importance(fits, repetitions = 5, seed = 2)
#' claim_relies_mainly(imp, k = 1)
#' claim_order(imp, c("a", "b"))
#' eff = csdg_effect(fits, "a")
#' claim_direction(eff, "a")
#' @name claim_from_results
NULL

#' @rdname claim_from_results
#' @export
claim_relies_mainly = function(imp, k = 2L, factor = 2, learners = NULL, predictors = NULL, marginal_only = FALSE,
                               refers_to = c("variables", "constructs"), labels = NULL, id = NULL, note = NULL) {
  assert_class(imp, "CSDGImportance", .var.name = "imp")
  assert_number(factor, lower = 1, finite = TRUE, .var.name = "factor")
  if (missing(k) && !is.null(predictors)) k = length(predictors)
  .csdg_ranking_claim(imp, kind = "relies_mainly", learners = learners, k = k, factor = factor,
    predictors = predictors, marginal_only = marginal_only, refers_to = refers_to, labels = labels, id = id,
    note = note)
}

#' @rdname claim_from_results
#' @export
claim_top_k = function(imp, k = 3L, learners = NULL, predictors = NULL, marginal_only = FALSE,
                       refers_to = c("variables", "constructs"), labels = NULL, id = NULL, note = NULL) {
  assert_class(imp, "CSDGImportance", .var.name = "imp")
  if (missing(k) && !is.null(predictors)) k = length(predictors)
  .csdg_ranking_claim(imp, kind = "top_k", learners = learners, k = k, factor = 1, predictors = predictors,
    marginal_only = marginal_only, refers_to = refers_to, labels = labels, id = id, note = note)
}

#' @rdname claim_from_results
#' @export
claim_order = function(imp, first, second = NULL, learners = NULL, marginal_only = FALSE,
                       refers_to = c("variables", "constructs"), labels = NULL, id = NULL, note = NULL) {
  assert_class(imp, "CSDGImportance", .var.name = "imp")
  features = imp$fits$features
  learners = .csdg_claim_learners(imp$fits, learners)
  learner = learners[[1L]]
  averages = .csdg_marginal_averages(imp, learner)
  ordered_by_results = is.null(second)
  if (ordered_by_results) {
    assert_character(first, len = 2L, any.missing = FALSE, unique = TRUE, .var.name = "first")
    assert_subset(first, features, .var.name = "first")
    pair = first[order(-averages[first], seq_along(first))]
    first = pair[[1L]]
    second = pair[[2L]]
  }
  assert_choice(first, features, .var.name = "first")
  assert_choice(second, features, .var.name = "second")
  if (identical(first, second)) .csdg_stop("`first` and `second` must be different predictors.")
  larger = averages[[first]] > averages[[second]]
  if (!larger) {
    .csdg_warn("`first` (%s) does not have the larger average PFI of the two; the content check will show this.",
      first)
  }
  base = .csdg_claim_base(imp$fits, "order", learners, marginal_only, refers_to, labels, id, note,
    parts = c(first, second))
  selection = sprintf("Formulated after inspecting the held-out PFI of %s; %s has the %s average PFI of the two%s.",
    .csdg_learner_label(imp$fits, learner), .csdg_feature_label(base, first), if (larger) "larger" else
      "smaller", if (ordered_by_results) " (the pair was ordered by these averages)" else "")
  .csdg_new_derived_claim(base, imp$fits, imp, params = list(first = first, second = second), selection = selection)
}

#' @rdname claim_from_results
#' @export
claim_direction = function(x, feature, direction = NULL, learners = NULL, probs = c(0.1, 0.9), intervals = 20L,
                           refers_to = c("variables", "constructs"), labels = NULL, id = NULL, note = NULL) {
  assert_multi_class(x, c("CSDGEffect", "CSDGFits", "CSDGImportance"), .var.name = "x")
  effect_object = if (inherits(x, "CSDGEffect")) x else NULL
  imp = if (inherits(x, "CSDGImportance")) x else NULL
  fits = if (inherits(x, "CSDGFits")) x else x$fits
  assert_string(feature, .var.name = "feature")
  if (!is.null(effect_object)) {
    if (!feature %in% effect_object$features) {
      .csdg_stop("The effects contain no ALE of %s; compute it with csdg_effect(fits, \"%s\").", feature, feature)
    }
    probs = effect_object$probs
    intervals = effect_object$intervals
  } else {
    assert_choice(feature, fits$features, .var.name = "feature")
    if (is.null(fits$task) || !is.numeric(fits$task$data(cols = feature)[[feature]])) {
      .csdg_stop("`feature` must be a numeric predictor of the task; %s is not.", feature)
    }
    .csdg_check_probs(probs)
    assert_int(intervals, lower = 2L, upper = 100L, .var.name = "intervals")
  }
  learners = .csdg_claim_learners(fits, learners)
  learner = learners[[1L]]
  if (!is.null(direction)) assert_choice(direction, c("rise", "fall"), .var.name = "direction")
  effect = if (is.null(effect_object)) {
    .csdg_direction_effects(fits, feature, probs, intervals)$changes
  } else {
    wanted = feature
    effect_object$changes[effect_object$changes$feature == wanted]
  }
  mean_change = mean(effect$change[effect$learner == learner])
  observed = if (mean_change > 0) "rise" else "fall"
  if (!is.null(direction) && !identical(direction, observed)) {
    .csdg_warn("The average change of %s is %s, against the stated direction `%s`; the content check will show this.",
      .csdg_learner_label(fits, learner), .csdg_num(mean_change), direction)
  }
  direction = direction %||% observed
  base = .csdg_claim_base(fits, "direction", learners, FALSE, refers_to, labels, id, note, parts = feature)
  sign_word = if (mean_change > 0) "positive" else if (mean_change < 0) "negative" else "zero"
  between = sprintf("between the %s and %s percentiles of %s", .csdg_ordinal(probs[[1L]]), .csdg_ordinal(probs[[2L]]),
    .csdg_feature_label(base, feature))
  selection = if (is.null(effect_object)) {
    sprintf("Derived from the ALE of %s computed by claim_direction(); the average change %s is %s (%s).",
      .csdg_learner_label(fits, learner), between, .csdg_num(mean_change), sign_word)
  } else {
    sprintf("Formulated after inspecting the ALE of %s (csdg_effect()); the average change %s is %s (%s).",
      .csdg_learner_label(fits, learner), between, .csdg_num(mean_change), sign_word)
  }
  .csdg_new_derived_claim(base, fits, imp,
    params = list(feature = feature, direction = direction, probs = probs, intervals = as.integer(intervals)),
    selection = selection, effect = effect)
}

.csdg_claim_learners = function(fits, learners) {
  if (is.numeric(learners)) .csdg_stop("`learners` takes learner names or labels; numbers belong to `k`.")
  known = names(fits$resamples) %||% names(fits$labels)
  .csdg_resolve_learner_names(fits, learners %||% known[[1L]], "learners")
}

# Average marginal PFI of one learner, named by predictor, in the order of the task's features.
.csdg_marginal_averages = function(imp, learner) {
  rows = which(imp$summary$learner == learner & imp$summary$type == "marginal")
  tab = imp$summary[rows]
  out = stats::setNames(tab$mean_importance, tab$feature_group)
  out[intersect(imp$fits$features, names(out))]
}

# Top k of named averages; ties are broken by feature order (a tie at the cutoff is warned about by the caller).
.csdg_top = function(averages, k) {
  names(averages)[order(-averages, seq_along(averages))][seq_len(k)]
}

.csdg_tie_at_cutoff = function(averages, k) {
  sorted = sort(averages, decreasing = TRUE)
  k < length(sorted) && isTRUE(all.equal(sorted[[k]], sorted[[k + 1L]], tolerance = 1e-12))
}

.csdg_claim_base = function(fits, kind, learners, marginal_only, refers_to, labels, id, note, parts) {
  refers_to = match.arg(refers_to, c("variables", "constructs"))
  assert_flag(marginal_only, .var.name = "marginal_only")
  if (!is.null(labels)) {
    assert_character(labels, any.missing = FALSE, min.chars = 1L, names = "unique", .var.name = "labels")
    unknown = setdiff(names(labels), fits$features)
    if (length(unknown)) .csdg_stop("Unknown predictors in `labels`: %s.", paste(unknown, collapse = ", "))
  }
  assert_string(id, min.chars = 1L, null.ok = TRUE, .var.name = "id")
  assert_string(note, min.chars = 1L, null.ok = TRUE, .var.name = "note")
  all_learners = names(fits$resamples) %||% names(fits$labels)
  learner_part = if (length(learners) > 1L && setequal(learners, all_learners)) "all" else learners
  id = id %||% .safe_name(paste(c(kind, parts, learner_part), collapse = "_"))
  list(id = id, kind = kind, learner = learners[[1L]], covered = learners, marginal_only = marginal_only,
    refers_to = refers_to, labels = labels, note = note)
}

.csdg_ranking_claim = function(imp, kind, learners, k, factor, predictors, marginal_only, refers_to, labels, id,
                               note) {
  features = imp$fits$features
  p = length(features)
  if (p < 2L) .csdg_stop("A ranking claim needs at least two predictors.")
  if (is.character(k)) .csdg_stop("`k` must be a number; name the learners with `learners = \"%s\"`.", k[[1L]])
  assert_int(k, lower = 1L, upper = p - 1L, .var.name = "k")
  k = as.integer(k)
  learners = .csdg_claim_learners(imp$fits, learners)
  learner = learners[[1L]]
  averages = .csdg_marginal_averages(imp, learner)
  top = .csdg_top(averages, k)
  if (.csdg_tie_at_cutoff(averages, k)) {
    .csdg_warn("The average PFI of %s ties at the cutoff k = %d; the selection is not unique.",
      .csdg_learner_label(imp$fits, learner), k)
  }
  if (is.null(predictors)) {
    predictors = top
  } else {
    assert_character(predictors, any.missing = FALSE, unique = TRUE, .var.name = "predictors")
    unknown = setdiff(predictors, features)
    if (length(unknown)) .csdg_stop("Unknown predictors: %s.", paste(unknown, collapse = ", "))
    if (length(predictors) != k) .csdg_stop("`predictors` must name `k` = %d predictors.", k)
    if (!setequal(predictors, top)) {
      .csdg_warn(paste(
        "The claim names predictors that are not the %s largest on average; the content check will show this."
      ), if (k == 1L) "one" else .csdg_number_word(k))
    }
  }
  base = .csdg_claim_base(imp$fits, kind, learners, marginal_only, refers_to, labels, id, note, parts = predictors)
  learner_label = .csdg_learner_label(imp$fits, learner)
  named = .csdg_and(.csdg_feature_label(base, predictors))
  leading = if (k == 1L) "its leading predictor" else sprintf("its %s leading predictors", .csdg_number_word(k))
  selection = if (setequal(predictors, top)) {
    sprintf("Formulated after inspecting the held-out PFI of %s; %s %s %s on average.", learner_label, named,
      if (k == 1L) "is" else "are", leading)
  } else {
    sprintf("Formulated after inspecting the held-out PFI of %s; the claim names %s, which %s not %s on average.",
      learner_label, named, if (k == 1L) "is" else "are", leading)
  }
  .csdg_new_derived_claim(base, imp$fits, imp, params = list(predictors = predictors, k = k, factor = factor),
    selection = selection)
}

.csdg_new_derived_claim = function(base, fits, imp, params, selection, effect = NULL) {
  claim = c(base, list(
    predictors = params$predictors, k = params$k, factor = params$factor, first = params$first,
    second = params$second, feature = params$feature, direction = params$direction, probs = params$probs,
    intervals = params$intervals, labels_learner = fits$labels[base$covered], selection = selection,
    effect = effect
  ))
  claim$statement = .csdg_derived_statement(claim, fits)
  claim$scope = .csdg_derived_scope(claim, fits, imp)
  claim$source = list(task_id = fits$task_id, features = fits$features, loss = if (is.null(imp)) fits$loss else
    imp$loss, K = fits$K, R = if (is.null(imp)) NA_integer_ else imp$repetitions, data_hash = fits$data_hash)
  claim$origin = "retrospective_exploratory"
  claim$record = .csdg_derived_record(claim, origin = "retrospective_exploratory", date = format(Sys.Date()),
    time_basis = "date the claim was derived from the results", selection_basis = selection,
    evidence_ids = character())
  structure(claim, class = c("CSDGDerivedClaim", "list"))
}

.csdg_derived_record = function(claim, origin, date, time_basis, selection_basis, evidence_ids) {
  csdg_claim(
    id = claim$id,
    statement = claim$statement,
    claim_type = "global_explanation",
    quantity = claim$scope$quantity,
    model_scope = if (length(claim$covered) > 1L) "several_models" else "learner",
    procedure = claim$scope$procedure,
    data = claim$scope$data,
    meaning = "model_description",
    use = "scientific description; no decision about individuals",
    unit = "row of the task",
    population = "the analyzed sample",
    notes = claim$note,
    provenance = list(origin = origin, date = date, time_basis = time_basis, selection_basis = selection_basis,
      evidence_ids = evidence_ids)
  )
}

.csdg_derived_statement = function(claim, fits) {
  labels = unname(fits$labels[claim$covered])
  several = length(labels) > 1L
  subject = .csdg_and(labels)
  both = if (length(labels) == 2L) "both" else "all"
  feature = function(x) .csdg_feature_label(claim, x)
  statement = switch(claim$kind,
    relies_mainly = if (several) {
      sprintf("%s %s rely mainly on %s.", subject, both, .csdg_and(feature(claim$predictors)))
    } else {
      sprintf("%s relies mainly on %s.", subject, .csdg_and(feature(claim$predictors)))
    },
    top_k = if (claim$k == 1L) {
      if (several) sprintf("%s %s rely most on %s.", subject, both, feature(claim$predictors)) else
        sprintf("%s relies most on %s.", subject, feature(claim$predictors))
    } else {
      sprintf("The %s predictors on which %s %s most are %s.", .csdg_number_word(claim$k), subject,
        if (several) "each rely" else "relies", .csdg_and(feature(claim$predictors)))
    },
    order = if (several) {
      sprintf("%s %s rely more on %s than on %s.", subject, both, feature(claim$first), feature(claim$second))
    } else {
      sprintf("%s relies more on %s than on %s.", subject, feature(claim$first), feature(claim$second))
    },
    direction = sprintf("The predictions of %s %s with %s.", subject, claim$direction, feature(claim$feature))
  )
  if (isTRUE(claim$marginal_only)) statement = paste0("Under marginal permutation, ", statement)
  .csdg_sentence(statement)
}

.csdg_ordinal = function(p) {
  vapply(p, function(v) {
    percent = round(100 * v, 2)
    value = format(percent)
    if (abs(percent - round(percent)) > 1e-9) return(paste0(value, "th"))
    last = as.integer(round(percent)) %% 100L
    suffix = if (last %% 10L == 1L && last != 11L) "st" else if (last %% 10L == 2L && last != 12L) "nd" else
      if (last %% 10L == 3L && last != 13L) "rd" else "th"
    paste0(value, suffix)
  }, character(1L), USE.NAMES = FALSE)
}

.csdg_model_scope = function(fits, learners) {
  labels = unname(fits$labels[learners])
  ids = fits$learner_ids[learners]
  parts = if (is.null(ids) || anyNA(ids)) labels else sprintf("%s (mlr3 learner %s)", labels, unname(ids))
  sprintf("%s.", .csdg_sentence(.csdg_and(parts)))
}

.csdg_missing_features = function(fits) {
  if (is.null(fits$task)) return(character())
  miss = fits$task$missings(fits$features)
  base::names(miss)[miss > 0]
}

.csdg_training_text = function(fits) {
  n_train = mean(fits$n_train)
  K = fits$K
  share = if (K >= 2L && abs(n_train / fits$n - (K - 1) / K) < 0.01) {
    sprintf("%d/%d of the sample", K - 1L, K)
  } else {
    sprintf("%s of the sample", .csdg_pct(n_train / fits$n))
  }
  sprintf("training sets of %s rows (%s)", .csdg_count(round(n_train)), share)
}

.csdg_derived_scope = function(claim, fits, imp) {
  labels = unname(fits$labels[claim$covered])
  learner_text = if (length(labels) > 1L) paste0(.csdg_and(labels), ", each") else labels
  training = .csdg_training_text(fits)
  if (identical(claim$kind, "direction")) {
    target = if (identical(fits$task_type, "classif")) "predicted probability" else "prediction"
    quantity = sprintf(paste(
      "Learner ALE of %s: the change of the %s between its %s and %s percentiles in held-out data, for %s fitted",
      "to %s."
    ), .csdg_feature_label(claim, claim$feature), target, .csdg_ordinal(claim$probs[[1L]]),
    .csdg_ordinal(claim$probs[[2L]]), learner_text, training)
    procedure = sprintf(paste(
      "%s with the same splits for all learners: one fit per training set; ALE on %d quantile intervals of the",
      "held-out fold, averaged over folds."
    ), fits$resampling_label, claim$intervals)
  } else {
    quantity = sprintf(paste(
      "Learner PFI of each predictor under marginal permutation, which includes combinations absent from the data:",
      "the expected increase in held-out %s for %s fitted to %s."
    ), .csdg_loss_label(imp$loss), learner_text, training)
    procedure = .csdg_procedure_text(fits, imp)
  }
  outcome = if (identical(fits$task_type, "classif")) {
    sprintf("outcome %s (positive class %s)", fits$target, fits$positive)
  } else {
    sprintf("outcome %s", fits$target)
  }
  missing_features = .csdg_missing_features(fits)
  missing_text = if (length(missing_features)) {
    sprintf("missing values in %s, handled by the learners", .csdg_and(missing_features))
  } else {
    "no missing values"
  }
  list(
    quantity = .csdg_sentence(quantity),
    model = .csdg_model_scope(fits, claim$covered),
    procedure = .csdg_sentence(procedure),
    data = sprintf("Task %s: %s rows, %d features, %s; %s.", fits$task_id, .csdg_count(fits$n),
      length(fits$features), outcome, missing_text),
    meaning = "Model description.",
    use = "Scientific description; no decision about individuals."
  )
}

.csdg_procedure_text = function(fits, imp) {
  text = sprintf(paste(
    "%s with the same splits for all learners: one fit per training set; each predictor permuted %d times in the",
    "held-out fold; increases averaged over permutations and folds"
  ), fits$resampling_label, imp$repetitions)
  for (group in base::names(imp$groups)) {
    text = paste0(text, sprintf("; grouped PFI for %s (%s)", group, paste(imp$groups[[group]], collapse = ", ")))
  }
  for (feature in imp$strata_features) {
    text = paste0(text, sprintf("; conditional PFI of %s within %s", feature, imp$strata_definition[[feature]]))
  }
  paste0(text, ".")
}

#' @export
format.CSDGDerivedClaim = function(x, width = getOption("width", 80L), ...) {
  origin = if (identical(x$origin, "independently_confirmed")) "tested on new data" else "exploratory"
  c(
    sprintf("<CSDG claim> %s", x$id),
    .csdg_wrap(x$statement, indent = 2L, width = width),
    .csdg_wrap(sprintf("Origin: %s. %s", origin, x$selection), indent = 2L, width = width),
    if (!is.null(x$note)) .csdg_wrap(sprintf("Note: %s", x$note), indent = 2L, width = width)
  )
}

#' @export
print.CSDGDerivedClaim = function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
as.data.table.CSDGDerivedClaim = function(x, ...) {
  data.table(id = x$id, kind = x$kind, statement = x$statement, origin = x$origin, quantity = x$scope$quantity,
    model = x$scope$model, procedure = x$scope$procedure, data = x$scope$data, meaning = x$scope$meaning,
    use = x$scope$use)
}
