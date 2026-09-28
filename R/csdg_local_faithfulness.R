
.make_local_neighborhood = function(case, background, n, seed) {
  set.seed(seed)
  out = background[sample.int(nrow(background), n, replace = TRUE), ]
  for (nm in names(case)) {
    x = background[[nm]]
    value = case[[nm]][[1L]]
    if (is.numeric(x)) {
      scale = stats::sd(x, na.rm = TRUE)
      if (!is.finite(scale) || scale == 0) scale = 1
      out[[nm]] = stats::rnorm(n, mean = as.numeric(value), sd = 0.25 * scale)
      limits = range(x, na.rm = TRUE)
      if (all(is.finite(limits))) {
        out[[nm]] = pmin(pmax(out[[nm]], limits[[1L]]), limits[[2L]])
      }
      if (is.integer(x)) out[[nm]] = as.integer(round(out[[nm]]))
    } else if (is.factor(x)) {
      lvls = levels(x)
      probs = prop.table(table(factor(x, levels = lvls), useNA = "no"))
      draws = sample(lvls, n, replace = TRUE, prob = probs)
      keep = stats::runif(n) < 0.70
      draws[keep] = as.character(value)
      out[[nm]] = factor(draws, levels = lvls, ordered = is.ordered(x))
    } else if (is.logical(x)) {
      draws = sample(c(FALSE, TRUE), n, replace = TRUE,
                      prob = c(mean(!x, na.rm = TRUE), mean(x, na.rm = TRUE)))
      keep = stats::runif(n) < 0.70
      draws[keep] = as.logical(value)
      out[[nm]] = draws
    } else {
      values = unique(stats::na.omit(as.character(x)))
      draws = sample(values, n, replace = TRUE)
      keep = stats::runif(n) < 0.70
      draws[keep] = as.character(value)
      out[[nm]] = draws
    }
  }
  out
}

.make_empirical_local_neighborhood = function(case, background, n, seed, neighbors = n) {
  distance = .local_distance(background, case, background)
  finite = which(is.finite(distance))
  if (!length(finite)) {
    .csdg_stop("No finite empirical neighbors are available for the local case.")
  }
  neighbors = min(max(2L, as.integer(neighbors)), length(finite))
  candidate = finite[order(distance[finite])][seq_len(neighbors)]
  set.seed(seed)
  sampled = background[sample(candidate, n, replace = TRUE)]
  .impute_local_neighborhood_from_background(sampled, background)
}

.local_prediction_scale = function(prediction, task_type, target_scale) {
  if (identical(task_type, "regr") || identical(target_scale, "response")) {
    return(as.numeric(prediction))
  }
  stats::qlogis(.clip_probability(prediction))
}

.local_distance = function(neighborhood, case, background) {
  pieces = matrix(0, nrow(neighborhood), ncol(neighborhood))
  for (j in seq_along(neighborhood)) {
    nm = names(neighborhood)[[j]]
    x = neighborhood[[nm]]
    cval = case[[nm]][[1L]]
    if (is.numeric(x)) {
      s = stats::sd(background[[nm]], na.rm = TRUE)
      if (!is.finite(s) || s == 0) s = 1
      pieces[, j] = (as.numeric(x) - as.numeric(cval)) / s
    } else {
      pieces[, j] = as.numeric(as.character(x) != as.character(cval))
    }
  }
  sqrt(rowMeans(pieces^2, na.rm = TRUE))
}

.local_crossfit_folds = function(n, seed, folds = 5L) {
  folds = min(as.integer(folds), as.integer(n))
  set.seed(as.integer(seed))
  sample(rep(seq_len(folds), length.out = n))
}

.local_effective_n = function(weights) {
  weights = as.numeric(weights)
  weights = weights[is.finite(weights) & weights > 0]
  if (!length(weights)) return(0)
  sum(weights)^2 / sum(weights^2)
}

.local_weighted_metrics = function(observed, predicted, weights) {
  keep = is.finite(observed) & is.finite(predicted) & is.finite(weights) & weights > 0
  if (!any(keep)) {
    return(list(
      r2 = NA_real_,
      rmse = NA_real_,
      mae = NA_real_,
      maximum_absolute_error = NA_real_
    ))
  }
  observed = as.numeric(observed[keep])
  predicted = as.numeric(predicted[keep])
  weights = as.numeric(weights[keep])
  weights = weights / max(weights)
  weighted_mean = sum(weights * observed) / sum(weights)
  sse = sum(weights * (observed - predicted)^2)
  sst = sum(weights * (observed - weighted_mean)^2)
  list(
    r2 = if (sst > 0) 1 - sse / sst else NA_real_,
    rmse = sqrt(sse / sum(weights)),
    mae = sum(weights * abs(observed - predicted)) / sum(weights),
    maximum_absolute_error = max(abs(observed - predicted))
  )
}

.local_weighted_ridge = function(design, outcome, weights) {
  if (!is.matrix(design) || nrow(design) != length(outcome) || length(outcome) != length(weights)) {
    .csdg_stop("Local surrogate design, outcome, and weight dimensions do not agree.")
  }
  if (any(!is.finite(design)) || any(!is.finite(outcome))) {
    .csdg_stop("Local surrogate design and predictions must be finite.")
  }
  intercept = match("(Intercept)", colnames(design))
  if (is.na(intercept)) {
    .csdg_stop("Local surrogate design must contain an intercept.")
  }
  weights = as.numeric(weights)
  if (any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0)) {
    .csdg_stop("Local surrogate weights must be finite, nonnegative, and not all zero.")
  }
  weights = weights / max(weights)
  weights = weights / mean(weights)
  feature_columns = setdiff(seq_len(ncol(design)), intercept)
  x = design[, feature_columns, drop = FALSE]
  weighted_mean = if (ncol(x)) colSums(x * weights) / sum(weights) else numeric()
  centered = if (ncol(x)) sweep(x, 2L, weighted_mean, FUN = "-") else x
  weighted_variance = if (ncol(x)) colSums(centered^2 * weights) / sum(weights) else numeric()
  active = which(is.finite(weighted_variance) & weighted_variance > 0)
  scaled = if (length(active)) {
    sweep(centered[, active, drop = FALSE], 2L, sqrt(weighted_variance[active]), FUN = "/")
  } else {
    matrix(numeric(), nrow = nrow(design), ncol = 0L)
  }
  scaled_design = cbind("(Intercept)" = 1, scaled)
  effective_n = .local_effective_n(weights)
  penalty_fraction = max(1e-4, 0.01 * length(active) / max(effective_n, 1))
  penalty_values = c(0, rep(nrow(design) * penalty_fraction, length(active)))
  penalty = diag(penalty_values, nrow = length(penalty_values), ncol = length(penalty_values))
  gram = crossprod(scaled_design, scaled_design * weights) + penalty
  rhs = crossprod(scaled_design, outcome * weights)
  scaled_coefficient = as.numeric(solve(gram, rhs))

  coefficient = setNames(rep(0, ncol(design)), colnames(design))
  if (length(active)) {
    active_coefficient = scaled_coefficient[-1L] / sqrt(weighted_variance[active])
    coefficient[feature_columns[active]] = active_coefficient
  }
  coefficient[[intercept]] = scaled_coefficient[[1L]] -
    sum(weighted_mean[active] * coefficient[feature_columns[active]])
  list(
    coefficients = coefficient,
    fitted = as.numeric(design %*% coefficient),
    penalty_fraction = penalty_fraction,
    effective_n = effective_n,
    n_active_terms = length(active)
  )
}

.local_crossfit_surrogate = function(design, outcome, weights, fold_id) {
  if (length(fold_id) != nrow(design)) {
    .csdg_stop("Local surrogate fold assignments must align with design rows.")
  }
  folds = sort(unique(fold_id[!is.na(fold_id)]))
  predicted = rep(NA_real_, nrow(design))
  penalty_fraction = numeric(length(folds))
  effective_n = numeric(length(folds))
  n_training = integer(length(folds))
  for (i in seq_along(folds)) {
    assessment = which(fold_id == folds[[i]])
    training = which(is.na(fold_id) | fold_id != folds[[i]])
    fit = .local_weighted_ridge(design[training, , drop = FALSE], outcome[training], weights[training])
    predicted[assessment] = as.numeric(design[assessment, , drop = FALSE] %*% fit$coefficients)
    penalty_fraction[[i]] = fit$penalty_fraction
    effective_n[[i]] = fit$effective_n
    n_training[[i]] = length(training)
  }
  list(
    predicted = predicted,
    penalty_fraction = penalty_fraction,
    effective_n = effective_n,
    n_training = n_training,
    folds = folds
  )
}

.impute_local_case_from_background = function(case, background) {
  imputed = character()
  for (nm in names(case)) {
    value = case[[nm]][[1L]]
    if (!is.na(value)) next
    observed = background[[nm]][!is.na(background[[nm]])]
    if (!length(observed)) {
      .csdg_stop("Local case feature `%s` is missing and has no observed training-background value.", nm)
    }
    replacement = if (is.numeric(background[[nm]])) {
      stats::median(observed)
    } else {
      counts = sort(table(as.character(observed)), decreasing = TRUE)
      names(counts)[[1L]]
    }
    if (is.factor(background[[nm]])) {
      replacement = factor(
        replacement,
        levels = levels(background[[nm]]),
        ordered = is.ordered(background[[nm]])
      )
    } else if (is.integer(background[[nm]])) {
      replacement = as.integer(round(replacement))
    } else if (is.logical(background[[nm]])) {
      replacement = identical(replacement, "TRUE")
    }
    data.table::set(case, j = nm, value = replacement)
    imputed = c(imputed, nm)
  }
  list(case = case, features = imputed)
}

.impute_local_neighborhood_from_background = function(neighborhood, background) {
  neighborhood = copy(.as_dt(neighborhood))
  for (nm in names(neighborhood)) {
    missing = which(is.na(neighborhood[[nm]]))
    if (!length(missing)) next
    observed = background[[nm]][!is.na(background[[nm]])]
    if (!length(observed)) {
      .csdg_stop("Local neighborhood feature `%s` has no observed training-background value.", nm)
    }
    replacement = if (is.numeric(background[[nm]])) {
      stats::median(observed)
    } else {
      counts = sort(table(as.character(observed)), decreasing = TRUE)
      names(counts)[[1L]]
    }
    if (is.factor(background[[nm]])) {
      replacement = factor(
        replacement,
        levels = levels(background[[nm]]),
        ordered = is.ordered(background[[nm]])
      )
    } else if (is.integer(background[[nm]])) {
      replacement = as.integer(round(replacement))
    } else if (is.logical(background[[nm]])) {
      replacement = identical(replacement, "TRUE")
    }
    data.table::set(neighborhood, i = missing, j = nm, value = replacement)
  }
  neighborhood
}

#' @rdname csdg_diagnostics
#' @export
csdg_local_surrogate = function(
    learner,
    task,
    cases,
    background = NULL,
    n_perturb = 500L,
    kernel_width = 0.75,
    target_scale = c("response", "link"),
    neighborhood_method = c("synthetic", "empirical_knn"),
    empirical_neighbors = n_perturb,
    seed = 20260201L,
    train_if_needed = TRUE) {
  .require_task(task)
  .require_learner(learner)
  checkmate::assert_int(n_perturb, lower = 50)
  checkmate::assert_number(kernel_width, lower = .Machine$double.eps, finite = TRUE)
  checkmate::assert_int(seed, lower = 0)
  checkmate::assert_flag(train_if_needed)
  target_scale = match.arg(target_scale)
  neighborhood_method = match.arg(neighborhood_method)
  checkmate::assert_int(empirical_neighbors, lower = 2L)
  if (identical(task$task_type, "regr") && identical(target_scale, "link")) {
    .csdg_stop("`target_scale = \"link\"` is available only for binary classification.")
  }
  checkmate::assert_true(
    checkmate::test_integerish(cases, any.missing = FALSE, min.len = 1L) ||
      checkmate::test_data_frame(cases, min.rows = 1L),
    .var.name = "cases"
  )
  if (!is.null(background)) checkmate::assert_data_frame(background, min.rows = 1L)

  had_random_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_random_seed) {
    caller_random_seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_random_seed) {
      assign(".Random.seed", caller_random_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      remove(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  fit = learner
  state = tryCatch(fit$state, error = function(e) NULL)
  if (is.null(state)) {
    if (!isTRUE(train_if_needed)) {
      .csdg_stop("`learner` is not trained.")
    }
    fit = learner$clone(deep = TRUE)
    set.seed(seed)
    fit$train(task)
  }

  features = task$feature_names
  all_data = task$data(cols = features)
  background = background %||% all_data
  background = .as_dt(background)
  if (!all(features %in% names(background))) {
    .csdg_stop("`background` must contain every task feature.")
  }
  background = background[, ..features]

  if (is.numeric(cases)) {
    case_rows = as.integer(cases)
    case_data = task$data(rows = case_rows, cols = features)
    case_ids = as.character(case_rows)
  } else {
    case_data = .as_dt(cases)
    if (!all(features %in% names(case_data))) {
      .csdg_stop("`cases` must contain every task feature.")
    }
    case_data = case_data[, ..features]
    case_ids = rownames(cases) %||% as.character(seq_len(nrow(case_data)))
  }
  if (!nrow(case_data)) .csdg_stop("At least one case is required.")

  summaries = vector("list", nrow(case_data))
  coefficients = vector("list", nrow(case_data))
  any_imputed = FALSE
  for (i in seq_len(nrow(case_data))) {
    case = case_data[i]
    case_imputation = .impute_local_case_from_background(case, background)
    case = case_imputation$case
    any_imputed = any_imputed || length(case_imputation$features) > 0L
    neighborhood = if (identical(neighborhood_method, "synthetic")) {
      .make_local_neighborhood(case, background, as.integer(n_perturb), as.integer(seed) + i - 1L)
    } else {
      .make_empirical_local_neighborhood(
        case,
        background,
        as.integer(n_perturb),
        as.integer(seed) + i - 1L,
        empirical_neighbors
      )
    }
    neighborhood = data.table::rbindlist(list(case, neighborhood), fill = TRUE)
    pred = .predict_newdata(fit, neighborhood, task = task)
    y_hat = .prediction_vector(pred, task$task_type, .task_positive(task))
    y_hat = .local_prediction_scale(y_hat, task$task_type, target_scale)
    distance = .local_distance(neighborhood, case, background)
    log_weight = -(distance^2) / max(kernel_width^2, .Machine$double.eps)
    weights = exp(pmax(log_weight, log(.Machine$double.xmin)))

    model_frame = data.frame(neighborhood, check.names = TRUE)
    design = stats::model.matrix(~ ., data = model_frame)
    evaluation_rows = seq.int(2L, nrow(neighborhood))
    case_seed = as.integer(seed) + i - 1L
    fold_id = c(NA_integer_, .local_crossfit_folds(length(evaluation_rows), case_seed + 100000L))
    crossfit = .local_crossfit_surrogate(design, y_hat, weights, fold_id)
    crossfit_metrics = .local_weighted_metrics(
      y_hat[evaluation_rows],
      crossfit$predicted[evaluation_rows],
      weights[evaluation_rows]
    )
    final_fit = .local_weighted_ridge(design, y_hat, weights)
    target_fit = .local_weighted_ridge(
      design[evaluation_rows, , drop = FALSE],
      y_hat[evaluation_rows],
      weights[evaluation_rows]
    )
    target_prediction = as.numeric(design[1L, , drop = FALSE] %*% target_fit$coefficients)
    apparent_metrics = .local_weighted_metrics(
      y_hat[evaluation_rows],
      final_fit$fitted[evaluation_rows],
      weights[evaluation_rows]
    )

    summaries[[i]] = data.table::data.table(
      case_id = case_ids[[i]],
      n_perturb = length(evaluation_rows),
      n_neighborhood = nrow(neighborhood),
      n_evaluation = length(evaluation_rows),
      n_crossfit_folds = length(crossfit$folds),
      evaluation_method = "deterministic weighted ridge cross-fitting",
      weighted_r2 = crossfit_metrics$r2,
      weighted_rmse = crossfit_metrics$rmse,
      weighted_mae = crossfit_metrics$mae,
      maximum_absolute_error = crossfit_metrics$maximum_absolute_error,
      target_case_model_prediction = y_hat[[1L]],
      target_case_surrogate_prediction = target_prediction,
      target_case_error = target_prediction - y_hat[[1L]],
      target_case_absolute_error = abs(target_prediction - y_hat[[1L]]),
      apparent_weighted_r2 = apparent_metrics$r2,
      apparent_weighted_rmse = apparent_metrics$rmse,
      effective_weight = sum(weights[evaluation_rows]),
      neighborhood_effective_weight = sum(weights),
      crossfit_training_n_min = min(crossfit$n_training),
      crossfit_training_n_max = max(crossfit$n_training),
      crossfit_effective_n_min = min(crossfit$effective_n),
      crossfit_effective_n_max = max(crossfit$effective_n),
      ridge_penalty_fraction = final_fit$penalty_fraction,
      crossfit_ridge_penalty_fraction_min = min(crossfit$penalty_fraction),
      crossfit_ridge_penalty_fraction_max = max(crossfit$penalty_fraction),
      n_design_terms = ncol(design),
      n_active_design_terms = final_fit$n_active_terms,
      maximum_distance = max(distance[evaluation_rows]),
      n_case_values_imputed = length(case_imputation$features),
      case_imputed_features = paste(case_imputation$features, collapse = ",")
    )
    coefficients[[i]] = data.table::data.table(
      case_id = case_ids[[i]],
      term = colnames(design),
      coefficient = as.numeric(final_fit$coefficients),
      fit_scope = "full-neighborhood descriptive refit after cross-fitting",
      surrogate_family = "weighted ridge linear model"
    )
  }

  list(
    summary = data.table::rbindlist(summaries),
    coefficients = data.table::rbindlist(coefficients),
    evaluation = list(
      method = "deterministic weighted ridge cross-fitting",
      folds = 5L,
      fold_assignment = "balanced seeded permutation, independently generated for each case",
      score_scope = paste(
        "Cross-fitted metrics score perturbations; target-case error is scored from a surrogate fitted",
        "without the original case"
      ),
      fit_separation = paste(
        "Each perturbation is scored by a surrogate fitted without that perturbation;",
        "the original case anchors every training fold."
      ),
      coefficient_scope = "descriptive weighted ridge refit using the original case and all perturbations",
      regularization = paste(
        "Predictors are weighted-standardized, and the ridge penalty fraction is",
        "max(1e-4, 0.01 times active design terms divided by Kish effective training size)."
      ),
      metric_fields = c(
        "weighted_r2", "weighted_rmse", "weighted_mae", "maximum_absolute_error",
        "target_case_error", "target_case_absolute_error"
      ),
      apparent_metric_fields = c("apparent_weighted_r2", "apparent_weighted_rmse")
    ),
    target_scale = if (identical(task$task_type, "classif") && identical(target_scale, "link")) {
      "positive-class logit"
    } else if (identical(task$task_type, "classif")) {
      "positive-class predicted probability"
    } else {
      "predicted outcome"
    },
    limitations = c(
      "A local surrogate assesses fidelity only in the constructed neighborhood.",
      paste(
        "Reported weighted metrics are cross-fitted over the declared perturbations;",
        "they do not measure generalization to independently observed cases."
      ),
      paste(
        "The original case anchors cross-fitted perturbation fits; its target-case error comes from",
        "a separate surrogate fitted without that case."
      ),
      sprintf(
        "The `%s` neighborhood remains conditional on its background, distance, kernel, and encoding.",
        neighborhood_method
      ),
      if (identical(neighborhood_method, "empirical_knn")) {
        paste(
          "Missing values in sampled empirical neighbors were completed with fold-training medians or modes",
          "before prediction and surrogate encoding."
        )
      },
      "The additive surrogate and its deterministic ridge rule can understate sharp nonlinear local behavior.",
      if (any_imputed) {
        paste(
          "Missing case features were filled from the corresponding training background",
          "before neighborhood construction; affected feature names are recorded."
        )
      },
      "Local fidelity does not make an explanation causal or actionable."
    )
  )
}


#' @rdname csdg_diagnostics
#' @export
csdg_oof_local_surrogate = function(
    x,
    cases,
    n_perturb = 500L,
    kernel_width = 0.75,
    target_scale = c("response", "link"),
    neighborhood_method = c("synthetic", "empirical_knn"),
    empirical_neighbors = n_perturb,
    seed = 20260201L,
    selection = c("prespecified", "post_hoc_communication")) {
  checkmate::assert_class(x, "CSDGResample")
  if (is.null(x$models)) {
    .csdg_stop("Fold models are required for held-out local diagnostics.")
  }
  checkmate::assert_character(selection, any.missing = FALSE, min.len = 1L)
  selection = match.arg(selection)
  checkmate::assert_integerish(cases, any.missing = FALSE, min.len = 1L, unique = TRUE)
  checkmate::assert_int(n_perturb, lower = 50)
  checkmate::assert_number(kernel_width, lower = .Machine$double.eps, finite = TRUE)
  checkmate::assert_int(seed, lower = 0)
  target_scale = match.arg(target_scale)
  neighborhood_method = match.arg(neighborhood_method)
  checkmate::assert_int(empirical_neighbors, lower = 2L)
  cases = as.integer(cases)
  unknown = setdiff(cases, x$task$row_ids)
  if (length(unknown)) {
    .csdg_stop("Unknown case row ids: %s.", paste(unknown, collapse = ", "))
  }

  records = list()
  coefficients = list()
  evaluation = NULL
  target_scale_label = NULL
  k = 0L
  for (i in seq_along(x$models)) {
    held_out = intersect(cases, x$test_sets[[i]])
    if (!length(held_out)) next
    background = x$task$data(
      rows = x$train_sets[[i]],
      cols = x$task$feature_names
    )
    for (row in held_out) {
      k = k + 1L
      case_data = x$task$data(rows = row, cols = x$task$feature_names)
      local = csdg_local_surrogate(
        learner = x$models[[i]],
        task = x$task,
        cases = case_data,
        background = background,
        n_perturb = n_perturb,
        kernel_width = kernel_width,
        target_scale = target_scale,
        neighborhood_method = neighborhood_method,
        empirical_neighbors = empirical_neighbors,
        seed = as.integer(seed) + i * 10000L + as.integer(row),
        train_if_needed = FALSE
      )
      local$summary[, `:=`(
        case_id = as.character(row),
        iteration = i,
        repetition = x$fold_scores$repetition[[i]],
        fold = x$fold_scores$fold[[i]],
        case_selection = selection
      )]
      local$coefficients[, `:=`(
        case_id = as.character(row),
        iteration = i,
        repetition = x$fold_scores$repetition[[i]],
        fold = x$fold_scores$fold[[i]]
      )]
      records[[k]] = local$summary
      coefficients[[k]] = local$coefficients
      evaluation = local$evaluation
      target_scale_label = local$target_scale
    }
  }
  if (!length(records)) {
    .csdg_stop("None of the requested cases occurred in an assessment split.")
  }
  requested_case_ids = as.character(cases)
  summary = data.table::rbindlist(records, fill = TRUE)
  coefficient_table = data.table::rbindlist(coefficients, fill = TRUE)
  if (!setequal(unique(summary$case_id), requested_case_ids) ||
      !setequal(unique(coefficient_table$case_id), requested_case_ids)) {
    .csdg_stop("The held-out local diagnostics did not preserve the requested case set.")
  }
  order_column = "requested_case_order__"
  data.table::set(summary, j = order_column, value = match(summary$case_id, requested_case_ids))
  data.table::set(
    coefficient_table,
    j = order_column,
    value = match(coefficient_table$case_id, requested_case_ids)
  )
  data.table::setorderv(summary, c(order_column, "iteration", "repetition", "fold"))
  data.table::setorderv(coefficient_table, c(order_column, "iteration", "repetition", "fold"))
  data.table::set(summary, j = order_column, value = NULL)
  data.table::set(coefficient_table, j = order_column, value = NULL)
  list(
    summary = summary,
    coefficients = coefficient_table,
    selection = selection,
    evaluation = evaluation,
    target_scale = target_scale_label,
    limitations = c(
      "Each diagnostic uses a fold model that did not train on the evaluated case.",
      paste(
        "Each perturbation is scored by a cross-fitted surrogate that did not fit that perturbation;",
        "target-case error is obtained from a surrogate fitted without the original held-out case."
      ),
      "Neighborhoods are generated from the corresponding fold-training data and remain perturbation-dependent.",
      "Returned coefficients are descriptive full-neighborhood ridge refits, not cross-fitted coefficients.",
      if (identical(selection, "post_hoc_communication")) {
        paste(
          "Cases selected after inspecting out-of-fold results are communication examples",
          "and cannot establish local fidelity in general."
        )
      }
    )
  )
}
