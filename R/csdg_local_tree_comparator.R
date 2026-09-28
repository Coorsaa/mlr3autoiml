.local_tree_fit = function(design, outcome, weights, control) {
  feature_columns = setdiff(colnames(design), "(Intercept)")
  predictors = data.frame(design[, feature_columns, drop = FALSE], check.names = TRUE)
  model_data = data.frame(local_target__ = as.numeric(outcome), predictors, check.names = TRUE)
  fit = rpart::rpart(
    local_target__ ~ .,
    data = model_data,
    weights = as.numeric(weights),
    method = "anova",
    control = control,
    model = FALSE,
    x = FALSE,
    y = FALSE
  )
  list(fit = fit, feature_columns = names(predictors))
}

.local_tree_predict = function(object, design) {
  feature_columns = setdiff(colnames(design), "(Intercept)")
  predictors = data.frame(design[, feature_columns, drop = FALSE], check.names = TRUE)
  missing = setdiff(object$feature_columns, names(predictors))
  if (length(missing)) {
    .csdg_stop("Tree-comparator prediction data are missing design columns: %s.", paste(missing, collapse = ", "))
  }
  as.numeric(stats::predict(object$fit, newdata = predictors[, object$feature_columns, drop = FALSE]))
}

.local_tree_crossfit = function(design, outcome, weights, fold_id, control) {
  folds = sort(unique(fold_id[!is.na(fold_id)]))
  prediction = rep(NA_real_, nrow(design))
  for (fold in folds) {
    assessment = which(fold_id == fold)
    training = which(is.na(fold_id) | fold_id != fold)
    fit = .local_tree_fit(design[training, , drop = FALSE], outcome[training], weights[training], control)
    prediction[assessment] = .local_tree_predict(fit, design[assessment, , drop = FALSE])
  }
  prediction
}

#' Compare additive local fidelity with a nonlinear tree surrogate
#'
#' Evaluates a pre-pruned regression-tree surrogate on the same held-out cases, model-output scale, neighborhoods,
#' locality weights, perturbation seeds, and cross-fit partitions used by [csdg_local_fidelity_audit()].
#' The result helps distinguish limitations of an additive ridge surrogate from limitations shared by a nonlinear
#' local approximation under the declared neighborhood design.
#' The tree is fitted with `rpart::rpart(method = "anova")` to the same points, kernel weights, and cross-fit folds as
#' the ridge surrogate (section "Local surrogate"), using the non-intercept columns of the same design matrix as
#' predictors; the default control is `rpart::rpart.control(minsplit = 20, minbucket = 7, cp = 0.001,
#' maxdepth = 6, xval = 0)`.
#'
#' @inheritSection csdg_diagnostics Local surrogate
#' @param x A `CSDGResample` with stored fold models.
#' @param cases Unique task row ids that each occur in exactly one assessment split.
#' @param seeds Either a vector of at least two perturbation seeds or a case-by-replicate matrix.
#' @param crossfit_seeds Optional vector or matrix of cross-fit partition seeds.
#' @param kernel_width Positive locality-kernel width.
#' @param n_perturb Number of perturbations per case and seed.
#' @param crossfit_folds Number of surrogate cross-fitting folds.
#' @param target_scale For classification, either `"response"` for probability or `"link"` for logit scale.
#' @param neighborhood_method Either `"synthetic"` or `"empirical_knn"`.
#' @param empirical_neighbors Number of nearest training rows eligible for empirical-neighbor resampling.
#' @param case_labels Optional unique pseudonymous labels in the same order as `cases`.
#' @param case_metadata Optional data frame copied to the replicate and case summaries.
#' @param control Optional `rpart.control` object; the default is given in the description.
#'
#' @return A list with replicate-level and case-level fidelity metrics, variable-importance stability, method metadata,
#'   and limitations.
#' @export
csdg_local_tree_comparator = function(
    x,
    cases,
    seeds = 20260201L + 0:19,
    crossfit_seeds = NULL,
    kernel_width = 0.75,
    n_perturb = 500L,
    crossfit_folds = 5L,
    target_scale = c("response", "link"),
    neighborhood_method = c("synthetic", "empirical_knn"),
    empirical_neighbors = n_perturb,
    case_labels = NULL,
    case_metadata = NULL,
    control = NULL) {
  assert_class(x, "CSDGResample", .var.name = "x")
  assert_number(kernel_width, lower = .Machine$double.eps, finite = TRUE)
  assert_int(n_perturb, lower = 50L)
  assert_int(crossfit_folds, lower = 2L, upper = n_perturb)
  assert_int(empirical_neighbors, lower = 2L)
  target_scale = match.arg(target_scale)
  neighborhood_method = match.arg(neighborhood_method)
  if (identical(x$task_type, "regr") && identical(target_scale, "link")) {
    .csdg_stop("`target_scale = \"link\"` is available only for binary classification.")
  }
  if (!requireNamespace("rpart", quietly = TRUE)) {
    .csdg_stop("Install the suggested package `rpart` to use the nonlinear local comparator.")
  }
  if (is.null(control)) {
    control = rpart::rpart.control(
      minsplit = 20L,
      minbucket = 7L,
      cp = 0.001,
      maxdepth = 6L,
      xval = 0L
    )
  }
  assert_list(control, names = "named", .var.name = "control")

  case_map = .local_fidelity_prepare_cases(x, cases, case_labels, case_metadata)
  seed_matrix = .local_fidelity_seed_matrix(seeds, nrow(case_map))
  crossfit_seed_matrix = if (is.null(crossfit_seeds)) {
    seed_matrix + 100000L
  } else {
    .local_fidelity_seed_matrix(crossfit_seeds, nrow(case_map))
  }
  if (!identical(dim(seed_matrix), dim(crossfit_seed_matrix))) {
    .csdg_stop("`crossfit_seeds` must provide the same case-by-replicate grid as `seeds`.")
  }

  evaluated = lapply(seq_len(nrow(case_map)), function(case_index) {
    map = case_map[case_index]
    model_index = map$iteration[[1L]]
    case_data = .local_fidelity_case_data(x, map)
    public_case = map[, setdiff(names(map), "row_id"), with = FALSE]
    records = lapply(seq_len(ncol(seed_matrix)), function(replicate) {
      perturbation_seed = seed_matrix[case_index, replicate]
      crossfit_seed = crossfit_seed_matrix[case_index, replicate]
      neighborhood = if (identical(neighborhood_method, "synthetic")) {
        .make_local_neighborhood(case_data$case, case_data$background, n_perturb, perturbation_seed)
      } else {
        .make_empirical_local_neighborhood(
          case_data$case,
          case_data$background,
          n_perturb,
          perturbation_seed,
          empirical_neighbors
        )
      }
      neighborhood = rbindlist(list(case_data$case, neighborhood), fill = TRUE)
      model_prediction = .prediction_vector(
        .predict_newdata(x$models[[model_index]], neighborhood, task = x$task),
        x$task$task_type,
        x$task$positive
      )
      model_prediction = .local_prediction_scale(model_prediction, x$task_type, target_scale)
      design = model.matrix(~ ., data = data.frame(neighborhood, check.names = TRUE))
      distance = .local_distance(neighborhood, case_data$case, case_data$background)
      weights = exp(pmax(-(distance^2) / kernel_width^2, log(.Machine$double.xmin)))
      evaluation_rows = seq.int(2L, nrow(neighborhood))
      fold_id = c(
        NA_integer_,
        .local_crossfit_folds(length(evaluation_rows), crossfit_seed, folds = crossfit_folds)
      )
      crossfit_prediction = .local_tree_crossfit(design, model_prediction, weights, fold_id, control)
      metrics = .local_weighted_metrics(
        model_prediction[evaluation_rows],
        crossfit_prediction[evaluation_rows],
        weights[evaluation_rows]
      )
      target_fit = .local_tree_fit(
        design[evaluation_rows, , drop = FALSE],
        model_prediction[evaluation_rows],
        weights[evaluation_rows],
        control
      )
      target_prediction = .local_tree_predict(target_fit, design[1L, , drop = FALSE])[[1L]]
      full_fit = .local_tree_fit(design, model_prediction, weights, control)
      variable_names = setdiff(colnames(design), "(Intercept)")
      importance = full_fit$fit$variable.importance %||% numeric()
      importance_table = data.table(
        term = variable_names,
        importance = as.numeric(importance[match(variable_names, names(importance))])
      )
      importance_table[is.na(importance), importance := 0]
      importance_total = sum(importance_table$importance)
      importance_table[, normalized_importance := if (importance_total > 0) importance / importance_total else 0]
      list(
        replicate = cbind(public_case, data.table(
          perturbation_replicate = as.integer(replicate),
          perturbation_seed = as.integer(perturbation_seed),
          crossfit_seed = as.integer(crossfit_seed),
          kernel_width = kernel_width,
          target_scale = target_scale,
          neighborhood_method = neighborhood_method,
          weighted_r2 = metrics$r2,
          weighted_rmse = metrics$rmse,
          weighted_mae = metrics$mae,
          maximum_absolute_error = metrics$maximum_absolute_error,
          target_case_model_prediction = model_prediction[[1L]],
          target_case_surrogate_prediction = target_prediction,
          target_case_error = target_prediction - model_prediction[[1L]],
          target_case_absolute_error = abs(target_prediction - model_prediction[[1L]])
        )),
        importance = cbind(
          public_case,
          data.table(perturbation_replicate = as.integer(replicate)),
          importance_table
        )
      )
    })
    list(
      replicates = rbindlist(lapply(records, `[[`, "replicate")),
      importance = rbindlist(lapply(records, `[[`, "importance"))
    )
  })
  replicates = rbindlist(lapply(evaluated, `[[`, "replicates"))
  importance = rbindlist(lapply(evaluated, `[[`, "importance"))
  cases = replicates[, .(
    n_perturbation_replicates = .N,
    median_weighted_r2 = median(weighted_r2),
    median_weighted_rmse = median(weighted_rmse),
    median_weighted_mae = median(weighted_mae),
    median_maximum_absolute_error = median(maximum_absolute_error),
    median_target_case_absolute_error = median(target_case_absolute_error),
    monte_carlo_sd_weighted_r2 = sd(weighted_r2),
    monte_carlo_se_weighted_r2 = sd(weighted_r2) / sqrt(.N)
  ), by = .(case_label, iteration, repetition, fold)]
  importance_stability = importance[, .(
    median_normalized_importance = median(normalized_importance),
    q10_normalized_importance = quantile(normalized_importance, 0.10, names = FALSE, type = 8),
    q90_normalized_importance = quantile(normalized_importance, 0.90, names = FALSE, type = 8)
  ), by = .(case_label, iteration, repetition, fold, term)]

  list(
    replicates = replicates[],
    cases = cases[],
    importance_stability = importance_stability[],
    evaluation = list(
      surrogate_family = "pre-pruned regression tree",
      crossfit_folds = crossfit_folds,
      target_scale = target_scale,
      neighborhood_method = neighborhood_method,
      control = unclass(control)
    ),
    limitations = c(
      "The tree is a diagnostic comparator, not a universally optimal local explanation.",
      "Its result remains conditional on the declared neighborhood, distance, kernel, output scale, and controls.",
      if (identical(neighborhood_method, "empirical_knn")) {
        paste(
          "Missing values in sampled empirical neighbors were completed with fold-training medians or modes",
          "before prediction and surrogate encoding."
        )
      },
      "Cross-fitted perturbation error does not estimate performance on independently observed cases.",
      "Local fidelity does not establish causal meaning, feasibility, or actionability."
    )
  )
}
