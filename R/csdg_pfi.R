
.permute_within = function(x, strata = NULL) {
  if (is.null(strata)) return(sample(x, length(x), replace = FALSE))
  out = x
  idx = split(seq_along(x), as.character(strata), drop = TRUE)
  for (ii in idx) out[ii] = sample(x[ii], length(ii), replace = FALSE)
  out
}

.cluster_permutation_index = function(cluster, strata = NULL) {
  cluster = as.character(cluster)
  strata = if (is.null(strata)) rep("all", length(cluster)) else as.character(strata)
  cluster_table = unique(data.table(row = seq_along(cluster), cluster = cluster, stratum = strata), by = "cluster")
  nesting = unique(data.table(cluster = cluster, stratum = strata))[, .(n_strata = uniqueN(stratum)), by = cluster]
  if (any(nesting$n_strata > 1L)) {
    .csdg_stop("Every `cluster` must be nested in exactly one permutation stratum.")
  }
  cluster_table[, donor_cluster := sample(cluster, .N, replace = FALSE), by = stratum]
  donor_row = stats::setNames(cluster_table$row, cluster_table$cluster)
  donor_cluster = stats::setNames(cluster_table$donor_cluster, cluster_table$cluster)
  as.integer(donor_row[donor_cluster[cluster]])
}

.normalize_feature_groups = function(features, feature_groups) {
  if (is.null(feature_groups)) {
    return(stats::setNames(lapply(features, function(x) x), features))
  }
  checkmate::assert_list(feature_groups, .var.name = "feature_groups")
  if (is.null(names(feature_groups)) || any(!nzchar(names(feature_groups))) || anyDuplicated(names(feature_groups))) {
    .csdg_stop("`feature_groups` must be a named list.")
  }
  checkmate::assert_true(
    all(vapply(feature_groups, function(x) {
      checkmate::test_character(x, any.missing = FALSE, min.len = 1L)
    }, logical(1L))),
    .var.name = "feature_groups"
  )
  feature_groups = lapply(feature_groups, unique)
  unknown = setdiff(unique(unlist(feature_groups, use.names = FALSE)), features)
  if (length(unknown)) {
    .csdg_stop("Unknown grouped features: %s.", paste(unknown, collapse = ", "))
  }
  feature_groups
}

.pfi_max_batch_rows = 100000L

.pfi_effective_batch_size = function(batch_size, n_assessment) {
  max(1L, min(batch_size, .pfi_max_batch_rows %/% n_assessment))
}

#' @rdname csdg_diagnostics
#' @export
csdg_fold_pfi = function(
    x,
    task = NULL,
    features = NULL,
    feature_groups = NULL,
    loss = NULL,
    repetitions = 10L,
    strata = NULL,
    cluster = NULL,
    cluster_level_groups = NULL,
    batch_size = 1L,
    seed = 20260201L) {
  checkmate::assert_class(x, "CSDGResample")
  if (is.null(x$models)) {
    .csdg_stop("Fold models were not stored; rerun csdg_resample(store_models = TRUE).")
  }
  task = task %||% x$task
  .require_task(task)
  checkmate::assert_int(repetitions, lower = 1)
  checkmate::assert_int(batch_size, lower = 1)
  checkmate::assert_int(seed, lower = 0)
  if (!is.null(loss)) checkmate::assert_string(loss, min.chars = 1L)
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
  features = features %||% task$feature_names
  checkmate::assert_character(features, any.missing = FALSE, min.len = 1L, unique = TRUE)
  missing = setdiff(features, task$feature_names)
  if (length(missing)) {
    .csdg_stop("Unknown task features: %s.", paste(missing, collapse = ", "))
  }
  groups = .normalize_feature_groups(features, feature_groups)
  strata_all = .normalize_named_vector(strata, task$row_ids, "strata")
  if (!is.null(strata_all)) names(strata_all) = as.character(task$row_ids)
  cluster_all = .normalize_named_vector(cluster, task$row_ids, "cluster")
  if (!is.null(cluster_all)) names(cluster_all) = as.character(task$row_ids)
  cluster_level_groups = cluster_level_groups %||% character()
  checkmate::assert_character(
    cluster_level_groups,
    any.missing = FALSE,
    unique = TRUE,
    .var.name = "cluster_level_groups"
  )
  unknown_cluster_groups = setdiff(cluster_level_groups, names(groups))
  if (length(unknown_cluster_groups)) {
    .csdg_stop("Unknown `cluster_level_groups`: %s.", paste(unknown_cluster_groups, collapse = ", "))
  }
  if (length(cluster_level_groups) && is.null(cluster_all)) {
    .csdg_stop("`cluster` is required when `cluster_level_groups` is supplied.")
  }
  loss = loss %||% if (identical(task$task_type, "regr")) "rmse" else "logloss"

  records = vector("list", length(x$models) * length(groups) * repetitions)
  effective_batch_sizes = integer(length(x$models))
  effective_batch_rows = integer(length(x$models))
  counter = 0L
  for (i in seq_along(x$models)) {
    model = x$models[[i]]
    test_rows = x$test_sets[[i]]
    dat = task$data(
      rows = test_rows,
      cols = c(task$feature_names, task$target_names)
    )
    truth = dat[[task$target_names[[1L]]]]
    feature_names = task$feature_names
    newdata = dat[, ..feature_names]
    base_pred = .predict_newdata(model, newdata, task = task)
    base_vec = .prediction_vector(base_pred, task$task_type, x$positive)
    base_loss = .compute_loss(
      truth, base_vec, task$task_type, loss, positive = x$positive
    )
    fold_strata = if (is.null(strata_all)) NULL else {
      unname(strata_all[as.character(test_rows)])
    }
    fold_cluster = if (is.null(cluster_all)) NULL else {
      unname(cluster_all[as.character(test_rows)])
    }

    requests = data.table::data.table(
      group_index = rep(seq_along(groups), each = repetitions),
      permutation_repetition = rep(seq_len(repetitions), times = length(groups))
    )
    effective_batch_size = .pfi_effective_batch_size(batch_size, nrow(newdata))
    effective_batch_sizes[[i]] = effective_batch_size
    effective_batch_rows[[i]] = min(effective_batch_size, nrow(requests)) * nrow(newdata)
    batch_starts = seq.int(1L, nrow(requests), by = effective_batch_size)
    for (batch_start in batch_starts) {
      batch_end = min(batch_start + effective_batch_size - 1L, nrow(requests))
      batch_requests = requests[batch_start:batch_end]
      permuted_data = vector("list", nrow(batch_requests))
      for (request_index in seq_len(nrow(batch_requests))) {
        g = batch_requests$group_index[[request_index]]
        b = batch_requests$permutation_repetition[[request_index]]
        group_name = names(groups)[[g]]
        group_features = groups[[g]]
        set.seed(as.integer(seed) + i * 100000L + g * 1000L + b)
        perm = data.table::copy(newdata)
        permutation_index = seq_len(nrow(perm))
        if (group_name %in% cluster_level_groups) {
          permutation_index = .cluster_permutation_index(fold_cluster, fold_strata)
          for (feature in group_features) {
            cluster_variation = data.table(value = newdata[[feature]], cluster = fold_cluster)[
              , uniqueN(value), by = cluster
            ]
            if (any(cluster_variation$V1 > 1L)) {
              .csdg_stop(
                "Feature `%s` is not constant within every assessment cluster for group `%s`.",
                feature,
                group_name
              )
            }
          }
        } else if (is.null(fold_strata)) {
          permutation_index = sample(permutation_index)
        } else {
          blocks = split(permutation_index, as.character(fold_strata), drop = TRUE)
          # Preserve row order while permuting donors separately in each block.
          permuted_by_block = seq_len(nrow(perm))
          for (block in blocks) {
            permuted_by_block[block] = block[sample.int(length(block))]
          }
          permutation_index = permuted_by_block
        }
        for (feature in group_features) {
          perm[[feature]] = newdata[[feature]][permutation_index]
        }
        permuted_data[[request_index]] = perm
      }
      batch_data = data.table::rbindlist(permuted_data, use.names = TRUE)
      batch_prediction = .predict_newdata(model, batch_data, task = task)
      batch_vector = .prediction_vector(batch_prediction, task$task_type, x$positive)
      expected_predictions = nrow(newdata) * nrow(batch_requests)
      if (length(batch_vector) != expected_predictions) {
        .csdg_stop(
          "Batched PFI prediction returned %d values; expected %d.",
          length(batch_vector), expected_predictions
        )
      }
      for (request_index in seq_len(nrow(batch_requests))) {
        counter = counter + 1L
        g = batch_requests$group_index[[request_index]]
        b = batch_requests$permutation_repetition[[request_index]]
        group_name = names(groups)[[g]]
        group_features = groups[[g]]
        prediction_start = (request_index - 1L) * nrow(newdata) + 1L
        prediction_end = request_index * nrow(newdata)
        perm_vec = batch_vector[prediction_start:prediction_end]
        perm_loss = .compute_loss(
          truth, perm_vec, task$task_type, loss, positive = x$positive
        )
        records[[counter]] = data.table::data.table(
          iteration = i,
          repetition = x$fold_scores$repetition[[i]],
          fold = x$fold_scores$fold[[i]],
          feature_group = group_name,
          features = paste(group_features, collapse = "|"),
          permutation_repetition = b,
          n_assessment = length(test_rows),
          loss = loss,
          baseline_loss = base_loss,
          permuted_loss = perm_loss,
          importance = perm_loss - base_loss
        )
      }
    }
  }
  per_iteration = data.table::rbindlist(records[seq_len(counter)], fill = TRUE)
  fold_level = per_iteration[, .(
    importance = mean(importance, na.rm = TRUE),
    monte_carlo_sd = if (.N > 1L) stats::sd(importance, na.rm = TRUE) else NA_real_,
    monte_carlo_se = if (.N > 1L) stats::sd(importance, na.rm = TRUE) / sqrt(.N) else NA_real_,
    n_permutations = .N,
    baseline_loss = mean(baseline_loss, na.rm = TRUE),
    permuted_loss = mean(permuted_loss, na.rm = TRUE),
    n_assessment = max(n_assessment)
  ), by = .(iteration, repetition, fold, feature_group, features, loss)]
  summary = fold_level[, .(
    n_iterations = sum(is.finite(importance)),
    mean_importance = mean(importance, na.rm = TRUE),
    sd_importance = stats::sd(importance, na.rm = TRUE),
    median_importance = stats::median(importance, na.rm = TRUE),
    q10_importance = stats::quantile(importance, 0.10, na.rm = TRUE, names = FALSE),
    q90_importance = stats::quantile(importance, 0.90, na.rm = TRUE, names = FALSE),
    positive_fraction = mean(importance > 0, na.rm = TRUE),
    mean_monte_carlo_se = if (any(is.finite(monte_carlo_se))) {
      mean(monte_carlo_se[is.finite(monte_carlo_se)])
    } else {
      NA_real_
    },
    maximum_monte_carlo_se = if (any(is.finite(monte_carlo_se))) {
      max(monte_carlo_se[is.finite(monte_carlo_se)])
    } else {
      NA_real_
    }
  ), by = .(feature_group, features, loss)]
  summary[, rank := data.table::frank(-mean_importance, ties.method = "average")]
  data.table::setorder(summary, rank, feature_group)
  summary[, uncertainty_label :=
            "descriptive variation across held-out folds; not a confidence interval"]

  list(
    raw = per_iteration,
    per_iteration = fold_level,
    summary = summary,
    perturbation = list(
      type = "marginal permutation",
      within_strata = !is.null(strata),
      cluster_aware = length(cluster_level_groups) > 0L,
      cluster_level_groups = cluster_level_groups,
      repetitions = as.integer(repetitions),
      requested_prediction_batch_size = as.integer(batch_size),
      effective_prediction_batch_sizes = sort(unique(effective_batch_sizes)),
      batch_row_target = .pfi_max_batch_rows,
      observed_stacked_batch_rows = sort(unique(effective_batch_rows)),
      loss = loss
    ),
    limitations = c(
      "Permutation importance is prediction- and perturbation-distribution specific.",
      "Correlated or substitutable variables can redistribute importance.",
      paste(
        "Prediction batching above one assumes deterministic prediction for a fixed trained learner and row;",
        "use batch_size = 1 for learners with stochastic prediction behavior."
      ),
      "Fold variation is descriptive and does not constitute independent-sample uncertainty."
    )
  )
}
