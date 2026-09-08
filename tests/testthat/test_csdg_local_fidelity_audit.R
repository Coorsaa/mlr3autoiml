test_that("systematic local-fidelity audit produces a complete deterministic grid", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task,
    fx$learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  cases = c(3L, 28L)
  seeds = matrix(c(9L, 10L, 19L, 20L), nrow = 2L, byrow = TRUE)
  arguments = list(
    x = out,
    cases = cases,
    seeds = seeds,
    kernel_widths = c(0.50, 0.75, 1.00),
    n_perturb = 100L,
    fidelity_threshold = 0.80,
    case_labels = c("case_007", "case_009"),
    case_metadata = data.frame(prediction_score_decile = c(2L, 8L))
  )

  set.seed(301L)
  before = .Random.seed
  first = do.call(csdg_local_fidelity_audit, arguments)
  after = .Random.seed
  second = do.call(csdg_local_fidelity_audit, arguments)
  support = csdg_local_support(
    out,
    cases = cases,
    kernel_width = 0.75,
    case_labels = c("case_007", "case_009"),
    case_metadata = data.frame(prediction_score_decile = c(2L, 8L))
  )

  expect_identical(after, before)
  expect_identical(first, second)
  expect_identical(first$support, support)
  expect_equal(nrow(first$replicates), 12L)
  expect_equal(nrow(first$support), 2L)
  expect_equal(nrow(first$bandwidth_cases), 6L)
  expect_equal(nrow(first$summary), 5L)
  expect_equal(
    data.table::uniqueN(first$replicates, by = c("case_label", "perturbation_replicate", "kernel_width")),
    12L
  )
  expect_identical(first$case_map$row_id, cases)
  expect_identical(first$case_map$iteration, c(1L, 3L))
  expect_identical(first$case_map$prediction_score_decile, c(2L, 8L))
  expect_identical(unique(first$replicates$case_label), c("case_007", "case_009"))
  expect_identical(first$replicates$perturbation_seed, rep(as.vector(t(seeds)), each = 3L))
  expect_true(all(first$replicates$n_crossfit_folds == 5L))
  expect_true(all(is.finite(first$replicates$weighted_r2)))
  expect_true(all(is.finite(first$replicates$weighted_rmse)))
  expect_true(all(first$support$support_kernel_width == 0.75))
  expect_identical(first$support$prediction_score_decile, c(2L, 8L))
  expect_identical(first$bandwidth_cases$prediction_score_decile, rep(c(2L, 8L), each = 3L))
  expect_equal(first$support$n_training_background, vapply(c(1L, 3L), function(i) {
    length(out$train_sets[[i]])
  }, integer(1L)))
  expect_false(any(vapply(seq_len(nrow(first$case_map)), function(i) {
    first$case_map$row_id[[i]] %in% out$train_sets[[first$case_map$iteration[[i]]]]
  }, logical(1L))))

  primary = first$replicates[analysis_role == "primary"]
  expected_case_medians = primary[, .(
    median_weighted_r2 = median(weighted_r2),
    median_weighted_rmse = median(weighted_rmse)
  ), by = case_label]
  observed_case_medians = first$cases[, .(case_label, median_weighted_r2, median_weighted_rmse)]
  expect_equal(observed_case_medians, expected_case_medians)
  primary_summary = first$summary[scope == "primary case medians"]
  expect_equal(primary_summary$first_quartile, stats::quantile(
    expected_case_medians$median_weighted_r2,
    0.25,
    names = FALSE,
    type = 8
  ))
  expect_equal(primary_summary$n_below_threshold, sum(expected_case_medians$median_weighted_r2 < 0.80))
  expect_equal(first$summary[scope == "bandwidth-sensitivity case medians", n_values], c(2L, 2L))
})

test_that("systematic local-fidelity audit matches the existing local-surrogate algorithm", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task,
    fx$learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  row_id = 3L
  model_index = which(vapply(out$test_sets, function(rows) row_id %in% rows, logical(1L)))
  background = out$task$data(rows = out$train_sets[[model_index]], cols = out$task$feature_names)
  case = out$task$data(rows = row_id, cols = out$task$feature_names)
  audited = csdg_local_fidelity_audit(
    out,
    cases = row_id,
    seeds = c(9L, 10L),
    kernel_widths = 0.75,
    n_perturb = 100L
  )
  common = c(
    "n_perturb", "n_crossfit_folds", "weighted_r2", "weighted_rmse", "apparent_weighted_r2",
    "apparent_weighted_rmse", "crossfit_effective_n_min", "crossfit_effective_n_max",
    "n_design_terms", "n_active_design_terms", "n_case_values_imputed"
  )

  for (seed in c(9L, 10L)) {
    existing = csdg_local_surrogate(
      learner = out$models[[model_index]],
      task = out$task,
      cases = case,
      background = background,
      n_perturb = 100L,
      kernel_width = 0.75,
      seed = seed,
      train_if_needed = FALSE
    )
    observed = audited$replicates[perturbation_seed == seed]
    expect_equal(observed[, ..common], existing$summary[, ..common], tolerance = 0)
  }

  imputed_case = .impute_local_case_from_background(data.table::as.data.table(case), background)$case
  distance = sort(.local_distance(background, imputed_case, background))
  weights = exp(pmax(-(distance^2) / 0.75^2, log(.Machine$double.xmin)))
  expect_equal(audited$support$nearest_training_distance, distance[[1L]])
  expect_equal(audited$support$tenth_nearest_training_distance, distance[[10L]])
  expect_equal(audited$support$fiftieth_nearest_training_distance, distance[[50L]])
  expect_equal(audited$support$proportion_training_within_kernel, mean(distance <= 0.75))
  expect_equal(audited$support$kernel_weight_effective_n, sum(weights)^2 / sum(weights^2))
})

test_that("fold-training support imputes a missing held-out case value", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  fx$data$x1[[1L]] = NA_real_
  task = mlr3::as_task_classif(
    fx$data[, setdiff(names(fx$data), "cluster")],
    target = "y",
    positive = "1"
  )
  out = csdg_resample(
    task,
    mlr3::lrn("classif.rpart", predict_type = "prob"),
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 18L
  )

  support = csdg_local_support(out, cases = 1L)

  expect_equal(support$n_case_values_imputed, 1L)
  expect_identical(support$case_imputed_features, "x1")
  expect_true(all(is.finite(support$nearest_training_distance)))
  expect_true(all(is.finite(support$kernel_weight_effective_n)))
})

test_that("empirical local neighborhoods complete sampled training-row missingness", {
  skip_if_not_installed("rpart")
  background = data.table::data.table(
    numeric_feature = c(NA_real_, 1, 3),
    integer_feature = c(NA_integer_, 2L, 4L),
    factor_feature = factor(c(NA_character_, "a", "b"), levels = c("a", "b")),
    logical_feature = c(NA, TRUE, FALSE)
  )
  case = background[2L]
  neighborhood = .make_empirical_local_neighborhood(
    case,
    background,
    n = 100L,
    seed = 17L,
    neighbors = 3L
  )

  expect_false(anyNA(neighborhood))
  expect_type(neighborhood$numeric_feature, "double")
  expect_type(neighborhood$integer_feature, "integer")
  expect_s3_class(neighborhood$factor_feature, "factor")
  expect_type(neighborhood$logical_feature, "logical")
  expect_true(all(neighborhood$numeric_feature %in% c(1, 2, 3)))
  expect_true(all(neighborhood$integer_feature %in% c(2L, 3L, 4L)))

  all_missing = data.table::copy(background)
  all_missing[, numeric_feature := NA_real_]
  expect_error(
    .make_empirical_local_neighborhood(case, all_missing, n = 50L, seed = 17L, neighbors = 3L),
    "no observed training-background value"
  )
})

test_that("systematic empirical-neighborhood fidelity handles incomplete training rows", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  fx$data$x1[seq.int(1L, 90L, by = 3L)] = NA_real_
  task = mlr3::as_task_classif(
    fx$data[, setdiff(names(fx$data), "cluster")],
    target = "y",
    positive = "1"
  )
  out = csdg_resample(
    task,
    mlr3::lrn("classif.rpart", predict_type = "prob"),
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  result = csdg_local_fidelity_audit(
    out,
    cases = c(3L, 28L),
    seeds = c(9L, 10L),
    kernel_widths = 0.75,
    n_perturb = 100L,
    neighborhood_method = "empirical_knn",
    empirical_neighbors = 60L
  )

  expect_equal(nrow(result$replicates), 4L)
  expect_true(all(is.finite(result$replicates$weighted_r2)))
  expect_match(paste(result$limitations, collapse = " "), "fold-training medians or modes")
})

test_that("systematic local-fidelity audit rejects ambiguous or malformed designs", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task,
    fx$learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  arguments = list(
    x = out,
    cases = 3L,
    seeds = c(9L, 10L),
    kernel_widths = 0.75,
    n_perturb = 100L,
    fidelity_threshold = 0.80
  )

  no_models = out
  no_models$models = NULL
  no_model_arguments = arguments
  no_model_arguments$x = no_models
  expect_error(do.call(csdg_local_fidelity_audit, no_model_arguments), "Stored fold models")
  expect_error(csdg_local_support(no_models, cases = 3L), "Stored fold models")
  expect_error(do.call(csdg_local_fidelity_audit, modifyList(arguments, list(cases = c(3L, 3L)))), "duplicated")
  expect_error(do.call(csdg_local_fidelity_audit, modifyList(arguments, list(cases = 999L))), "Unknown case")
  expect_error(
    do.call(csdg_local_fidelity_audit, modifyList(arguments, list(kernel_widths = c(0.50, 1.00)))),
    "must occur exactly once"
  )
  expect_error(
    do.call(csdg_local_fidelity_audit, modifyList(arguments, list(seeds = matrix(1:4, nrow = 2L)))),
    "one row per requested case"
  )
  expect_error(
    do.call(csdg_local_fidelity_audit, modifyList(arguments, list(case_metadata = data.frame(fold = 1L)))),
    "reserved columns"
  )

  ambiguous = out
  ambiguous$test_sets[[2L]] = c(ambiguous$test_sets[[2L]], 3L)
  ambiguous_arguments = arguments
  ambiguous_arguments$x = ambiguous
  expect_error(
    do.call(csdg_local_fidelity_audit, ambiguous_arguments),
    "exactly one assessment split"
  )
})

test_that("public local-fidelity summaries combine per-case audits exactly", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task,
    fx$learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  cases = c(3L, 28L)
  labels = c("case_007", "case_009")
  common = list(
    seeds = c(9L, 10L),
    kernel_widths = c(0.75, 1.00),
    n_perturb = 100L,
    fidelity_threshold = 0.80
  )
  one_shot = do.call(csdg_local_fidelity_audit, c(
    list(x = out, cases = cases, case_labels = labels),
    common
  ))
  separate = lapply(seq_along(cases), function(index) {
    do.call(csdg_local_fidelity_audit, c(
      list(x = out, cases = cases[[index]], case_labels = labels[[index]]),
      common
    ))
  })
  combined = csdg_summarize_local_fidelity(
    data.table::rbindlist(lapply(separate, `[[`, "replicates")),
    data.table::rbindlist(lapply(separate, `[[`, "support")),
    fidelity_threshold = 0.80
  )

  expect_named(combined, c("summary", "cases", "bandwidth_cases", "by_support", "coefficient_stability"))
  expect_equal(nrow(combined$coefficient_stability), 0L)
  expect_identical(combined$summary, one_shot$summary)
  expect_identical(combined$cases, one_shot$cases)
  expect_identical(combined$bandwidth_cases, one_shot$bandwidth_cases)
  expect_identical(combined$by_support, one_shot$by_support)
})

test_that("public local-fidelity summaries reject malformed public tables", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task,
    fx$learner,
    mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE,
    seed = 8L
  )
  audited = csdg_local_fidelity_audit(
    out,
    cases = c(3L, 28L),
    seeds = c(9L, 10L),
    kernel_widths = c(0.75, 1.00),
    n_perturb = 100L,
    fidelity_threshold = 0.80,
    case_labels = c("case_007", "case_009")
  )
  summarize = function(replicates = audited$replicates, support = audited$support, ...) {
    csdg_summarize_local_fidelity(replicates, support, fidelity_threshold = 0.80, ...)
  }

  incomplete = data.table::copy(audited$replicates[-1L])
  expect_error(summarize(incomplete), "exact common case-by-replicate-by-width grid")

  unstable_seed = data.table::copy(audited$replicates)
  unstable_seed[1L, perturbation_seed := 99L]
  expect_error(summarize(unstable_seed), "stable seed")

  repeated_seed = data.table::copy(audited$replicates)
  first_seed = repeated_seed[perturbation_replicate == 1L, perturbation_seed][[1L]]
  repeated_seed[case_label == "case_007" & perturbation_replicate == 2L, perturbation_seed := first_seed]
  expect_error(summarize(repeated_seed), "distinct perturbation seeds")

  one_replicate = audited$replicates[perturbation_replicate == 1L]
  expect_error(summarize(one_replicate), "at least two common")

  wrong_role = data.table::copy(audited$replicates)
  wrong_role[1L, analysis_role := "sensitivity"]
  expect_error(summarize(wrong_role), "analysis_role.*inconsistent")

  wrong_flag = data.table::copy(audited$replicates)
  wrong_flag[1L, meets_threshold := !meets_threshold]
  expect_error(summarize(wrong_flag), "meets_threshold.*inconsistent")

  wrong_threshold = data.table::copy(audited$replicates)
  wrong_threshold[, fidelity_threshold := 0.70]
  expect_error(summarize(wrong_threshold), "fidelity_threshold.*inconsistent")
  expect_error(summarize(primary_kernel_width = 0.50), "must occur exactly once")

  duplicated_support = data.table::rbindlist(list(audited$support, audited$support[1L]))
  expect_error(summarize(support = duplicated_support), "exactly one row")

  mismatched_support = data.table::copy(audited$support)
  mismatched_support[1L, fold := fold + 1L]
  expect_error(summarize(support = mismatched_support), "matching every replicate case location")

  invalid_support = data.table::copy(audited$support)
  invalid_support[1L, kernel_weight_effective_n := n_training_background + 1]
  expect_error(summarize(support = invalid_support), "invalid or primary-width-inconsistent support metrics")

  mismatched_imputation = data.table::copy(audited$support)
  mismatched_imputation[1L, n_case_values_imputed := n_case_values_imputed + 1L]
  expect_error(summarize(support = mismatched_imputation), "Case-imputation counts")
})
