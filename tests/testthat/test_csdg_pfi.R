test_that("foldwise PFI is computed on held-out rows", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture()
  out = csdg_resample(
    fx$task, fx$learner, mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE, seed = 5L
  )
  sequential = csdg_fold_pfi(out, repetitions = 2L, batch_size = 1L, seed = 6L)
  pfi = csdg_fold_pfi(out, repetitions = 2L, batch_size = 4L, seed = 6L)
  expect_equal(pfi$raw, sequential$raw)
  expect_equal(pfi$per_iteration, sequential$per_iteration)
  expect_equal(pfi$summary, sequential$summary)
  expect_equal(pfi$perturbation$requested_prediction_batch_size, 4L)
  expect_equal(pfi$perturbation$effective_prediction_batch_sizes, 4L)
  expect_equal(pfi$perturbation$observed_stacked_batch_rows, 160L)
  expect_equal(
    data.table::uniqueN(pfi$per_iteration$iteration),
    3L
  )
  expect_equal(
    sort(unique(pfi$summary$feature_group)),
    sort(fx$task$feature_names)
  )
  expect_match(pfi$summary$uncertainty_label[[1L]], "not a confidence interval")
  expect_true(all(c("repetition", "permutation_repetition") %in% names(pfi$raw)))
  expect_equal(anyDuplicated(names(pfi$raw)), 0L)
  expect_error(csdg_fold_pfi(out, batch_size = 0L), "batch_size")
})

test_that("batched PFI preserves graph-pipeline results, factors, strata, and RNG state", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("mlr3pipelines")
  set.seed(21L)
  n = 90L
  data = data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::runif(n),
    category = factor(sample(c("a", "b", "c", NA), n, replace = TRUE))
  )
  data$x1[sample.int(n, 8L)] = NA_real_
  data$y = 2 * ifelse(is.na(data$x1), 0, data$x1) - data$x2 + stats::rnorm(n, sd = 0.2)
  task = mlr3::as_task_regr(data, target = "y")
  graph = Reduce(mlr3pipelines::`%>>%`, list(
    mlr3pipelines::po("imputemedian", affect_columns = mlr3pipelines::selector_type("numeric")),
    mlr3pipelines::po("imputemode", affect_columns = mlr3pipelines::selector_type("factor")),
    mlr3pipelines::po("encode", method = "one-hot", affect_columns = mlr3pipelines::selector_type("factor")),
    mlr3pipelines::po("learner", learner = mlr3::lrn("regr.rpart"))
  ))
  learner = mlr3::as_learner(graph)
  out = csdg_resample(task, learner, mlr3::rsmp("cv", folds = 3L), store_models = TRUE, seed = 22L)
  feature_groups = list(x1 = "x1", category = "category", joint = c("x1", "category"))
  strata = rep(c("left", "right"), length.out = n)

  sequential = csdg_fold_pfi(
    out,
    feature_groups = feature_groups,
    repetitions = 2L,
    strata = strata,
    batch_size = 1L,
    seed = 23L
  )
  sequential_rng = .Random.seed
  batched = csdg_fold_pfi(
    out,
    feature_groups = feature_groups,
    repetitions = 2L,
    strata = strata,
    batch_size = 4L,
    seed = 23L
  )
  batched_rng = .Random.seed
  oversized = csdg_fold_pfi(
    out,
    feature_groups = feature_groups,
    repetitions = 2L,
    strata = strata,
    batch_size = 20L,
    seed = 23L
  )

  expect_identical(batched$raw, sequential$raw)
  expect_identical(batched$per_iteration, sequential$per_iteration)
  expect_identical(batched$summary, sequential$summary)
  expect_identical(batched_rng, sequential_rng)
  expect_identical(oversized$raw, sequential$raw)
  expect_equal(.pfi_effective_batch_size(200000L, 1000L), 100L)
})

test_that("cluster-level PFI permutes coherent cluster features within strata", {
  skip_if_not_installed("rpart")
  set.seed(25L)
  n_clusters = 30L
  cluster = rep(seq_len(n_clusters), each = 4L)
  country = rep(rep(c("a", "b", "c"), each = 10L), each = 4L)
  school_feature = rep(rnorm(n_clusters), each = 4L)
  data = data.frame(
    school_feature = school_feature,
    student_feature = rnorm(length(cluster)),
    y = school_feature + rnorm(length(cluster), sd = 0.2)
  )
  task = mlr3::as_task_regr(data, target = "y")
  resampling = csdg_grouped_resampling(task, group = cluster, strata = country, folds = 3L, seed = 26L)
  result = csdg_resample(
    task,
    mlr3::lrn("regr.rpart"),
    resampling,
    store_models = TRUE,
    seed = 26L
  )
  pfi = csdg_fold_pfi(
    result,
    feature_groups = list(school = "school_feature", student = "student_feature"),
    repetitions = 3L,
    strata = country,
    cluster = cluster,
    cluster_level_groups = "school",
    seed = 27L
  )

  expect_true(pfi$perturbation$cluster_aware)
  expect_identical(pfi$perturbation$cluster_level_groups, "school")
  expect_true(all(pfi$per_iteration$n_permutations == 3L))
  expect_true(all(is.finite(pfi$summary$mean_monte_carlo_se)))
})

test_that("importance agreement distinguishes exact ranks from practical ties", {
  reference = data.frame(
    feature_group = c("a", "b", "c", "joint"),
    importance = c(1.00, 0.60, 0.59, 1.20)
  )
  comparison = data.frame(
    feature_group = c("a", "b", "c", "joint"),
    importance = c(0.98, 0.59, 0.60, 1.10)
  )
  agreement = csdg_importance_agreement(
    reference,
    comparison,
    top_k = 2L,
    practical_tolerances = c(0, 0.02),
    exclude_groups = "joint"
  )

  expect_true(agreement$tolerance_sensitivity[practical_tolerance == 0, n_exact_reversals] > 0L)
  expect_equal(agreement$tolerance_sensitivity[practical_tolerance == 0.02, n_practical_reversals], 0L)
  expect_true(agreement$direct_differences[feature_group == "joint", excluded_from_rank_comparison])
  expect_equal(agreement$top_k$jaccard, 1 / 3)

  strict_default = csdg_importance_agreement(reference, comparison, exclude_groups = "joint")
  expect_identical(strict_default$tolerance_sensitivity$practical_tolerance, 0)
})

test_that("OOF local surrogate uses held-out fold models", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  out = csdg_resample(
    fx$task, fx$learner, mlr3::rsmp("cv", folds = 3L),
    store_models = TRUE, seed = 8L
  )
  requested_cases = c(out$test_sets[[3L]][[1L]], out$test_sets[[1L]][[1L]])
  local = csdg_oof_local_surrogate(
    out,
    cases = requested_cases,
    n_perturb = 60L,
    seed = 9L
  )
  expect_identical(local$summary$case_id, as.character(requested_cases))
  expect_identical(unique(local$coefficients$case_id), as.character(requested_cases))
  expect_true(all(is.finite(local$summary$weighted_rmse)))
  expect_true(all(local$summary$n_perturb == 60L))
  expect_true(all(local$summary$n_evaluation == 60L))
  expect_true(all(local$summary$n_neighborhood == 61L))
  expect_true(all(local$summary$n_crossfit_folds == 5L))
  expect_true(all(local$summary$evaluation_method == "deterministic weighted ridge cross-fitting"))
  expect_equal(local$evaluation$method, "deterministic weighted ridge cross-fitting")
  expect_match(local$evaluation$fit_separation, "without that perturbation")
  expect_match(local$limitations, "did not fit that perturbation", all = FALSE)
})

test_that("local surrogate cross-fitting does not score memorized perturbations", {
  n = 60L
  design = cbind("(Intercept)" = 1, diag(n))
  outcome = rep(c(0, 1), length.out = n)
  weights = rep(1, n)
  fold_id = .local_crossfit_folds(n, seed = 91L)

  crossfit = .local_crossfit_surrogate(design, outcome, weights, fold_id)
  crossfit_metrics = .local_weighted_metrics(outcome, crossfit$predicted, weights)
  apparent = .local_weighted_ridge(design, outcome, weights)
  apparent_metrics = .local_weighted_metrics(outcome, apparent$fitted, weights)

  expect_true(all(is.finite(crossfit$predicted)))
  expect_lt(crossfit_metrics$r2, 0.10)
  expect_gt(apparent_metrics$r2, 0.95)
  expect_gt(apparent_metrics$r2 - crossfit_metrics$r2, 0.80)
})

test_that("weighted local ridge handles an intercept-only effective design", {
  design = cbind("(Intercept)" = 1, constant = rep(3, 20L))
  outcome = rep(0.4, 20L)
  fit = .local_weighted_ridge(design, outcome, rep(1, 20L))

  expect_equal(fit$fitted, outcome)
  expect_equal(fit$coefficients[["constant"]], 0)
  expect_equal(fit$n_active_terms, 0L)
})

test_that("local surrogate evaluation is deterministic and preserves the caller RNG", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture(n = 90L)
  learner = fx$learner$clone(deep = TRUE)
  learner$train(fx$task)

  set.seed(301L)
  before = .Random.seed
  first = csdg_local_surrogate(learner, fx$task, cases = 1L, n_perturb = 60L, seed = 92L)
  after = .Random.seed
  second = csdg_local_surrogate(learner, fx$task, cases = 1L, n_perturb = 60L, seed = 92L)

  expect_identical(after, before)
  expect_identical(first$summary, second$summary)
  expect_identical(first$coefficients, second$coefficients)
  expect_equal(first$summary$n_evaluation, first$summary$n_perturb)
  expect_lte(first$summary$weighted_r2, 1)
  expect_true(is.finite(first$summary$weighted_rmse))
  expect_true(is.finite(first$summary$apparent_weighted_rmse))
})

test_that("OOF local surrogate handles a held-out case with a missing feature", {
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
  local = csdg_oof_local_surrogate(out, cases = 1L, n_perturb = 60L, seed = 19L)

  expect_equal(local$summary$n_case_values_imputed, 1L)
  expect_equal(local$summary$case_imputed_features, "x1")
  expect_true(is.finite(local$summary$weighted_rmse))
})
