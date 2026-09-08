test_that("grouped resampling has no cluster leakage", {
  fx = make_classif_fixture()
  grouped = csdg_grouped_resampling(
    fx$task,
    group = fx$cluster,
    folds = 4L,
    repeats = 2L,
    seed = 11L
  )
  expect_s3_class(grouped, "CSDGGroupedResampling")
  expect_equal(nrow(grouped$iteration_map), 8L)
  for (i in seq_along(grouped$train_sets)) {
    train_groups = unique(fx$cluster[grouped$train_sets[[i]]])
    test_groups = unique(fx$cluster[grouped$test_sets[[i]]])
    expect_length(intersect(train_groups, test_groups), 0L)
  }
  out = csdg_resample(
    fx$task,
    mlr3::lrn("classif.featureless", predict_type = "prob"),
    grouped,
    seed = 12L
  )
  expect_equal(out$fold_scores$repetition, rep(1:2, each = 4L))
  expect_equal(out$fold_scores$fold, rep(1:4, times = 2L))
})

test_that("grouped resampling rejects groups that span nonmissing strata", {
  fx = make_classif_fixture()
  strata = rep("a", fx$task$nrow)
  strata[[2L]] = "b"

  expect_error(
    csdg_grouped_resampling(
      fx$task,
      group = fx$cluster,
      strata = strata,
      folds = 4L
    ),
    "Each group must map to at most one nonmissing stratum"
  )
})

test_that("resampling produces explicit OOF predictions", {
  skip_if_not_installed("rpart")
  fx = make_classif_fixture()
  rs = mlr3::rsmp("cv", folds = 3L)
  out = csdg_resample(
    fx$task, fx$learner, rs,
    store_models = TRUE, seed = 3L
  )
  expect_s3_class(out, "CSDGResample")
  expect_equal(length(out$models), 3L)
  expect_equal(sort(unique(out$predictions$row_id)), fx$task$row_ids)
  expect_equal(nrow(out$fold_scores), 3L)
  perf = csdg_performance(out)
  expect_true(all(c("per_iteration", "summary") %in% names(perf)))
})

test_that("custom resampling records a single repetition and sequential folds", {
  fx = make_classif_fixture()
  test_sets = split(fx$task$row_ids, rep(1:3, length.out = fx$task$nrow))
  train_sets = lapply(test_sets, function(test) setdiff(fx$task$row_ids, test))
  custom = mlr3::rsmp("custom")
  custom$instantiate(fx$task, train_sets = train_sets, test_sets = test_sets)

  out = csdg_resample(
    fx$task,
    mlr3::lrn("classif.featureless", predict_type = "prob"),
    custom,
    seed = 4L
  )

  expect_equal(out$fold_scores$repetition, rep(1L, 3L))
  expect_equal(out$fold_scores$fold, 1:3)
})
