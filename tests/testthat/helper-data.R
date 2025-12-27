# FILE: tests/testthat/helper-data.R

.autoiml_test_cache = new.env(parent = emptyenv())

make_task_iris = function() {
  mlr3::as_task_classif(Species ~ ., data = iris, id = "iris")
}

make_task_mtcars_regr = function() {
  mlr3::as_task_regr(mpg ~ ., data = mtcars, id = "mtcars_regr")
}

make_resampling_cv = function(folds = 3L) {
  mlr3::rsmp("cv", folds = folds)
}

make_learner_classif = function() {
  # mlr3 >= 1.3.0 ships rpart learners; older setups can use mlr3learners.
  lr = mlr3::lrn("classif.rpart", predict_type = "prob")
  lr
}

make_learner_regr = function() {
  lr = mlr3::lrn("regr.rpart")
  lr
}

get_auto_iris = function(quick_start = FALSE, seed = 123) {
  key = sprintf("iris_qs=%s_seed=%s", quick_start, seed)
  if (exists(key, envir = .autoiml_test_cache, inherits = FALSE)) {
    return(get(key, envir = .autoiml_test_cache, inherits = FALSE))
  }

  task = make_task_iris()
  learner = make_learner_classif()
  resampling = make_resampling_cv(folds = 3L)

  auto = mlr3autoiml::AutoIML$new(
    task = task,
    learner = learner,
    resampling = resampling,
    purpose = "global_insight",
    quick_start = quick_start,
    seed = seed,
    config = list(
      structure = list(
        sample_n = min(120L, task$nrow),
        grid_n = 10L,
        max_features = 4L,
        regionalize = FALSE
      )
    )
  )

  auto$run(verbose = FALSE)
  assign(key, auto, envir = .autoiml_test_cache)
  auto
}

get_auto_mtcars = function(quick_start = FALSE, seed = 123) {
  key = sprintf("mtcars_qs=%s_seed=%s", quick_start, seed)
  if (exists(key, envir = .autoiml_test_cache, inherits = FALSE)) {
    return(get(key, envir = .autoiml_test_cache, inherits = FALSE))
  }

  task = make_task_mtcars_regr()
  learner = make_learner_regr()
  resampling = make_resampling_cv(folds = 3L)

  auto = mlr3autoiml::AutoIML$new(
    task = task,
    learner = learner,
    resampling = resampling,
    purpose = "global_insight",
    quick_start = quick_start,
    seed = seed,
    config = list(
      structure = list(
        sample_n = min(32L, task$nrow),
        grid_n = 10L,
        max_features = 4L,
        regionalize = FALSE
      )
    )
  )

  auto$run(verbose = FALSE)
  assign(key, auto, envir = .autoiml_test_cache)
  auto
}
