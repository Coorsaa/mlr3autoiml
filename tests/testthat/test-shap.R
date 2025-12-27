# FILE: tests/testthat/test-shap.R

test_that("AutoIML$shap returns additive Shapley-style contributions", {
  skip_on_cran()
  auto = get_auto_iris(quick_start = FALSE)
  task = auto$task
  model = auto$ctx$final_model

  expect_true(!is.null(model))

  pred_matrix = function(model, task, X) {
    pr = model$predict_newdata(X)
    if (inherits(task, "TaskClassif")) {
      as.matrix(pr$prob)
    } else {
      matrix(pr$response, ncol = 1L)
    }
  }

  # compute shap for a single row
  seed = 123
  sh = auto$shap(row_id = 1L, sample_size = 40L, background_n = 30L, seed = seed)

  expect_true(data.table::is.data.table(sh))
  expect_true(all(c("feature", "phi", "class_label") %in% names(sh)))

  # Reconstruct expected baseline: mean prediction on the sampled background rows
  set.seed(seed)
  bg_n = min(30L, task$nrow)
  bg_rows = sample(task$row_ids, size = bg_n)

  X_bg = task$data(rows = bg_rows, cols = task$feature_names)
  pred_bg = pred_matrix(model, task, X_bg)
  mean_bg = colMeans(pred_bg)

  X_x = task$data(rows = 1L, cols = task$feature_names)
  pred_x = pred_matrix(model, task, X_x)[1, ]

  # For each class label, sum(phi) ≈ pred_x - mean_bg
  by_class = split(sh, sh$class_label)
  for (cl in names(by_class)) {
    phi_sum = sum(by_class[[cl]]$phi)
    idx = which(colnames(pred_bg) == cl)
    if (length(idx) == 1L) {
      expect_lt(abs(phi_sum - (pred_x[idx] - mean_bg[idx])), 0.10)
    }
  }
})
