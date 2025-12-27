# FILE: R/shapley.R
#
# Shapley / "SHAP"-style attributions via the same intervention construction
# used by the 'iml' package's Shapley class.
#
# @keywords internal
NULL

# Predict a numeric matrix:
# - classif: probabilities per class (columns = class labels)
# - regr: single column 'response'
#
# @keywords internal
.autoiml_predict_matrix = function(task, model, newdata) {
  pred = model$predict_newdata(newdata)

  if (inherits(task, "TaskClassif")) {
    prob = pred$prob
    if (is.null(prob)) {
      stop("Learner does not provide probability predictions; set predict_type='prob'.", call. = FALSE)
    }
    # PredictionClassif$prob can be a data.table or matrix; coerce robustly
    if (inherits(prob, "data.table")) {
      mat = as.matrix(prob)
      colnames(mat) = colnames(prob)
      return(mat)
    }
    mat = as.matrix(prob)
    if (is.null(colnames(mat)) && !is.null(task$class_names) && ncol(mat) == length(task$class_names)) {
      colnames(mat) = task$class_names
    }
    return(mat)
  }

  if (inherits(task, "TaskRegr")) {
    y = as.numeric(pred$response)
    mat = matrix(y, ncol = 1L)
    colnames(mat) = "response"
    return(mat)
  }

  stop("Unsupported task type for Shapley: ", class(task)[1L], call. = FALSE)
}

#' Internal Shapley computation matching iml's intervention design
#'
#' This follows the 'iml' Shapley algorithm:
#' - sample random permutations of features
#' - sample a background row for each permutation
#' - for each feature position in the permutation, build (with_k, without_k)
#'   instances and compute prediction differences
#'
#' @param task mlr3 task
#' @param model trained mlr3 learner
#' @param x_interest 1-row data.table/data.frame with task$feature_names
#' @param background data.table/data.frame with task$feature_names
#' @param sample_size integer Monte Carlo sample size
#' @param seed optional integer seed
#' @param class_labels optional character vector; subset classes for multiclass
#'
#' @return data.table with columns: class_label, feature, phi, phi_var, feature_value, sample_size
#'
#' @keywords internal
.autoiml_shapley_iml = function(task, model, x_interest, background,
  sample_size = 100L,
  seed = NULL,
  class_labels = NULL
) {
  if (is.null(seed)) seed = 1L
  set.seed(as.integer(seed))

  features = task$feature_names
  p = length(features)
  if (p < 1L) {
    return(data.table::data.table())
  }

  x_interest = data.table::as.data.table(x_interest)
  background = data.table::as.data.table(background)

  if (nrow(x_interest) != 1L) {
    stop("x_interest must have exactly 1 row.", call. = FALSE)
  }
  if (nrow(background) < 1L) {
    stop("background must have at least 1 row.", call. = FALSE)
  }

  missing_x = setdiff(features, names(x_interest))
  missing_bg = setdiff(features, names(background))
  if (length(missing_x) > 0L) {
    stop("x_interest is missing required features: ", paste(missing_x, collapse = ", "), call. = FALSE)
  }
  if (length(missing_bg) > 0L) {
    stop("background is missing required features: ", paste(missing_bg, collapse = ", "), call. = FALSE)
  }

  B = as.integer(sample_size)
  if (B < 1L) B = 1L

  # Determine prediction columns once
  base_pred = .autoiml_predict_matrix(task, model, x_interest[, features, with = FALSE])
  cls_all = colnames(base_pred)
  if (is.null(cls_all) || length(cls_all) < 1L) {
    stop("Could not determine prediction column names for Shapley.", call. = FALSE)
  }

  cls_keep = cls_all
  if (inherits(task, "TaskClassif") && !is.null(class_labels)) {
    cls_keep = intersect(as.character(class_labels), cls_all)
    if (length(cls_keep) < 1L) {
      stop("Requested class_labels not found in prediction outputs: ",
        paste(class_labels, collapse = ", "),
        call. = FALSE
      )
    }
  }

  C = length(cls_keep)

  sum_phi = matrix(0, nrow = p, ncol = C, dimnames = list(features, cls_keep))
  sum_phi2 = matrix(0, nrow = p, ncol = C, dimnames = list(features, cls_keep))

  xi = x_interest[, features, with = FALSE]

  for (m in seq_len(B)) {
    perm = sample.int(p, size = p, replace = FALSE)

    bg = background[sample.int(nrow(background), 1L), features, with = FALSE]
    current = data.table::copy(bg)

    rows = vector("list", 2L * p)

    for (pos in seq_len(p)) {
      without = data.table::copy(current)

      f = features[[perm[[pos]]]]
      current[[f]] = xi[[f]]

      with = data.table::copy(current)

      rows[[2L * pos - 1L]] = with
      rows[[2L * pos]] = without
    }

    newdata = data.table::rbindlist(rows, use.names = TRUE, fill = FALSE)

    pred = .autoiml_predict_matrix(task, model, newdata)
    pred = pred[, cls_keep, drop = FALSE]

    with_idx = seq.int(1L, 2L * p, by = 2L)
    without_idx = with_idx + 1L

    diff = pred[with_idx, , drop = FALSE] - pred[without_idx, , drop = FALSE] # p x C

    # Rows of diff correspond to permutation order -> accumulate into feature order via perm
    sum_phi[perm, ] = sum_phi[perm, ] + diff
    sum_phi2[perm, ] = sum_phi2[perm, ] + diff^2
  }

  phi = sum_phi / B
  phi_var = matrix(NA_real_, nrow = p, ncol = C, dimnames = list(features, cls_keep))
  if (B > 1L) {
    phi_var = (sum_phi2 - (sum_phi^2) / B) / (B - 1L)
  }

  if (inherits(task, "TaskRegr")) {
    out = data.table::data.table(
      class_label = NA_character_,
      feature = features,
      phi = as.numeric(phi[, 1L]),
      phi_var = as.numeric(phi_var[, 1L])
    )
  } else {
    dt_phi = data.table::as.data.table(phi)
    dt_phi[, feature := features]
    long_phi = data.table::melt(dt_phi, id.vars = "feature", variable.name = "class_label", value.name = "phi")

    dt_var = data.table::as.data.table(phi_var)
    dt_var[, feature := features]
    long_var = data.table::melt(dt_var, id.vars = "feature", variable.name = "class_label", value.name = "phi_var")

    out = merge(long_phi, long_var, by = c("feature", "class_label"), all = TRUE)
  }

  # Attach feature values for x_interest
  feat_vals = data.table::data.table(feature = features)
  feat_vals[, feature_value := vapply(features, function(f) {
    v = xi[[f]][1L]
    if (is.factor(v)) {
      return(as.character(v))
    }
    if (is.character(v)) {
      return(v)
    }
    if (is.logical(v)) {
      return(as.character(v))
    }
    if (is.numeric(v) || is.integer(v)) {
      return(as.character(as.numeric(v)))
    }
    as.character(v)
  }, character(1L))]

  out = merge(out, feat_vals, by = "feature", all.x = TRUE)
  out[, sample_size := B]
  data.table::setcolorder(out, c("class_label", "feature", "feature_value", "phi", "phi_var", "sample_size"))
  out[]
}
