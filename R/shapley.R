# FILE: R/shapley.R

#' @title Shapley Value Computations
#'
#' @description
#' Internal Shapley/SHAP-style attribution computations via an intervention-style
#' construction inspired by the 'iml' package's Shapley class.
#'
#' The implementation supports two modes:
#' \itemize{
#'   \item `"marginal"` (interventional): breaks feature dependence by design and
#'         answers a model-based "what-if" question (holding other features to
#'         background draws). This matches the classic SHAP formulation.
#'   \item `"conditional"`: aims to respect empirical dependence by sampling
#'         background rows conditional on the already-fixed feature coalition.
#'         This approximates an on-manifold / associational question using a
#'         lightweight kNN conditional sampler.
#' }
#'
#' @name shapley
#' @keywords internal
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

#' Internal Shapley computation
#'
#' * `mode = "marginal"` matches iml's interventional design.
#' * `mode = "conditional"` uses a lightweight kNN conditional sampler on the provided background data.
#'
#' This follows a Monte Carlo Shapley scheme:
#' - sample random permutations of features
#' - for each feature position, estimate the marginal contribution via paired predictions
#'
#' @param task mlr3 task
#' @param model trained mlr3 learner
#' @param x_interest 1-row data.table/data.frame with task$feature_names
#' @param background data.table/data.frame with task$feature_names
#' @param sample_size integer Monte Carlo sample size
#' @param seed optional integer seed
#' @param class_labels optional character vector; subset classes for multiclass
#' @param mode "marginal" or "conditional"
#' @param conditional_k integer; k for kNN conditional sampling (only for mode="conditional")
#' @param conditional_weighted logical; if TRUE, sample among kNN with weights 1/(dist+eps)
#'
#' @return data.table with columns: class_label, feature, phi, phi_var, feature_value, sample_size
#'
#' @keywords internal
.autoiml_shapley_iml = function(task, model, x_interest, background,
  sample_size = 100L,
  seed = NULL,
  class_labels = NULL,
  mode = c("marginal", "conditional"),
  conditional_k = 5L,
  conditional_weighted = TRUE
) {
  if (is.null(seed)) seed = 1L
  set.seed(as.integer(seed))

  mode = match.arg(mode)
  conditional_k = as.integer(conditional_k)
  conditional_k = max(1L, conditional_k)

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

  # Precompute numeric SDs for conditional kNN scaling (cheap, background is small by design).
  sd_scale = NULL
  if (identical(mode, "conditional")) {
    sd_scale = vapply(features, function(f) {
      col = background[[f]]
      if (is.numeric(col) || is.integer(col)) {
        s = stats::sd(as.numeric(col), na.rm = TRUE)
        if (!is.finite(s) || s <= 0) s = 1
        return(as.numeric(s))
      }
      NA_real_
    }, numeric(1L))
  }

  # Lightweight conditional sampler:
  # sample a background row that is close to x_interest on the conditioning features.
  sample_conditional_row = function(cond_features) {
    if (length(cond_features) < 1L) {
      return(background[sample.int(nrow(background), 1L), features, with = FALSE])
    }

    cond_features = intersect(cond_features, features)
    if (length(cond_features) < 1L) {
      return(background[sample.int(nrow(background), 1L), features, with = FALSE])
    }

    n_bg = nrow(background)
    d = rep(0, n_bg)

    for (f in cond_features) {
      xv = xi[[f]][1L]
      if (is.null(xv) || is.na(xv)) next

      col = background[[f]]
      if (is.numeric(col) || is.integer(col)) {
        s = sd_scale[[f]]
        if (!is.finite(s) || s <= 0) s = 1
        dv = abs(as.numeric(col) - as.numeric(xv)) / s
        dv[is.na(dv)] = 0
        d = d + dv
      } else {
        dv = as.numeric(col != xv)
        dv[is.na(dv)] = 0
        d = d + dv
      }
    }

    k = min(conditional_k, n_bg)
    idx = order(d)[seq_len(k)]

    if (isTRUE(conditional_weighted)) {
      w = 1 / (d[idx] + 1e-6)
      w = w / sum(w)
      ii = sample(idx, size = 1L, prob = w)
    } else {
      ii = sample(idx, size = 1L)
    }

    background[ii, features, with = FALSE]
  }

  for (m in seq_len(B)) {
    perm = sample.int(p, size = p, replace = FALSE)

    rows = vector("list", 2L * p)

    if (identical(mode, "marginal")) {
      # ---- interventional / marginal ("iml-style") -----------------------
      bg = background[sample.int(nrow(background), 1L), features, with = FALSE]
      current = data.table::copy(bg)

      for (pos in seq_len(p)) {
        without = data.table::copy(current)

        f = features[[perm[[pos]]]]
        current[[f]] = xi[[f]]

        with = data.table::copy(current)

        rows[[2L * pos - 1L]] = with
        rows[[2L * pos]] = without
      }
    } else {
      # ---- conditional / on-manifold approximation ------------------------
      for (pos in seq_len(p)) {
        # coalition without the current feature
        S0 = if (pos <= 1L) character(0) else features[perm[seq_len(pos - 1L)]]
        # coalition including the current feature
        S1 = features[perm[seq_len(pos)]]

        bg0 = data.table::copy(sample_conditional_row(S0))
        bg1 = data.table::copy(sample_conditional_row(S1))

        # enforce coalition values
        if (length(S0) > 0L) {
          for (ff in S0) bg0[[ff]] = xi[[ff]]
        }
        if (length(S1) > 0L) {
          for (ff in S1) bg1[[ff]] = xi[[ff]]
        }

        rows[[2L * pos - 1L]] = bg1
        rows[[2L * pos]] = bg0
      }
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
  out[, shap_mode := mode]
  data.table::setcolorder(out, c("class_label", "feature", "feature_value", "phi", "phi_var", "sample_size", "shap_mode"))
  out[]
}
