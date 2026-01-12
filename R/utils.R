# FILE: R/utils.R
# Internal helpers for mlr3autoiml
# @keywords internal
NULL

`%||%` = function(a, b) if (!is.null(a)) a else b

# ---- normalization helpers -----------------------------------------------
# These keep the public API aligned with the paper terminology while
# remaining tolerant to minor formatting differences.

.autoiml_normalize_semantics = function(x, default = NA_character_) {
  # Returns one of: "associational", "marginal", "causal".
  # If `default` is NA and `x` is not recognized, returns NA.
  if (is.null(x) || length(x) < 1L) {
    return(default)
  }
  s_raw = as.character(x)[1L]
  if (is.na(s_raw) || !nzchar(s_raw)) {
    return(default)
  }

  s = tolower(s_raw)
  s_us = gsub("[[:space:]-]+", "_", s)

  # exact canonical values
  if (s_us %in% c("associational", "marginal", "causal")) {
    return(s_us)
  }

  # paper-aligned phrasing
  if (grepl("causal", s) || grepl("recourse", s) || grepl("counterfactual", s)) {
    return("causal")
  }
  if (grepl("marginal", s) || grepl("what[- ]?if", s) || grepl("interventional", s)) {
    return("marginal")
  }
  if (grepl("associational", s) || grepl("on[- ]?manifold", s) || grepl("observational", s) || grepl("descriptive", s)) {
    return("associational")
  }

  default
}

.autoiml_normalize_shap_mode = function(x, default = NA_character_) {
  # Returns one of: "marginal", "conditional".
  if (is.null(x) || length(x) < 1L) {
    return(default)
  }
  s_raw = as.character(x)[1L]
  if (is.na(s_raw) || !nzchar(s_raw)) {
    return(default)
  }

  s = tolower(s_raw)
  s_us = gsub("[[:space:]-]+", "_", s)

  if (s_us %in% c("marginal", "conditional")) {
    return(s_us)
  }
  if (s_us %in% c("interventional", "intervention")) {
    return("marginal")
  }
  if (s_us %in% c("on_manifold", "onmanifold", "associational")) {
    return("conditional")
  }

  default
}

.autoiml_clamp01 = function(p, eps = 1e-15) pmin(pmax(p, eps), 1 - eps)

.autoiml_logit = function(p) {
  p = .autoiml_clamp01(p)
  log(p / (1 - p))
}

.autoiml_inv_logit = function(x) 1 / (1 + exp(-x))

.autoiml_require_pkg = function(pkg) requireNamespace(pkg, quietly = TRUE)

.autoiml_feature_type = function(task, feature) {
  ft = tryCatch(task$feature_types, error = function(e) NULL)
  if (is.null(ft) || !all(c("id", "type") %in% names(ft))) {
    return(NA_character_)
  }
  out = ft[id == feature, type]
  if (length(out) == 0L) {
    return(NA_character_)
  }
  out[[1L]]
}

.autoiml_grid_quantiles = function(x, n = 10L, trim = c(0.05, 0.95)) {
  x = x[is.finite(x)]
  if (length(x) < 2L) {
    return(numeric())
  }
  n = as.integer(n)
  probs = seq(trim[1], trim[2], length.out = n)
  q_type = if (is.integer(x)) 1L else 7L
  qs = unique(as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, type = q_type)))
  qs = qs[is.finite(qs)]
  if (is.integer(x)) qs <- unique(as.integer(round(qs)))
  qs = qs[is.finite(qs)]
  qs
}

.autoiml_breaks_quantiles = function(x, bins = 10L, trim = c(0.05, 0.95)) {
  x = x[is.finite(x)]
  if (length(x) < 2L) {
    return(numeric())
  }
  bins = as.integer(bins)
  probs = seq(trim[1], trim[2], length.out = bins + 1L)
  q_type = if (is.integer(x)) 1L else 7L
  br = unique(as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, type = q_type)))
  br = br[is.finite(br)]
  if (is.integer(x)) br <- unique(as.integer(round(br)))
  br = br[is.finite(br)]
  sort(unique(br))
}

.autoiml_pred_prob_matrix = function(pred, task = NULL) {
  stopifnot(inherits(pred, "PredictionClassif"))
  prob = as.matrix(as.data.frame(pred$prob))
  storage.mode(prob) = "numeric"
  if (!is.null(task) && inherits(task, "TaskClassif")) {
    cn = task$class_names
    miss = setdiff(cn, colnames(prob))
    if (length(miss) > 0L) stop("Missing probability columns: ", paste(miss, collapse = ", "))
    prob = prob[, cn, drop = FALSE]
  }
  prob
}

.autoiml_pred_prob_for_class = function(pred, class_label) {
  stopifnot(inherits(pred, "PredictionClassif"))
  prob = pred$prob
  if (!class_label %in% colnames(prob)) stop("Class not found in prob: ", class_label)
  as.numeric(prob[, class_label])
}

.autoiml_pred_numeric = function(pred, task) {
  if (inherits(pred, "PredictionClassif")) {
    prob = pred$prob
    if (ncol(prob) == 1L) {
      return(as.numeric(prob[[1L]]))
    }
    if (ncol(prob) == 2L) {
      pos = task$positive %||% colnames(prob)[2L]
      return(as.numeric(prob[, pos]))
    }
    return(as.numeric(apply(as.matrix(prob), 1, max)))
  }
  if (inherits(pred, "PredictionRegr")) {
    return(as.numeric(pred$response))
  }
  stop("Unsupported prediction type: ", class(pred)[1])
}

.autoiml_predict_score = function(model, newdata, task, class_of_interest = NULL) {
  pr = model$predict_newdata(newdata)
  if (inherits(task, "TaskClassif")) {
    if (!is.null(class_of_interest)) {
      return(.autoiml_pred_prob_for_class(pr, class_of_interest))
    }
    return(.autoiml_pred_numeric(pr, task))
  }
  .autoiml_pred_numeric(pr, task)
}

.autoiml_auc = function(truth01, score) {
  truth01 = as.integer(truth01)
  ok = is.finite(score) & (truth01 %in% c(0L, 1L))
  truth01 = truth01[ok]
  score = score[ok]
  n1 = sum(truth01 == 1L)
  n0 = sum(truth01 == 0L)
  if (n1 == 0L || n0 == 0L) {
    return(NA_real_)
  }
  r = rank(score, ties.method = "average")
  (sum(r[truth01 == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

.autoiml_brier = function(truth01, p_hat) mean((truth01 - p_hat)^2)

.autoiml_logloss = function(truth01, p_hat) {
  p = .autoiml_clamp01(p_hat)
  -mean(truth01 * log(p) + (1 - truth01) * log(1 - p))
}

.autoiml_logloss_multiclass = function(truth, prob) {
  if (!is.factor(truth)) truth <- factor(truth)
  prob = as.matrix(prob)
  lev = levels(truth)
  miss = setdiff(lev, colnames(prob))
  if (length(miss) > 0L) stop("prob missing classes: ", paste(miss, collapse = ", "))
  prob = prob[, lev, drop = FALSE]
  idx = match(as.character(truth), lev)
  p_true = prob[cbind(seq_len(nrow(prob)), idx)]
  p_true = .autoiml_clamp01(p_true)
  -mean(log(p_true))
}

.autoiml_mbrier = function(truth, prob) {
  if (!is.factor(truth)) truth <- factor(truth)
  prob = as.matrix(prob)
  lev = levels(truth)
  miss = setdiff(lev, colnames(prob))
  if (length(miss) > 0L) stop("prob missing classes: ", paste(miss, collapse = ", "))
  prob = prob[, lev, drop = FALSE]
  n = length(truth)
  K = length(lev)
  I = matrix(0, nrow = n, ncol = K)
  I[cbind(seq_len(n), match(as.character(truth), lev))] = 1
  mean(rowSums((I - prob)^2))
}

.autoiml_bin_mean = function(x, y, breaks) {
  bx = cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)
  dt = data.table::data.table(bin = bx, x = x, y = y)
  out = dt[, .(n = .N, x_mid = mean(x, na.rm = TRUE), y_mean = mean(y, na.rm = TRUE)), by = bin]
  out[order(x_mid)]
}

.autoiml_ece_binary = function(truth01, p_hat, bins = 10L) {
  p_hat = as.numeric(p_hat)
  truth01 = as.integer(truth01)
  ok = is.finite(p_hat) & (truth01 %in% c(0L, 1L))
  p_hat = p_hat[ok]
  truth01 = truth01[ok]
  if (length(p_hat) < 2L) {
    return(NA_real_)
  }
  qs = unique(stats::quantile(p_hat, probs = seq(0, 1, length.out = bins + 1L), na.rm = TRUE))
  qs = qs[is.finite(qs)]
  if (length(qs) < 3L) {
    return(NA_real_)
  }
  dt = .autoiml_bin_mean(p_hat, truth01, breaks = qs)
  if (is.null(dt) || nrow(dt) == 0L) {
    return(NA_real_)
  }
  N = sum(dt$n)
  sum((dt$n / N) * abs(dt$y_mean - dt$x_mid), na.rm = TRUE)
}

.autoiml_reliability_curve_binary = function(truth01, p_hat, bins = 10L) {
  p_hat = as.numeric(p_hat)
  truth01 = as.integer(truth01)
  ok = is.finite(p_hat) & (truth01 %in% c(0L, 1L))
  p_hat = p_hat[ok]
  truth01 = truth01[ok]
  if (length(p_hat) < 2L) {
    return(NULL)
  }
  qs = unique(stats::quantile(p_hat, probs = seq(0, 1, length.out = bins + 1L), na.rm = TRUE))
  qs = qs[is.finite(qs)]
  if (length(qs) < 3L) {
    return(NULL)
  }
  .autoiml_bin_mean(p_hat, truth01, breaks = qs)
}

.autoiml_calibration_glm = function(truth01, p_hat) {
  df = data.frame(y = truth01, lp = .autoiml_logit(p_hat))
  fit = stats::glm(y ~ lp, data = df, family = stats::binomial())
  co = stats::coef(summary(fit))
  list(
    intercept = unname(co["(Intercept)", "Estimate"]),
    slope = unname(co["lp", "Estimate"]),
    intercept_se = unname(co["(Intercept)", "Std. Error"]),
    slope_se = unname(co["lp", "Std. Error"])
  )
}

.autoiml_dca = function(truth01, p_hat, thresholds = seq(0.01, 0.99, by = 0.01)) {
  N = length(truth01)
  out = lapply(thresholds, function(tau) {
    pred_pos = p_hat >= tau
    TP = sum(pred_pos & truth01 == 1)
    FP = sum(pred_pos & truth01 == 0)
    nb = TP / N - FP / N * (tau / (1 - tau))
    data.table::data.table(threshold = tau, TP = TP, FP = FP, N = N, net_benefit = nb)
  })
  data.table::rbindlist(out)
}

.autoiml_rmse = function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))

.autoiml_mode = function(x) {
  x = x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# ---- simple CI helpers ----------------------------------------------------

.autoiml_ci_mean = function(x, level = 0.95) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  n = length(x)

  if (n == 0L) {
    return(list(mean = NA_real_, lower = NA_real_, upper = NA_real_, n = 0L))
  }

  m = mean(x)
  if (n == 1L) {
    return(list(mean = m, lower = NA_real_, upper = NA_real_, n = 1L))
  }

  alpha = 1 - level
  se = stats::sd(x) / sqrt(n)
  tcrit = stats::qt(1 - alpha / 2, df = n - 1)
  list(mean = m, lower = m - tcrit * se, upper = m + tcrit * se, n = n)
}


# ---- GateResult resolution ------------------------------------------------
# @keywords internal
.autoiml_get_gate_result = function(x, gate_id) {
  if (inherits(x, "AutoIML")) {
    if (!is.null(x$result)) {
      return(.autoiml_get_gate_result(x$result, gate_id))
    }
    return(NULL)
  }

  if (inherits(x, "AutoIMLResult")) {
    gs = x$gate_results
    if (is.null(gs) || length(gs) == 0L) {
      return(NULL)
    }

    # Named lookup first
    g = gs[[gate_id]]
    if (inherits(g, "GateResult")) {
      return(g)
    }

    # Fallback: scan list
    for (i in gs) {
      if (inherits(i, "GateResult") && identical(i$gate_id, gate_id)) {
        return(i)
      }
    }
    return(NULL)
  }

  if (is.list(x) && length(x) > 0L) {
    for (i in x) {
      if (inherits(i, "GateResult") && identical(i$gate_id, gate_id)) {
        return(i)
      }
    }
  }

  NULL
}

.autoiml_assert_known_names = function(cfg, allowed, where = "config") {
  if (is.null(cfg)) {
    return(invisible(TRUE))
  }
  if (is.environment(cfg)) cfg = as.list(cfg)

  if (!is.list(cfg)) {
    stop(where, " must be a list (or environment coercible to list).", call. = FALSE)
  }

  nms = names(cfg)
  if (is.null(nms) || length(nms) == 0L) {
    return(invisible(TRUE))
  }

  nms = nms[nzchar(nms)]
  unknown = setdiff(nms, allowed)

  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "%s contains unknown keys: %s\nAllowed keys: %s",
        where,
        paste(unknown, collapse = ", "),
        paste(allowed, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.autoiml_validate_ctx = function(ctx) {
  if (!is.environment(ctx)) {
    return(invisible(TRUE))
  }

  allowed_structure = c(
    "sample_n", "max_features", "ice_keep_n",
    "grid_n", "grid_type",
    "ice_center",
    "ale_bins",
    "cor_threshold",
    "hstat_max_features", "hstat_grid_n", "hstat_threshold",
    "support_check",
    "regionalize", "regional_method",
    "gadget_max_depth", "gadget_min_bucket", "gadget_gamma",
    "gadget_top_k"
  )

  allowed_calibration = c("thresholds", "bins")

  allowed_stability = c("B", "max_features", "grouping")

  if (!is.null(ctx$structure)) .autoiml_assert_known_names(ctx$structure, allowed_structure, "ctx$structure")
  if (!is.null(ctx$calibration)) .autoiml_assert_known_names(ctx$calibration, allowed_calibration, "ctx$calibration")
  if (!is.null(ctx$stability)) .autoiml_assert_known_names(ctx$stability, allowed_stability, "ctx$stability")

  invisible(TRUE)
}
