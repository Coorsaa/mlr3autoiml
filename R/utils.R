# FILE: R/utils.R

#' @title Internal Utility Functions for mlr3autoiml
#'
#' @description
#' Collection of internal helper functions used throughout the mlr3autoiml package.
#' These include:
#' \itemize{
#'   \item Normalization helpers for semantics and SHAP modes
#'   \item Prediction utilities for classification and regression
#'   \item Calibration helpers (ECE, reliability curves, calibration GLM)
#'   \item Decision curve analysis (DCA)
#'   \item GateResult resolution utilities
#'   \item Configuration validation
#' }
#'
#' @name utils
#' @keywords internal
NULL

`%||%` = function(a, b) if (!is.null(a)) a else b

# ---- common utility helpers -----------------------------------------------

#' Safely convert environment to list, pass through lists and handle NULL
#' @keywords internal
.autoiml_as_list = function(x) {

  if (is.null(x)) {
    return(list())
  }
  if (is.environment(x)) {
    return(as.list(x))
  }
  if (is.list(x)) {
    return(x)
  }
  list()
}

#' Ensure final model exists in ctx, training if needed with clear warning
#' @keywords internal
.autoiml_ensure_model = function(ctx, task) {
  model = ctx$final_model
  if (!is.null(model)) {
    return(model)
  }

  stop(
    "No trained final model in context. Run Gate 1 before downstream gates.",
    call. = FALSE
  )
}

#' Valid claim purposes
#' @keywords internal
.autoiml_purposes = c("exploratory", "global_insight", "decision_support", "deployment")

#' Validate purpose argument
#' @keywords internal
.autoiml_validate_purpose = function(x) {
  match.arg(x, .autoiml_purposes)
}

#' Derive deterministic gate-specific seed
#' @keywords internal
.autoiml_gate_seed = function(ctx, gate_id) {
  base_seed = ctx$seed %||% 1L
  # Use gate_id hash to derive reproducible offset
  offset = sum(utf8ToInt(as.character(gate_id)))
  (base_seed + offset) %% .Machine$integer.max
}

# ---- normalization helpers -----------------------------------------------
# These keep the public API aligned with the framework terminology while
# remaining tolerant to minor formatting differences.

.autoiml_normalize_semantics = function(x, default = NA_character_) {
  # Canonical semantics labels used throughout the framework + package.
  #
  # Returns one of:
  #   - "within_support"         (associational / on-manifold / descriptive)
  #   - "marginal_model_query"   ("what would the model predict if X_j = x?")
  #   - "causal_recourse"        (action / recourse; requires causal identification outside AutoIML)
  #
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
  if (s_us %in% c("within_support", "marginal_model_query", "causal_recourse")) {
    return(s_us)
  }

  # common synonyms (accepted inputs)
  if (s_us %in% c("associational", "on_manifold", "onmanifold", "descriptive", "observational", "within-support")) {
    return("within_support")
  }
  if (s_us %in% c("marginal", "marginal_what_if", "what_if", "whatif", "model_query")) {
    return("marginal_model_query")
  }
  if (s_us %in% c("causal", "recourse", "counterfactual")) {
    return("causal_recourse")
  }

  # tolerant matching
  if (grepl("causal", s) || grepl("recourse", s) || grepl("counterfactual", s)) {
    return("causal_recourse")
  }
  # NOTE: "marginal model query" is a model-based query and NOT a causal claim.
  # Many authors use \"interventional\" loosely to denote this model query; we map it here.
  if (grepl("marginal", s) || grepl("model[ _-]?query", s) || grepl("what[- ]?if", s) || grepl("intervention", s)) {
    return("marginal_model_query")
  }
  if (grepl("within[ _-]?support", s) || grepl("associational", s) || grepl("on[- ]?manifold", s) ||
    grepl("observational", s) || grepl("descriptive", s)) {
    return("within_support")
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
  cfg = .autoiml_as_list(cfg)

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
  allowed_validation = c("split_policy", "cluster_var", "time_var", "site_var")
  allowed_plausible_values = c("pv_tasks")

  allowed_stability = c("B", "max_features", "grouping", "sanity_checks", "instability_rel_sd_warn")
  allowed_multiplicity = c(
    "enabled", "max_alt_learners", "rashomon_rule", "epsilon", "importance_n", "importance_max_features",
    "require_transport_for_high_stakes", "group_col", "transport_mode", "transport_measure_id"
  )

  if (!is.null(ctx$structure)) .autoiml_assert_known_names(ctx$structure, allowed_structure, "ctx$structure")
  if (!is.null(ctx$calibration)) .autoiml_assert_known_names(ctx$calibration, allowed_calibration, "ctx$calibration")
  if (!is.null(ctx$validation)) .autoiml_assert_known_names(ctx$validation, allowed_validation, "ctx$validation")
  if (!is.null(ctx$plausible_values)) .autoiml_assert_known_names(ctx$plausible_values, allowed_plausible_values, "ctx$plausible_values")
  if (!is.null(ctx$stability)) .autoiml_assert_known_names(ctx$stability, allowed_stability, "ctx$stability")
  if (!is.null(ctx$multiplicity)) .autoiml_assert_known_names(ctx$multiplicity, allowed_multiplicity, "ctx$multiplicity")

  invisible(TRUE)
}


# ---- framework traceability scaffolding ---------------------------------

#' Resolve a file in inst/extdata with robust package/dev fallback.
#' @keywords internal
.autoiml_extdata_path = function(file) {
  checkmate::assert_string(file, min.chars = 1L)

  p = system.file("extdata", file, package = "mlr3autoiml")
  if (nzchar(p) && file.exists(p)) {
    return(p)
  }

  p_dev = file.path("inst", "extdata", file)
  if (file.exists(p_dev)) {
    return(normalizePath(p_dev, winslash = "/", mustWork = TRUE))
  }

  stop("Traceability asset not found: ", file, call. = FALSE)
}


#' Read YAML file if yaml package is available.
#' @keywords internal
.autoiml_read_yaml_file = function(path) {
  checkmate::assert_string(path, min.chars = 1L)
  if (!file.exists(path)) {
    stop("YAML file does not exist: ", path, call. = FALSE)
  }

  yaml::read_yaml(path)
}


#' Load framework requirements matrix from inst/extdata.
#' @keywords internal
.autoiml_framework_requirements = function() {
  path = .autoiml_extdata_path("framework_requirements.yaml")
  x = .autoiml_read_yaml_file(path)
  x$`_path` = path
  x
}


#' Load IEL rules from inst/extdata.
#' @keywords internal
.autoiml_iel_rules = function() {
  path = .autoiml_extdata_path("iel_rules.yaml")
  x = .autoiml_read_yaml_file(path)
  x$`_path` = path
  x
}


#' Validate shape of framework requirements YAML.
#' @keywords internal
.autoiml_validate_framework_requirements = function(x) {
  out = list(ok = TRUE, errors = character())

  if (!is.list(x)) {
    return(list(ok = FALSE, errors = "framework_requirements: root must be a list"))
  }

  reqs = x$requirements
  if (!is.list(reqs) || length(reqs) < 1L) {
    return(list(ok = FALSE, errors = "framework_requirements: 'requirements' must be a non-empty list"))
  }

  required_keys = c(
    "id", "gate", "requirement_text", "evidence_type",
    "severity_if_missing", "applicability", "artifact_fields", "tests_required"
  )

  allowed_gate = c("G0A", "G0B", "G1", "G2", "G3", "G4", "G5", "G6", "G7A", "G7B", "IEL", "REPORT")
  allowed_evidence = c("computed", "user_provided", "external_study")
  allowed_severity = c("fail", "warn", "skip")

  ids = character(0)

  for (i in seq_along(reqs)) {
    r = reqs[[i]]
    tag = paste0("requirements[[", i, "]]")

    if (!is.list(r)) {
      out$errors = c(out$errors, paste0(tag, " must be a list"))
      next
    }

    miss = setdiff(required_keys, names(r))
    if (length(miss) > 0L) {
      out$errors = c(out$errors, paste0(tag, " missing keys: ", paste(miss, collapse = ", ")))
    }

    rid = as.character(r$id %||% "")
    if (!nzchar(rid)) {
      out$errors = c(out$errors, paste0(tag, " has empty id"))
    } else {
      ids = c(ids, rid)
    }

    gate = as.character(r$gate %||% "")
    if (!gate %in% allowed_gate) {
      out$errors = c(out$errors, paste0(tag, " has invalid gate: ", gate))
    }

    ev = as.character(r$evidence_type %||% "")
    if (!ev %in% allowed_evidence) {
      out$errors = c(out$errors, paste0(tag, " has invalid evidence_type: ", ev))
    }

    sev = as.character(r$severity_if_missing %||% "")
    if (!sev %in% allowed_severity) {
      out$errors = c(out$errors, paste0(tag, " has invalid severity_if_missing: ", sev))
    }

    af = r$artifact_fields %||% NULL
    if (!is.null(af)) {
      af = as.character(unlist(af, use.names = FALSE))
      if (length(af) < 1L || any(!nzchar(af))) {
        out$errors = c(out$errors, paste0(tag, " must have non-empty artifact_fields"))
      }
    }
  }

  dup = unique(ids[duplicated(ids)])
  if (length(dup) > 0L) {
    out$errors = c(out$errors, paste0("Duplicate requirement IDs: ", paste(dup, collapse = ", ")))
  }

  out$ok = length(out$errors) == 0L
  out
}


#' Validate shape of IEL rules YAML.
#' @keywords internal
.autoiml_validate_iel_rules = function(x) {
  out = list(ok = TRUE, errors = character())

  if (!is.list(x)) {
    return(list(ok = FALSE, errors = "iel_rules: root must be a list"))
  }

  rules = x$rules
  if (!is.list(rules) || length(rules) < 1L) {
    return(list(ok = FALSE, errors = "iel_rules: 'rules' must be a non-empty list"))
  }

  allowed_scope = c("global", "local", "decision")
  allowed_level = c("IEL-0", "IEL-1", "IEL-2", "IEL-3")
  allowed_status = c("pass", "warn")
  ids = character(0)

  for (i in seq_along(rules)) {
    r = rules[[i]]
    tag = paste0("rules[[", i, "]]")

    if (!is.list(r)) {
      out$errors = c(out$errors, paste0(tag, " must be a list"))
      next
    }

    rid = as.character(r$id %||% "")
    if (!nzchar(rid)) {
      out$errors = c(out$errors, paste0(tag, " has empty id"))
    } else {
      ids = c(ids, rid)
    }

    scope = as.character(r$scope %||% "")
    if (!scope %in% allowed_scope) {
      out$errors = c(out$errors, paste0(tag, " has invalid scope: ", scope))
    }

    level = as.character(r$level %||% "")
    if (!level %in% allowed_level) {
      out$errors = c(out$errors, paste0(tag, " has invalid level: ", level))
    }

    rg = as.character(unlist(r$required_gates %||% character(), use.names = FALSE))
    if (length(rg) < 1L || any(!nzchar(rg))) {
      out$errors = c(out$errors, paste0(tag, " must provide non-empty required_gates"))
    }

    st = as.character(unlist(r$requires_any_status_in %||% character(), use.names = FALSE))
    if (length(st) < 1L || any(!st %in% allowed_status)) {
      out$errors = c(out$errors, paste0(tag, " requires_any_status_in must be subset of {pass,warn}"))
    }

    req_keys = r$requires_artifact_keys %||% NULL
    if (!is.null(req_keys)) {
      if (!is.list(req_keys) || length(req_keys) < 1L || is.null(names(req_keys)) || any(!nzchar(names(req_keys)))) {
        out$errors = c(out$errors, paste0(tag, " requires_artifact_keys must be a named list of gate ids -> artifact keys"))
      } else {
        bad_gate_refs = setdiff(names(req_keys), rg)
        if (length(bad_gate_refs) > 0L) {
          out$errors = c(out$errors, paste0(tag, " requires_artifact_keys references gates not listed in required_gates: ", paste(bad_gate_refs, collapse = ", ")))
        }

        for (gid in names(req_keys)) {
          keys = as.character(unlist(req_keys[[gid]], use.names = FALSE))
          if (length(keys) < 1L || any(!nzchar(keys))) {
            out$errors = c(out$errors, paste0(tag, " requires_artifact_keys$", gid, " must contain non-empty artifact key names"))
          }
        }
      }
    }
  }

  dup = unique(ids[duplicated(ids)])
  if (length(dup) > 0L) {
    out$errors = c(out$errors, paste0("Duplicate IEL rule IDs: ", paste(dup, collapse = ", ")))
  }

  out$ok = length(out$errors) == 0L
  out
}


#' Run traceability scaffold checks without mutating runtime behavior.
#' @keywords internal
.autoiml_traceability_status = function() {
  fr = tryCatch(.autoiml_framework_requirements(), error = function(e) e)
  ir = tryCatch(.autoiml_iel_rules(), error = function(e) e)

  out = list(
    framework_loaded = !inherits(fr, "error"),
    iel_rules_loaded = !inherits(ir, "error"),
    framework_path = if (!inherits(fr, "error")) fr$`_path` %||% NA_character_ else NA_character_,
    iel_rules_path = if (!inherits(ir, "error")) ir$`_path` %||% NA_character_ else NA_character_,
    framework_validation = if (!inherits(fr, "error")) .autoiml_validate_framework_requirements(fr) else list(ok = FALSE, errors = conditionMessage(fr)),
    iel_rules_validation = if (!inherits(ir, "error")) .autoiml_validate_iel_rules(ir) else list(ok = FALSE, errors = conditionMessage(ir))
  )

  out$ok = isTRUE(out$framework_validation$ok) && isTRUE(out$iel_rules_validation$ok)
  out
}
