
`%||%` = function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

# Vocabulary of the article (version 0.1.6). Property status: supported, contradicted, or open.
.csdg_property_statuses = c("supported", "contradicted", "open")

# Status values of gate results and report-card rows; "not_required" marks a gate that the claim does not require
# and that was not run, and "error" a computation that failed.
.csdg_gate_statuses = c(.csdg_property_statuses, "not_required", "error")
.csdg_statuses = .csdg_gate_statuses

# Status values of report-card rows: a gate that the claim does not require but that was run is "context"; it is
# reported only and has no property status (its computed result is kept in `diagnostic_status`).
.csdg_report_statuses = c(.csdg_gate_statuses, "context")

# Assessment of a claim; "not_applicable" is reserved for a claim placed outside the evaluation.
.csdg_claim_decisions = c("met", "not_met", "unresolved", "not_applicable")

# Decision after the assessment.
.csdg_claim_actions = c("retain", "revise", "withhold")

.csdg_evidence_roles = c("required_property", "established_counterevidence", "unresolved_threat", "context")

# Scope elements in the order of Table 2 of the article.
.csdg_scope_elements = c("quantity", "model", "procedure", "data", "meaning", "use")

# Stored claim-card fields that hold the scope elements (legacy field names, kept for export compatibility).
.csdg_scope_fields = c(
  quantity = "target",
  model = "model_scope",
  procedure = "explanation_design",
  data = "analytic_distribution",
  meaning = "meaning",
  use = "scientific_use"
)

.csdg_meanings = c("model_description", "population_claim", "causal_claim")

.csdg_model_scopes = c("fitted_model", "learner", "several_models", "unspecified")

.csdg_scope_relations = c("same", "narrower", "broader", "incomparable")
.csdg_claim_relations = .csdg_scope_relations

# Kinds of revision (Supplement A of the article): weakening, restriction without entailment, change of question.
.csdg_revision_kinds = c("unspecified", "logical_weakening", "restriction_without_entailment", "change_of_question")

.csdg_proposition_relations = c("unchecked", "same", "logical_weakening", "logical_strengthening", "incomparable")

.csdg_gate_status_labels = c(
  supported = "Supported",
  contradicted = "Contradicted",
  open = "Open",
  not_required = "Not required",
  context = "Context",
  error = "Error"
)

.csdg_gate_areas = c("Foundation of the claim", "Predictions, explanations, and decisions", "Extensions")

# Legacy scope-element names of versions up to 0.1.5 (stored in the `coordinates` field of a claim card).
.csdg_claim_coordinates = c(
  "target",
  "model_scope",
  "semantics",
  "analytic_distribution",
  "scientific_use",
  "explanation_design"
)

.csdg_gate_ids = c("G0a", "G0b", "G1", "G2", "G3a", "G3b", "G4", "G5", "G6a", "G6b", "G7a", "G7b")

# Requirement identifiers: the 12 gates plus the causal design ("CD"), which is not a gate but is required by a
# causal claim.
.csdg_requirement_ids = c(.csdg_gate_ids, "CD")

# Legacy value maps (old value -> new value).
.csdg_legacy_statuses = c(
  met = "supported",
  not_met = "contradicted",
  unresolved = "open",
  not_applicable = "not_required"
)

.csdg_legacy_model_scopes = c(
  selected_model = "fitted_model",
  cross_fitted_pipeline = "learner",
  near_equivalent_models = "several_models",
  model_class = "several_models"
)

.csdg_legacy_semantics = c(
  fitted_model_description = "model_description",
  hypothetical_model_query = "model_description",
  causal = "causal_claim",
  recourse = "causal_claim"
)

.csdg_legacy_coordinates = c(
  target = "quantity",
  model_scope = "model",
  explanation_design = "procedure",
  analytic_distribution = "data",
  semantics = "meaning",
  scientific_use = "use"
)

.csdg_legacy_relations = c(alternative_or_incomparable = "incomparable")

.csdg_legacy_revision_kinds = c(
  context_restriction = "restriction_without_entailment",
  scope_restriction = "restriction_without_entailment",
  estimand_change = "change_of_question"
)

.csdg_stop = function(..., call. = FALSE) {
  stop(sprintf(...), call. = call.)
}

.csdg_warn = function(..., call. = FALSE) {
  warning(sprintf(...), call. = call.)
}

.csdg_note = function(...) {
  message(sprintf(...))
}

.csdg_deprecation_state = new.env(parent = emptyenv())

# Emit a classed deprecation warning once per session and identifier.
.csdg_warn_once = function(id, message) {
  if (isTRUE(.csdg_deprecation_state[[id]])) {
    return(invisible(FALSE))
  }
  assign(id, TRUE, envir = .csdg_deprecation_state)
  warning(structure(
    list(message = message, call = NULL),
    class = c("mlr3autoiml_deprecated", "deprecatedWarning", "warning", "condition")
  ))
  invisible(TRUE)
}

.csdg_deprecate = function(old, new, what, id = paste(what, old)) {
  .csdg_warn_once(
    id,
    sprintf("%s \"%s\" is deprecated since mlr3autoiml 0.1.6; use \"%s\".", what, old, new)
  )
}

.csdg_reset_deprecations = function() {
  rm(list = ls(.csdg_deprecation_state, all.names = TRUE), envir = .csdg_deprecation_state)
  invisible(TRUE)
}

# Map a legacy value to its 0.1.6 value, with a deprecation warning; current values pass unchanged.
.csdg_map_legacy = function(x, map, what, warn = TRUE) {
  if (is.null(x)) return(x)
  hit = !is.na(x) & x %in% names(map)
  if (any(hit)) {
    if (warn) {
      for (value in unique(x[hit])) .csdg_deprecate(value, map[[value]], what)
    }
    x[hit] = unname(map[x[hit]])
  }
  x
}

.is_scalar_string = function(x, allow_na = FALSE) {
  is.character(x) && length(x) == 1L &&
    (allow_na || !is.na(x)) && (allow_na || nzchar(x))
}

.assert_scalar_string = function(x, name, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(TRUE))
  }
  checkmate::assert_string(x, min.chars = 1L, .var.name = name)
  invisible(TRUE)
}

.assert_named_list = function(x, name, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(TRUE))
  }
  checkmate::assert_list(x, .var.name = name)
  checkmate::assert_true(
    !length(x) || (!is.null(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x))),
    .var.name = name
  )
  invisible(TRUE)
}

.assert_choice = function(x, choices, name, multiple = FALSE, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(TRUE))
  }
  if (multiple) {
    checkmate::assert_character(x, any.missing = FALSE, min.len = 1L, unique = TRUE, .var.name = name)
    checkmate::assert_subset(x, choices, empty.ok = FALSE, .var.name = name)
  } else {
    checkmate::assert_choice(x, choices, .var.name = name)
  }
  invisible(TRUE)
}

.assert_named_dots = function(x, name = "...") {
  checkmate::assert_list(x, .var.name = name)
  checkmate::assert_true(
    !length(x) || (!is.null(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x))),
    .var.name = name
  )
  invisible(TRUE)
}

.assert_optional_character = function(x, name) {
  if (!is.null(x)) {
    checkmate::assert_character(x, any.missing = FALSE, .var.name = name)
  }
  invisible(TRUE)
}

.recursive_modify = function(x, y) {
  if (is.null(y) || !length(y)) {
    return(x)
  }
  for (nm in names(y)) {
    if (is.list(x[[nm]]) && is.list(y[[nm]]) &&
        !inherits(x[[nm]], c("data.frame", "data.table"))) {
      x[[nm]] = .recursive_modify(x[[nm]], y[[nm]])
    } else {
      x[[nm]] = y[[nm]]
    }
  }
  x
}

.compact = function(x) {
  x[!vapply(x, is.null, logical(1))]
}

.safe_name = function(x) {
  x = gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
  x = gsub("_+", "_", x)
  x = sub("^_+", "", x)
  x = sub("_+$", "", x)
  ifelse(nzchar(x), x, "unnamed")
}

.as_dt = function(x, copy = TRUE) {
  if (data.table::is.data.table(x)) {
    if (copy) data.table::copy(x) else x
  } else {
    data.table::as.data.table(x)
  }
}

.is_mlr3_task = function(x) {
  inherits(x, "Task")
}

.is_mlr3_learner = function(x) {
  inherits(x, "Learner")
}

.require_task = function(task) {
  checkmate::assert_class(task, "Task", .var.name = "task")
  invisible(TRUE)
}

.require_learner = function(learner) {
  checkmate::assert_class(learner, "Learner", .var.name = "learner")
  invisible(TRUE)
}

.task_type = function(task) {
  task$task_type
}

.task_row_ids = function(task) {
  task$row_ids
}

.task_data = function(task, rows = NULL, cols = NULL) {
  .require_task(task)
  if (is.null(rows)) rows = task$row_ids
  if (is.null(cols)) cols = task$col_roles$feature
  task$data(rows = rows, cols = cols)
}

.task_truth = function(task, rows = NULL) {
  .require_task(task)
  if (is.null(rows)) rows = task$row_ids
  task$data(rows = rows, cols = task$target_names)[[task$target_names[[1L]]]]
}

.task_positive = function(task) {
  if (!identical(task$task_type, "classif")) return(NULL)
  task$positive
}

.normalize_named_vector = function(x, row_ids, name) {
  if (is.null(x)) return(NULL)
  checkmate::assert_atomic(x, .var.name = name)
  if (is.data.frame(x) || data.table::is.data.table(x)) {
    .csdg_stop("`%s` must be a vector; use a named data frame where documented.", name)
  }
  if (!is.null(names(x))) {
    idx = match(as.character(row_ids), names(x))
    if (anyNA(idx)) {
      .csdg_stop("Named `%s` does not cover every task row id.", name)
    }
    return(x[idx])
  }
  if (length(x) != length(row_ids)) {
    .csdg_stop(
      "`%s` has length %d; expected %d (one value per task row).",
      name, length(x), length(row_ids)
    )
  }
  x
}

.clone_resampling = function(resampling) {
  checkmate::assert_class(resampling, "Resampling", .var.name = "resampling")
  resampling$clone(deep = TRUE)
}

.default_resampling = function(task, config) {
  folds = config$resampling$folds
  repeats = config$resampling$repeats
  if (repeats > 1L) {
    rs = mlr3::rsmp("repeated_cv", folds = folds, repeats = repeats)
  } else {
    rs = mlr3::rsmp("cv", folds = folds)
  }
  rs$instantiate(task)
  rs
}

.default_measures = function(task) {
  if (identical(task$task_type, "regr")) {
    list(
      mlr3::msr("regr.rmse"),
      mlr3::msr("regr.mae"),
      mlr3::msr("regr.rsq")
    )
  } else if (identical(task$task_type, "classif")) {
    if (length(task$class_names) != 2L) {
      .csdg_stop(
        "CSDG classification diagnostics currently require binary classification."
      )
    }
    list(
      mlr3::msr("classif.auc"),
      mlr3::msr("classif.logloss"),
      mlr3::msr("classif.acc")
    )
  } else {
    .csdg_stop("Unsupported task type: %s.", task$task_type)
  }
}

.normalize_measures = function(measures, task) {
  if (is.null(measures)) return(.default_measures(task))
  if (inherits(measures, "Measure")) measures = list(measures)
  if (is.character(measures)) {
    measures = lapply(measures, mlr3::msr)
  }
  if (!is.list(measures) ||
      any(!vapply(measures, inherits, logical(1), what = "Measure"))) {
    .csdg_stop("`measures` must be measure keys, a Measure, or a list of Measures.")
  }
  measures
}

.measure_direction = function(measure) {
  if (!inherits(measure, "Measure")) {
    .csdg_stop("`measure` must inherit from mlr3::Measure.")
  }
  if (isTRUE(measure$minimize)) "minimize" else "maximize"
}

.get_probability_column = function(predictions, positive = NULL) {
  nms = names(predictions)
  if (!is.null(positive)) {
    candidates = c(
      paste0("prob.", positive),
      paste0("prob_", positive),
      positive
    )
    hit = candidates[candidates %in% nms]
    if (length(hit)) return(hit[[1L]])
  }
  prob_cols = grep("^prob[._]", nms, value = TRUE)
  if (length(prob_cols) == 1L) return(prob_cols)
  if ("prob" %in% nms) return("prob")
  if (length(prob_cols) > 1L) {
    .csdg_stop(
      "Multiple probability columns found; provide the positive class explicitly."
    )
  }
  .csdg_stop("No probability column found in predictions.")
}

.clip_probability = function(p, eps = 1e-15) {
  pmin(pmax(as.numeric(p), eps), 1 - eps)
}

.truth_to_event = function(truth, positive) {
  if (is.null(positive)) {
    if (is.logical(truth)) return(as.integer(truth))
    if (is.numeric(truth) && all(stats::na.omit(unique(truth)) %in% c(0, 1))) {
      return(as.integer(truth))
    }
    .csdg_stop("`positive` is required when truth is not coded as 0/1.")
  }
  as.integer(as.character(truth) == as.character(positive))
}

.aggregate_repeated_predictions = function(predictions, positive = NULL) {
  dt = .as_dt(predictions)
  if (!"row_id" %in% names(dt)) {
    if ("row_ids" %in% names(dt)) {
      data.table::setnames(dt, "row_ids", "row_id")
    } else {
      .csdg_stop("Predictions must contain `row_id`.")
    }
  }
  if (!"truth" %in% names(dt)) {
    .csdg_stop("Predictions must contain `truth`.")
  }

  prob_cols = grep("^prob[._]", names(dt), value = TRUE)
  numeric_response = "response" %in% names(dt) &&
    is.numeric(dt$response) && !length(prob_cols)
  avg_cols = unique(c(prob_cols, if (numeric_response) "response"))
  drop_cols = c("row_id", avg_cols, "iteration", "fold", "repetition")
  first_cols = setdiff(names(dt), drop_cols)

  if (length(first_cols)) {
    first_part = dt[
      ,
      lapply(.SD, function(z) z[[1L]]),
      by = row_id,
      .SDcols = first_cols
    ]
  } else {
    first_part = unique(dt[, .(row_id)])
  }
  if (!length(avg_cols)) return(first_part)
  avg_part = dt[
    ,
    lapply(.SD, function(z) {
      z = as.numeric(z)
      if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
    }),
    by = row_id,
    .SDcols = avg_cols
  ]
  merge(first_part, avg_part, by = "row_id", all = TRUE, sort = FALSE)
}

.predict_newdata = function(learner, newdata, task = NULL) {
  .require_learner(learner)
  out = tryCatch(
    learner$predict_newdata(newdata = newdata, task = task),
    error = function(e1) {
      tryCatch(
        learner$predict_newdata(newdata = newdata),
        error = function(e2) {
          .csdg_stop(
            "Prediction on new data failed. First error: %s; second error: %s",
            conditionMessage(e1), conditionMessage(e2)
          )
        }
      )
    }
  )
  out
}

.prediction_vector = function(prediction, task_type, positive = NULL) {
  dt = data.table::as.data.table(prediction)
  if (identical(task_type, "regr")) {
    return(as.numeric(dt$response))
  }
  if (identical(task_type, "classif")) {
    col = .get_probability_column(dt, positive)
    return(as.numeric(dt[[col]]))
  }
  .csdg_stop("Unsupported task type: %s.", task_type)
}

.compute_loss = function(truth, prediction, task_type, loss, positive = NULL) {
  loss = loss %||% if (identical(task_type, "regr")) "rmse" else "logloss"
  if (identical(task_type, "regr")) {
    y = as.numeric(truth)
    p = as.numeric(prediction)
    ok = is.finite(y) & is.finite(p)
    if (!any(ok)) return(NA_real_)
    if (identical(loss, "rmse")) return(sqrt(mean((y[ok] - p[ok])^2)))
    if (identical(loss, "mse")) return(mean((y[ok] - p[ok])^2))
    if (identical(loss, "mae")) return(mean(abs(y[ok] - p[ok])))
    .csdg_stop("Unsupported regression loss: %s.", loss)
  }

  y = .truth_to_event(truth, positive)
  p = .clip_probability(prediction)
  ok = is.finite(y) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  if (identical(loss, "logloss")) {
    return(-mean(y[ok] * log(p[ok]) + (1 - y[ok]) * log(1 - p[ok])))
  }
  if (identical(loss, "brier")) return(mean((y[ok] - p[ok])^2))
  if (identical(loss, "error")) return(mean((p[ok] >= 0.5) != y[ok]))
  .csdg_stop("Unsupported classification loss: %s.", loss)
}

.auc_rank = function(y, p) {
  ok = is.finite(y) & is.finite(p)
  y = y[ok]
  p = p[ok]
  n1 = sum(y == 1)
  n0 = sum(y == 0)
  if (!n1 || !n0) return(NA_real_)
  ranks = rank(p, ties.method = "average")
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

.now_utc = function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}

.hash_file = function(path) {
  unname(tools::md5sum(path))
}

.hash_object = function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

.atomic_dir = function(path, code) {
  parent = dirname(path)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  tmp = tempfile(pattern = paste0(".", basename(path), "-"), tmpdir = parent)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  backup = NULL
  ok = FALSE
  on.exit({
    if (!ok && dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
    if (!ok && !is.null(backup) && file.exists(backup) && !file.exists(path)) {
      file.rename(backup, path)
    }
  }, add = TRUE)
  force(code)(tmp)
  if (file.exists(path)) {
    backup = tempfile(pattern = paste0(".", basename(path), "-previous-"), tmpdir = parent)
    if (!file.rename(path, backup)) {
      .csdg_stop("Could not preserve the existing audit bundle at `%s`.", path)
    }
  }
  if (!file.rename(tmp, path)) {
    restored = !is.null(backup) && file.exists(backup) && file.rename(backup, path)
    .csdg_stop(
      "Could not atomically move the new audit bundle into `%s`.%s",
      path,
      if (restored) " The previous bundle was restored." else ""
    )
  }
  ok = TRUE
  if (!is.null(backup) && file.exists(backup)) {
    unlink(backup, recursive = TRUE, force = TRUE)
  }
  invisible(path)
}

.write_json = function(x, path, pretty = TRUE) {
  jsonlite::write_json(
    .csdg_json_arrays(x), path = path, pretty = pretty, auto_unbox = TRUE,
    null = "null", na = "null", digits = NA
  )
}

.csdg_json_arrays = function(x) {
  if (!is.list(x) || is.data.frame(x)) return(x)
  vector_fields = c(
    "evidence_ids", "varied_component", "held_constant", "blocking_gate_ids", "unresolved_gate_ids",
    "contradicted_gate_ids", "open_gate_ids", "counterevidence_gate_ids", "threat_gate_ids", "context_gate_ids",
    "decision_options"
  )
  for (i in seq_along(x)) {
    if (is.null(x[[i]])) next
    field = if (is.null(names(x))) "" else names(x)[[i]]
    x[[i]] = if (field %in% vector_fields && is.atomic(x[[i]])) {
      I(x[[i]])
    } else {
      .csdg_json_arrays(x[[i]])
    }
  }
  x
}

.write_csv = function(x, path) {
  data.table::fwrite(.as_dt(x), file = path, na = "")
}


.package_version = function() {
  tryCatch(
    as.character(utils::packageVersion("mlr3autoiml")),
    error = function(e) "development"
  )
}

.capture_session_info = function() {
  paste(utils::capture.output(utils::sessionInfo()), collapse = "\n")
}

.flatten_evidence = function(x, prefix = character()) {
  out = list()
  if (is.data.frame(x) || data.table::is.data.table(x)) {
    if (!ncol(x)) return(out)
    key = paste(prefix, collapse = "__")
    out[[if (nzchar(key)) key else "table"]] = x
    return(out)
  }
  if (!is.list(x)) return(out)
  nms = names(x)
  if (is.null(nms)) nms = sprintf("item_%02d", seq_along(x))
  for (i in seq_along(x)) {
    child = .flatten_evidence(x[[i]], c(prefix, .safe_name(nms[[i]])))
    out = c(out, child)
  }
  out
}

.card_to_list = function(x) {
  if (!is.list(x)) {
    .csdg_stop("A CSDG card must be a list.")
  }
  card_names = names(x)
  if (is.null(card_names) || anyNA(card_names) || any(!nzchar(card_names)) || anyDuplicated(card_names)) {
    .csdg_stop("A CSDG card must be a uniquely named list.")
  }
  y = unclass(x)
  attributes(y) = NULL
  names(y) = card_names
  y
}

.extract_prediction_table = function(prediction, iteration, repetition = NA_integer_,
                                      fold = NA_integer_) {
  dt = data.table::as.data.table(prediction)
  if ("row_ids" %in% names(dt) && !"row_id" %in% names(dt)) {
    data.table::setnames(dt, "row_ids", "row_id")
  }
  if (!"row_id" %in% names(dt)) {
    .csdg_stop("mlr3 prediction did not expose row ids.")
  }
  dt[, `:=`(
    iteration = as.integer(iteration),
    repetition = as.integer(repetition),
    fold = as.integer(fold)
  )]
  dt
}

.resampling_repeat_fold = function(resampling, iteration) {
  folds = tryCatch(resampling$param_set$values$folds, error = function(e) NA_integer_)
  repeats = tryCatch(resampling$param_set$values$repeats, error = function(e) 1L)
  if (is.null(folds) || !length(folds) || is.na(folds)) {
    return(c(repetition = 1L, fold = as.integer(iteration)))
  }
  c(
    repetition = as.integer(((iteration - 1L) %/% folds) + 1L),
    fold = as.integer(((iteration - 1L) %% folds) + 1L)
  )
}

.status_rank = function(status) {
  match(status, c("error", "contradicted", "open", "supported", "not_required"))
}
