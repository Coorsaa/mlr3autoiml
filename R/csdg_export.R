
#' @rdname csdg_reporting
#' @export
csdg_report_card = function(x) {
  checkmate::assert_class(x, "CSDGResult")
  plan = data.table::copy(data.table::as.data.table(x$plan))
  registry = .gate_registry()
  for (column in c("gate_name", "area", "evidence_question", "required_if", "typical_diagnostic")) {
    if (!column %in% names(plan)) {
      plan = merge(plan, registry[, c("gate_id", column), with = FALSE], by = "gate_id", all.x = TRUE, sort = FALSE)
    }
  }
  if (!"plan_role" %in% names(plan)) {
    plan[, plan_role := data.table::fifelse(required, "required", "not_required")]
  }
  # Every required gate is a required property; a gate that the claim does not require is context.
  plan[, evidence_role := data.table::fifelse(required, "required_property", "context")]
  rows = data.table::rbindlist(lapply(x$gates, function(gate) {
    data.table::data.table(
      gate_id = gate$gate_id,
      status = .csdg_map_legacy(gate$status, .csdg_legacy_statuses, "Gate status", warn = FALSE),
      availability = gate$availability,
      result_direction = gate$result_direction,
      criterion = if (is.null(gate$criterion)) {
        ""
      } else {
        as.character(toJSON(gate$criterion, auto_unbox = TRUE))
      },
      criterion_source = gate$criterion_source %||% NA_character_,
      criterion_rationale = gate$criterion_rationale %||% NA_character_,
      rationale = gate$rationale,
      summary = gate$summary,
      limitations = paste(gate$limitations, collapse = " | "),
      started_at = gate$started_at %||% NA_character_,
      completed_at = gate$completed_at %||% NA_character_,
      materiality = gate$materiality %||% "not_applicable",
      adjudication_basis = gate$adjudication_basis %||% NA_character_,
      claim_consequence = gate$claim_consequence %||% "none"
    )
  }), fill = TRUE)
  out = merge(plan, rows, by = "gate_id", all.x = TRUE, sort = FALSE)
  # A gate that the claim does not require is context: it is reported but has no property status. Its computed
  # result is kept in `diagnostic_status`; `status` is "context" if the gate was run and "not_required" otherwise.
  out[, diagnostic_status := data.table::fifelse(is.na(status), "not_required", status)]
  out[, status := data.table::fifelse(
    required %in% TRUE,
    status,
    data.table::fifelse(diagnostic_status == "not_required", "not_required", "context")
  )]
  out[, gate_order__ := match(gate_id, .csdg_gate_ids)]
  data.table::setorder(out, gate_order__)
  out[, gate_order__ := NULL]
  first = c(
    "gate_id", "gate_name", "area", "evidence_question", "required_if", "required", "plan_role",
    "evidence_role", "status", "diagnostic_status", "availability", "result_direction", "criterion",
    "criterion_source", "criterion_rationale", "rationale", "summary", "limitations", "started_at", "completed_at"
  )
  data.table::setcolorder(out, c(intersect(first, names(out)), setdiff(names(out), first)))
  out[]
}

#' @rdname csdg_reporting
#' @export
csdg_claim_report = function(x, evidence = NULL) {
  checkmate::assert_class(x, "CSDGResult")
  if (!is.null(evidence)) {
    checkmate::assert_list(evidence, .var.name = "evidence")
    if (any(!vapply(evidence, inherits, logical(1L), what = "CSDGEvidenceRecord"))) {
      .csdg_stop("Every element of `evidence` must be a CSDGEvidenceRecord.")
    }
  }
  card = csdg_report_card(x)
  required = card[required == TRUE]
  supplied = evidence %||% list()
  recorded = unique(unlist(lapply(supplied, function(record) {
    if (isTRUE(record$applicable) && record$role %in% c("required_property", "established_counterevidence")) {
      record$gate_id
    }
  })))
  audit_records = lapply(seq_len(nrow(required)), function(index) {
    row = required[index]
    if (row$gate_id %in% recorded) return(NULL)
    status = if (row$status %in% .csdg_property_statuses) row$status else "open"
    csdg_evidence_record(
      row$gate_id, TRUE, "required_property",
      status = status,
      availability = if (identical(status, "open")) {
        if (identical(row$status, "error")) "unavailable" else "incomplete"
      } else {
        "complete"
      },
      rationale = paste0("Audit diagnostic (", row$status, "): ", row$summary)
    )
  })
  audit_records = Filter(Negate(is.null), audit_records)
  plan = data.table::copy(data.table::as.data.table(x$plan))
  data.table::setattr(plan, "causal_design_required", identical(.csdg_claim_meaning(x$claim), "causal_claim"))
  adjudication = csdg_adjudicate_claim(c(audit_records, supplied), claim_applicable = TRUE, plan = plan)
  properties = adjudication$properties
  collect = function(value) paste(properties$gate_id[properties$status == value], collapse = ", ")
  origin = x$claim$provenance$origin %||% NA_character_
  exploratory = identical(origin, "retrospective_exploratory")
  scope = x$claim$scope %||% list(
    quantity = x$claim$target, model = x$claim$model_scope, procedure = x$claim$explanation_design,
    data = x$claim$analytic_distribution, meaning = .csdg_claim_meaning(x$claim), use = x$claim$scientific_use
  )
  scope_text = paste(vapply(.csdg_scope_elements, function(element) {
    paste0(tools::toTitleCase(element), ": ", paste(scope[[element]] %||% "unspecified", collapse = " "))
  }, character(1L)), collapse = " | ")
  assessment = adjudication$assessment
  assessment_basis = if (length(supplied)) {
    "audit diagnostics under the configured criteria and supplied evidence records"
  } else {
    "audit diagnostics under the configured criteria"
  }
  data.table::data.table(
    claim_id = x$claim$id,
    claim_version = x$claim$claim_version,
    parent_claim_id = x$claim$parent_claim_id %||% NA_character_,
    revision_relation = x$claim$revision_relation,
    claim_statement = x$claim$statement,
    origin = origin,
    assessment = assessment,
    assessment_label = paste0(gsub("_", " ", assessment), if (exploratory) " (exploratory)" else ""),
    exploratory = exploratory,
    decision_options = paste(adjudication$decision_options, collapse = " or "),
    required_gates = paste(properties$gate_id, collapse = ", "),
    supported_gates = collect("supported"),
    contradicted_gates = collect("contradicted"),
    open_gates = collect("open"),
    error_gates = paste(required[status == "error", gate_id], collapse = ", "),
    counterevidence_gates = paste(adjudication$counterevidence_gate_ids, collapse = ", "),
    threat_gates = paste(adjudication$threat_gate_ids, collapse = ", "),
    assessment_basis = assessment_basis,
    scope = scope_text,
    interpretation = paste(
      "Decision rule: not met if a required property is contradicted; otherwise unresolved if one is open;",
      "otherwise met. Favorable results never offset a contradicted property, and context never changes the",
      "assessment. Properties that the audit cannot judge, such as whether the procedure computes the quantity",
      "the claim names (G2), remain open until the researcher records them with csdg_evidence_record()."
    ),
    # Deprecated aliases kept for compatibility with versions up to 0.1.5: `decision` and `decision_basis` hold
    # the assessment and its basis (the decision itself is retain, revise, or withhold; see `decision_options`).
    decision = assessment,
    decision_basis = assessment_basis,
    met_gates = collect("supported"),
    not_met_gates = collect("contradicted"),
    unresolved_gates = collect("open")
  )
}

.write_nested_evidence = function(x, dir, stem) {
  tables = .flatten_evidence(x)
  files = character()
  if (length(tables)) {
    for (nm in names(tables)) {
      file = file.path(dir, paste0(stem, "__", .safe_name(nm), ".csv"))
      .write_csv(tables[[nm]], file)
      files = c(files, file)
    }
  }
  json_file = file.path(dir, paste0(stem, ".json"))
  serializable = tryCatch({
    .write_json(x, json_file)
    TRUE
  }, error = function(e) FALSE)
  if (serializable) {
    files = c(files, json_file)
  } else if (file.exists(json_file)) {
    unlink(json_file)
  }
  files
}

.bundle_readme = function(result) {
  card = csdg_report_card(result)
  report = csdg_claim_report(result)
  paste0(
    "# CSDG audit bundle\n\n",
    "Created: ", result$metadata$created_at, "\n\n",
    "Claim: **", result$claim$statement, "**\n\n",
    "Claim version: **", result$claim$claim_version, "**\n\n",
    "Assessment from the audit diagnostics: **", report$assessment_label, "**\n\n",
    "## Required properties and their status\n\n",
    paste0(
      "- ", card$gate_id, " ", card$gate_name, " (", gsub("_", " ", card$evidence_role), "): `", card$status,
      "` - ", card$summary, collapse = "\n"
    ),
    "\n\n## Decision rule\n\n",
    "A claim is not met if at least one required property is contradicted; otherwise it is unresolved if at least ",
    "one is open; otherwise it is met. Favorable results never offset a contradicted property, and context never ",
    "changes the assessment. No aggregate score is computed. A revised claim is recorded as a new, linked claim. ",
    "The records do not establish causal validity, population representativeness, clinical utility, fairness, or ",
    "deployment readiness.\n"
  )
}

.uncertainty_note = function() {
  paste(
    "# Uncertainty represented in this bundle",
    "",
    "Resampling-iteration summaries describe variation across fitted folds.",
    "They are not interpreted as estimates from independent samples and are not",
    "labeled as confidence intervals. A bootstrap of stored out-of-fold",
    "predictions, when included in subgroup evidence, is conditional on those",
    "predictions and does not reproduce uncertainty from preprocessing, tuning,",
    "feature selection, or model fitting unless the entire pipeline is rerun",
    "inside each bootstrap sample. Plausible-value variation or variation across",
    "aligned outcomes must likewise be described according to its data-generating",
    "role rather than automatically treated as sampling uncertainty.",
    sep = "\n"
  )
}


.local_faithfulness_aggregate = function(x) {
  if (!is.list(x) || !is.data.frame(x$summary) || !"case_id" %in% names(x$summary)) {
    return(NULL)
  }
  tab = data.table::as.data.table(x$summary)
  finite_mean = function(values) {
    values = as.numeric(values)
    values = values[is.finite(values)]
    if (length(values)) mean(values) else NA_real_
  }
  finite_min = function(values) {
    values = as.numeric(values)
    values = values[is.finite(values)]
    if (length(values)) min(values) else NA_real_
  }
  data.table::data.table(
    n_cases = data.table::uniqueN(tab$case_id),
    n_evaluations = nrow(tab),
    mean_weighted_r2 = if ("weighted_r2" %in% names(tab)) finite_mean(tab$weighted_r2) else NA_real_,
    minimum_weighted_r2 = if ("weighted_r2" %in% names(tab)) finite_min(tab$weighted_r2) else NA_real_,
    mean_weighted_rmse = if ("weighted_rmse" %in% names(tab)) finite_mean(tab$weighted_rmse) else NA_real_
  )
}

.normalize_export_field_names = function(fields) {
  snake_case = gsub("(?<=[a-z0-9])(?=[A-Z])", "_", fields, perl = TRUE)
  gsub("[^a-z0-9]+", "_", tolower(snake_case))
}

.export_private_field_names = function(fields, generic_identifiers = TRUE) {
  normalized = .normalize_export_field_names(fields)
  generic = c("id", "ids", "uuid", "uuids", "identifier", "identifiers")
  direct_identifiers = c(
    "student_id", "student_ids", "school_id", "school_ids", "respondent_id", "respondent_ids",
    "participant_id", "participant_ids", "person_id", "person_ids", "individual_id", "individual_ids",
    "record_id", "record_ids", "subject_id", "subject_ids", "user_id", "user_ids", "patient_id", "patient_ids"
  )
  names = c(
    "first_name", "first_names", "middle_name", "middle_names", "last_name", "last_names", "full_name", "full_names",
    "given_name", "given_names", "family_name", "family_names", "surname", "surnames"
  )
  contacts = c(
    "email", "emails", "email_address", "email_addresses", "phone", "phones", "phone_number", "phone_numbers",
    "telephone", "telephones", "telephone_number", "telephone_numbers", "mobile", "mobiles", "mobile_number",
    "mobile_numbers", "cell", "cells", "cell_number", "cell_numbers", "e_mail", "e_mails", "e_mail_address",
    "e_mail_addresses"
  )
  addresses = c(
    "address", "addresses", "street_address", "street_addresses", "mailing_address", "mailing_addresses",
    "residential_address", "residential_addresses", "home_address", "home_addresses", "work_address",
    "work_addresses", "address_line_1", "address_line_2", "postal_address", "postal_addresses", "postal_code",
    "postal_codes", "postcode", "postcodes", "zip", "zips", "zip_code", "zip_codes", "zipcode", "zipcodes"
  )
  births = c(
    "date_of_birth", "dates_of_birth", "birth_date", "birth_dates", "birthdate", "birthdates", "dob", "dobs"
  )
  credentials = c(
    "social_security_number", "social_security_numbers", "ssn", "ssns", "national_id", "national_ids",
    "passport_number", "passport_numbers", "driver_license_number", "driver_license_numbers", "government_id",
    "government_ids", "government_identifier", "government_identifiers", "account_id", "account_ids",
    "account_identifier", "account_identifiers", "account_number", "account_numbers", "tax_id", "tax_ids",
    "tax_identifier", "tax_identifiers"
  )
  device_and_location = c(
    "ip", "ip_address", "ip_addresses", "mac_address", "mac_addresses", "device_id", "device_ids",
    "device_identifier", "device_identifiers", "advertising_id", "advertising_ids", "gps_coordinate",
    "gps_coordinates", "home_latitude", "home_longitude"
  )
  person_prefix = paste0(
    "^(student|school|respondent|participant|person|individual|record|subject|user|patient)_",
    "(id|ids|key|keys|uuid|uuids|index|indices|code|codes|identifier|identifiers|hash|hashes|token|tokens|",
    "name|names|email|emails|phone|phones|address|addresses)$"
  )
  private = normalized %in% c(
    if (isTRUE(generic_identifiers)) generic else character(),
    direct_identifiers,
    names,
    contacts,
    addresses,
    births,
    credentials,
    device_and_location
  ) | grepl(person_prefix, normalized)
  unique(fields[private])
}

.export_observed_field_names = function(fields) {
  normalized = .normalize_export_field_names(fields)
  observed_records = c(
    "truth", "actual", "actual_value", "actual_outcome", "actual_response", "observed", "observed_value",
    "observed_outcome", "observed_response", "true_label", "y_true"
  )
  unique(fields[normalized %in% observed_records])
}

.export_fitted_field_names = function(fields) {
  normalized = .normalize_export_field_names(fields)
  fitted_records = c(
    "prediction", "predictions", "pred", "prob",
    "probability", "predicted_value", "predicted_values", "predicted_score", "predicted_scores", "prediction_score",
    "prediction_scores", "predicted_probability", "predicted_probabilities", "class_probability",
    "class_probabilities", "fitted_value", "fitted_values", "yhat", "yhats", "y_pred", "y_preds", "residual",
    "residuals", "observation_level"
  )
  unique(fields[normalized %in% fitted_records])
}

.export_prediction_table_columns = function(columns) {
  normalized = .normalize_export_field_names(columns)
  row_identifiers = c("row_id", "row_ids", "case_id", "case_ids")
  split_membership = c(
    "fold_assignment", "fold_assignments", "fold_id", "fold_ids", "split_id", "split_ids", "train_set",
    "train_sets", "test_set", "test_sets", "train_indices", "test_indices", "row_map"
  )
  unique(c(
    columns[normalized %in% c(row_identifiers, split_membership)],
    .export_observed_field_names(columns),
    .export_fitted_field_names(columns)
  ))
}

.export_sensitive_table_columns = function(columns) {
  unique(c(.export_private_field_names(columns), .export_prediction_table_columns(columns)))
}

.export_is_prediction_record = function(x) {
  if (!is.list(x) || is.null(names(x))) {
    return(FALSE)
  }
  length(.export_observed_field_names(names(x))) > 0L && length(.export_fitted_field_names(names(x))) > 0L
}

.export_is_fitted_model = function(x) {
  known_classes = c(
    "lm", "glm", "nls", "rpart", "randomForest", "ranger", "xgb.Booster", "lgb.Booster", "gbm", "glmnet",
    "cv.glmnet", "train", "workflow", "model_fit", "gam", "survfit", "stanfit", "WrappedModel"
  )
  classes = class(x)
  inherits(x, known_classes) || any(grepl("(model|fit|booster|learner|forest)$", tolower(classes)))
}

.export_model_container_names = function(fields) {
  normalized = .normalize_export_field_names(fields)
  unique(fields[grepl("(^|_)(model|models|learner|learners|fit|fits|fitted_object|booster|boosters)$", normalized)])
}

.export_private_container_names = function(fields) {
  normalized = .normalize_export_field_names(fields)
  explicit = c(
    "pii", "personal_information", "personally_identifiable_information", "contact_information", "contact_details",
    "private_data", "protected_data", "raw_data"
  )
  person_container = paste0(
    "^(student|school|respondent|participant|person|individual|record|subject|user|patient)_",
    "(contact|contacts|contact_details|profile|profiles|record|records|details|identifiers)$"
  )
  unique(fields[normalized %in% explicit | grepl(person_container, normalized)])
}

.export_prediction_container_names = function(fields) {
  normalized = .normalize_export_field_names(fields)
  pattern = paste0(
    "^(prediction|predictions|prediction_record|prediction_records|",
    "(?:oof|row|case|individual|observation)_(?:scores?|predictions?|prediction_records?))$"
  )
  unique(fields[grepl(pattern, normalized, perl = TRUE)])
}

.strip_export_attributes = function(x) {
  current = attributes(x)
  if (is.null(current)) {
    return(x)
  }
  allowed = c("names", "class", "row.names", "dim", "dimnames", "levels", "tzone", "units")
  attributes(x) = current[intersect(names(current), allowed)]
  x
}

.export_fields_require_removal = function(fields, include_predictions, allow_generic_identifiers = FALSE) {
  length(.export_private_field_names(fields, generic_identifiers = !allow_generic_identifiers)) > 0L ||
    (!isTRUE(include_predictions) && length(.export_prediction_table_columns(fields)) > 0L)
}

.export_contains_private_object = function(x, include_models, include_predictions) {
  if (inherits(x, c("Task", "DataBackend", "Resampling"))) {
    return(TRUE)
  }
  if (inherits(x, "Learner")) {
    return(!isTRUE(include_models))
  }
  if (inherits(x, "Prediction")) {
    return(!isTRUE(include_predictions))
  }
  if (.export_is_fitted_model(x)) {
    return(!isTRUE(include_models))
  }
  if (is.environment(x)) {
    return(TRUE)
  }
  if (!is.list(x)) {
    return(FALSE)
  }
  any(vapply(
    seq_along(x),
    function(index) .export_contains_private_object(x[[index]], include_models, include_predictions),
    logical(1L)
  ))
}

.sanitize_export_object = function(x, include_models, include_predictions, allow_generic_identifiers = FALSE) {
  if (inherits(x, c("Task", "DataBackend", "Resampling"))) {
    return(NULL)
  }
  if (inherits(x, "Learner")) {
    return(if (isTRUE(include_models)) x else NULL)
  }
  if (inherits(x, "Prediction")) {
    return(if (isTRUE(include_predictions)) x else NULL)
  }
  if (.export_is_fitted_model(x)) {
    return(if (isTRUE(include_models)) x else NULL)
  }
  if (isS4(x)) {
    return(NULL)
  }
  if (is.environment(x)) {
    return(NULL)
  }
  x = .strip_export_attributes(x)
  if (is.data.frame(x) || is.data.table(x)) {
    private_columns = .export_private_field_names(names(x))
    prediction_columns = .export_prediction_table_columns(names(x))
    if (length(private_columns) || (!isTRUE(include_predictions) && length(prediction_columns))) {
      return(NULL)
    }
    contains_private_object = any(vapply(
      x,
      .export_contains_private_object,
      logical(1L),
      include_models = include_models,
      include_predictions = include_predictions
    ))
    if (contains_private_object) {
      return(NULL)
    }
    out_table = x
    row.names(out_table) = NULL
    return(out_table)
  }
  if (is.matrix(x)) {
    fields = colnames(x) %||% character()
    if (.export_fields_require_removal(fields, include_predictions)) {
      return(NULL)
    }
    matrix_values = as.vector(x)
    if (is.list(matrix_values) && any(vapply(
      matrix_values,
      .export_contains_private_object,
      logical(1L),
      include_models = include_models,
      include_predictions = include_predictions
    ))) {
      return(NULL)
    }
    out_matrix = x
    rownames(out_matrix) = NULL
    return(out_matrix)
  }
  if (is.array(x)) {
    dimension_labels = unlist(dimnames(x), use.names = FALSE)
    dimension_fields = c(names(dimnames(x)), dimension_labels)
    if (.export_fields_require_removal(dimension_fields, include_predictions)) {
      return(NULL)
    }
    array_values = as.vector(x)
    if (is.list(array_values) && any(vapply(
      array_values,
      .export_contains_private_object,
      logical(1L),
      include_models = include_models,
      include_predictions = include_predictions
    ))) {
      return(NULL)
    }
    out_array = x
    dimnames(out_array) = NULL
    return(out_array)
  }
  if (!is.list(x) && !is.null(names(x)) && .export_fields_require_removal(
    names(x),
    include_predictions,
    allow_generic_identifiers
  )) {
    return(NULL)
  }
  if (!is.list(x)) return(x)

  if (.export_is_prediction_record(x)) {
    if (length(.export_private_field_names(names(x))) || !isTRUE(include_predictions)) {
      return(NULL)
    }
  }

  out = x
  if (!isTRUE(include_predictions)) {
    local_aggregate = .local_faithfulness_aggregate(out)
    if (!is.null(local_aggregate)) {
      out$aggregate_summary = local_aggregate
      out$summary = NULL
      out$coefficients = NULL
    }
  }
  item_names = names(out)
  if (is.null(item_names)) {
    item_names = rep("", length(out))
  }
  for (index in rev(seq_along(out))) {
    name = item_names[[index]]
    safe_scalar_description = name %in% c("weights", "clusters") &&
      test_string(out[[index]], min.chars = 1L)
    # The scope element `model` of a claim (for example "fitted_model" or "learner") is a character
    # description of the model scope, not a fitted model object, and must survive the export.
    scope_model_description = identical(name, "model") && test_string(out[[index]], min.chars = 1L)
    remove = length(.export_private_field_names(name, generic_identifiers = !allow_generic_identifiers)) > 0L ||
      length(.export_private_container_names(name)) > 0L ||
      (!isTRUE(include_predictions) && length(.export_prediction_table_columns(name)) > 0L) ||
      name %in% c("task", "backend", "backends", "resampling") ||
      (!isTRUE(include_models) && !scope_model_description &&
        length(.export_model_container_names(name)) > 0L) ||
      (!isTRUE(include_predictions) && length(.export_prediction_container_names(name)) > 0L) ||
      (!isTRUE(include_predictions) &&
        name %in% c(
          "observation_level",
          "train_sets", "test_sets",
          "row_map", "assignments", "group", "row_id", "row_ids", "case_id", "case_ids",
          "case", "cases", "case_data", "local_cases", "background", "additional"
        )) ||
      (!isTRUE(include_predictions) && name %in% c("weights", "clusters") && !safe_scalar_description)
    if (remove) {
      out[index] = NULL
    } else if (!is.null(out[[index]])) {
      sanitized = .sanitize_export_object(
        out[[index]],
        include_models = include_models,
        include_predictions = include_predictions,
        allow_generic_identifiers = FALSE
      )
      if (is.null(sanitized) || (is.list(sanitized) && !length(sanitized) && !nzchar(name))) {
        out[index] = NULL
      } else {
        out[[index]] = sanitized
      }
    }
  }
  out
}

.sanitize_export_plan = function(plan, include_models, include_predictions) {
  allowed = c(
    "gate_id", "gate_name", "area", "evidence_question", "required_if", "required", "plan_role",
    "evidence_role", "trigger", "typical_diagnostic", "required_components", "applicable", "applicability",
    "execute"
  )
  causal_design_required = isTRUE(attr(plan, "causal_design_required"))
  plan = copy(as.data.table(plan))
  plan = plan[, intersect(allowed, names(plan)), with = FALSE]
  sanitized = .sanitize_export_object(
    plan,
    include_models = include_models,
    include_predictions = include_predictions
  )
  if (is.null(sanitized)) {
    .csdg_stop("The gate plan contains a private or unsupported object and cannot be exported safely.")
  }
  sanitized = structure(sanitized, class = c("CSDGGatePlan", class(sanitized)))
  data.table::setattr(sanitized, "causal_design_required", causal_design_required)
  sanitized
}

.export_measure_ids = function(x) {
  if (inherits(x, "Measure")) {
    return(x$id)
  }
  if (is.list(x)) {
    return(lapply(x, .export_measure_ids))
  }
  x
}

.sanitize_result_for_export = function(x, include_models, include_predictions) {
  out = x
  out$claim = .sanitize_export_object(
    out$claim,
    include_models = include_models,
    include_predictions = include_predictions,
    allow_generic_identifiers = TRUE
  )
  out$measurement = .sanitize_export_object(
    out$measurement,
    include_models = include_models,
    include_predictions = include_predictions
  )
  out$explanation = .sanitize_export_object(
    out$explanation,
    include_models = include_models,
    include_predictions = include_predictions
  )
  export_config = out$config
  export_config$performance$measures = .export_measure_ids(export_config$performance$measures)
  export_config$performance$primary = .export_measure_ids(export_config$performance$primary)
  config_resampling = .sanitize_export_object(
    export_config$resampling,
    include_models = include_models,
    include_predictions = include_predictions
  )
  if (is.null(config_resampling)) {
    .csdg_stop("The resampling configuration contains a private or unsupported object and cannot be exported safely.")
  }
  out$config = .sanitize_export_object(
    export_config,
    include_models = include_models,
    include_predictions = include_predictions
  )
  out$config$resampling = config_resampling
  out$metadata = .sanitize_export_object(
    out$metadata,
    include_models = include_models,
    include_predictions = include_predictions
  )
  out$plan = .sanitize_export_plan(
    out$plan,
    include_models = include_models,
    include_predictions = include_predictions
  )
  out$artifacts = .sanitize_export_object(
    out$artifacts,
    include_models = include_models,
    include_predictions = include_predictions
  )
  out$gates = .sanitize_export_object(
    out$gates,
    include_models = include_models,
    include_predictions = include_predictions
  )
  out
}

#' @rdname csdg_reporting
#' @export
csdg_export = function(
    x,
    path,
    prefix = NULL,
    include_models = NULL,
    include_predictions = NULL) {
  checkmate::assert_class(x, "CSDGResult")
  .assert_scalar_string(path, "path")
  if (!is.null(prefix)) checkmate::assert_string(prefix, min.chars = 1L)
  prefix = .safe_name(prefix %||% x$claim$id)
  bundle = file.path(path, paste0(prefix, "_csdg_audit"))
  include_models = include_models %||% x$config$export$include_models
  include_predictions = include_predictions %||%
    x$config$export$include_predictions
  checkmate::assert_flag(include_models)
  checkmate::assert_flag(include_predictions)
  export_result = .sanitize_result_for_export(
    x,
    include_models = include_models,
    include_predictions = include_predictions
  )

  .atomic_dir(bundle, function(tmp) {
    dir.create(file.path(tmp, "cards"), recursive = TRUE)
    dir.create(file.path(tmp, "gates"), recursive = TRUE)
    dir.create(file.path(tmp, "artifacts"), recursive = TRUE)
    dir.create(file.path(tmp, "provenance"), recursive = TRUE)

    cards = list(
      claim = .card_to_list(export_result$claim),
      measurement = .card_to_list(export_result$measurement),
      explanation = .card_to_list(export_result$explanation),
      config = .card_to_list(export_result$config)
    )
    for (card_name in names(cards)) {
      .write_json(cards[[card_name]], file.path(tmp, "cards", paste0(card_name, ".json")))
    }
    .write_json(cards, file.path(tmp, "cards", "cards.json"))
    .write_csv(export_result$plan, file.path(tmp, "gate_plan.csv"))
    .write_csv(csdg_report_card(export_result), file.path(tmp, "report_card.csv"))
    .write_csv(csdg_claim_report(export_result), file.path(tmp, "claim_report.csv"))
    saveRDS(
      export_result,
      file = file.path(tmp, "csdg_result.rds"),
      version = 3
    )

    for (gate_id in names(export_result$gates)) {
      gate = export_result$gates[[gate_id]]
      gate_dir = file.path(tmp, "gates", gate_id)
      dir.create(gate_dir, recursive = TRUE)
      meta = gate
      meta$evidence = NULL
      .write_json(meta, file.path(gate_dir, "gate_result.json"))
      .write_nested_evidence(
        gate$evidence, gate_dir, stem = paste0(tolower(gate_id), "_evidence")
      )
    }

    artifacts = export_result$artifacts
    .write_nested_evidence(artifacts, file.path(tmp, "artifacts"), "artifact")

    writeLines(.bundle_readme(export_result), file.path(tmp, "README.md"))
    writeLines(.uncertainty_note(), file.path(tmp, "UNCERTAINTY_SCOPE.md"))
    writeLines(
      .capture_session_info(),
      file.path(tmp, "provenance", "sessionInfo.txt")
    )
    .write_json(
      list(
        package = "mlr3autoiml",
        package_version = .package_version(),
        created_at = .now_utc(),
        result_hash = .hash_object(list(
          claim = export_result$claim,
          measurement = export_result$measurement,
          explanation = export_result$explanation,
          report_card = csdg_report_card(export_result)
        )),
        metadata = export_result$metadata
      ),
      file.path(tmp, "provenance", "run_metadata.json")
    )

    files = list.files(tmp, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    files = files[file.info(files)$isdir == FALSE]
    rel = substring(files, nchar(tmp) + 2L)
    manifest = data.table::data.table(
      path = rel,
      bytes = file.info(files)$size,
      md5 = unname(tools::md5sum(files))
    )
    data.table::setorder(manifest, path)
    .write_csv(manifest, file.path(tmp, "MANIFEST.csv"))
  })
  invisible(bundle)
}
