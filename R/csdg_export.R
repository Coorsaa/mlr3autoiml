
#' @rdname csdg_reporting
#' @export
csdg_report_card = function(x) {
  checkmate::assert_class(x, "CSDGResult")
  plan = data.table::copy(data.table::as.data.table(x$plan))
  rows = data.table::rbindlist(lapply(x$gates, function(gate) {
    data.table::data.table(
      gate_id = gate$gate_id,
      status = gate$status,
      summary = gate$summary,
      limitations = paste(gate$limitations, collapse = " | "),
      started_at = gate$started_at %||% NA_character_,
      completed_at = gate$completed_at %||% NA_character_
    )
  }), fill = TRUE)
  out = merge(plan, rows, by = "gate_id", all.x = TRUE, sort = FALSE)
  out[, gate_order__ := match(gate_id, .csdg_gate_ids)]
  data.table::setorder(out, gate_order__)
  out[, gate_order__ := NULL]
  out
}

#' @rdname csdg_reporting
#' @export
csdg_claim_report = function(x) {
  checkmate::assert_class(x, "CSDGResult")
  card = csdg_report_card(x)
  required = card[required == TRUE]
  data.table::data.table(
    claim_id = x$claim$id,
    claim_version = x$claim$claim_version,
    parent_claim_id = x$claim$parent_claim_id %||% NA_character_,
    revision_relation = x$claim$revision_relation,
    claim_statement = x$claim$statement,
    support_decision = "requires_substantive_judgment",
    required_gates = paste(required$gate_id, collapse = ", "),
    met_gates = paste(required[status == "met", gate_id], collapse = ", "),
    not_met_gates = paste(required[status == "not_met", gate_id], collapse = ", "),
    unresolved_gates = paste(required[status == "unresolved", gate_id], collapse = ", "),
    error_gates = paste(required[status == "error", gate_id], collapse = ", "),
    scope = paste(
      "Model:", x$claim$model_scope,
      "| Setting:", x$claim$setting_scope,
      "| Population:", paste(x$claim$population %||% "unspecified", collapse = " ")
    ),
    interpretation = paste(
      "Gate states are evidence records, not an aggregate claim verdict.",
      "The analyst must decide whether every necessary warrant is met and record any narrower replacement",
      "as a new, linked claim version; several non-equivalent revisions may be defensible."
    )
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
  paste0(
    "# CSDG audit bundle\n\n",
    "Created: ", result$metadata$created_at, "\n\n",
    "Claim: **", result$claim$statement, "**\n\n",
    "Claim version: **", result$claim$claim_version, "**\n\n",
    "## Gate status\n\n",
    paste0(
      "- ", card$gate_id, " - ", card$gate_name, ": `", card$status, "` - ",
      card$summary, collapse = "\n"
    ),
    "\n\n## Interpretation boundary\n\n",
    "Gate states record whether claim-scoped evidence is available and meets any supplied criteria. ",
    "They are not combined into an overall score or verdict. Claim support and any maximal defensible ",
    "revision require substantive judgment and must be recorded as linked claim versions. The records do not ",
    "establish causal validity, population representativeness, clinical utility, fairness, or deployment readiness.\n"
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

.sanitize_export_object = function(x, include_models, include_predictions) {
  if (inherits(x, c("Task", "DataBackend", "Resampling"))) {
    return(NULL)
  }
  if (inherits(x, "Learner") && !isTRUE(include_models)) {
    return(NULL)
  }
  if (inherits(x, "Prediction") && !isTRUE(include_predictions)) {
    return(NULL)
  }
  if (is.data.frame(x) || data.table::is.data.table(x)) {
    if (!isTRUE(include_predictions) && any(c("row_id", "row_ids", "case_id") %in% names(x))) {
      return(NULL)
    }
    return(x)
  }
  if (!is.list(x)) return(x)

  out = x
  if (!isTRUE(include_predictions)) {
    local_aggregate = .local_faithfulness_aggregate(out)
    if (!is.null(local_aggregate)) {
      out$aggregate_summary = local_aggregate
      out$summary = NULL
      out$coefficients = NULL
    }
  }
  for (name in names(x)) {
    safe_scalar_description = name %in% c("weights", "clusters") &&
      checkmate::test_string(out[[name]], min.chars = 1L)
    remove = name %in% c("task", "backend", "backends", "resampling") ||
      (!isTRUE(include_models) && name %in% c("learner", "learners", "model", "models")) ||
      (!isTRUE(include_predictions) &&
        name %in% c(
          "prediction", "predictions", "observation_level", "train_sets", "test_sets",
          "row_map", "assignments", "group", "row_id", "row_ids", "case_id", "case_ids",
          "case", "cases", "case_data", "local_cases", "background", "additional"
        )) ||
      (!isTRUE(include_predictions) && name %in% c("weights", "clusters") && !safe_scalar_description)
    if (remove) {
      out[[name]] = NULL
    } else if (!is.null(out[[name]])) {
      out[[name]] = .sanitize_export_object(
        out[[name]],
        include_models = include_models,
        include_predictions = include_predictions
      )
    }
  }
  out
}

.sanitize_result_for_export = function(x, include_models, include_predictions) {
  out = x
  out$claim = .sanitize_export_object(
    out$claim,
    include_models = include_models,
    include_predictions = include_predictions
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
  out$artifacts = .sanitize_export_object(
    out$artifacts,
    include_models = include_models,
    include_predictions = include_predictions
  )
  out$gates = lapply(out$gates, function(gate) {
    gate$evidence = .sanitize_export_object(
      gate$evidence,
      include_models = include_models,
      include_predictions = include_predictions
    )
    gate
  })
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
    .write_csv(x$plan, file.path(tmp, "gate_plan.csv"))
    .write_csv(csdg_report_card(x), file.path(tmp, "report_card.csv"))
    .write_csv(csdg_claim_report(x), file.path(tmp, "claim_report.csv"))
    saveRDS(
      export_result,
      file = file.path(tmp, "csdg_result.rds"),
      version = 3
    )

    for (gate_id in names(x$gates)) {
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

    writeLines(.bundle_readme(x), file.path(tmp, "README.md"))
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
          claim = x$claim,
          measurement = x$measurement,
          explanation = x$explanation,
          report_card = csdg_report_card(x)
        )),
        metadata = x$metadata
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
