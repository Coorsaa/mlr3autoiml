
new_gate_result = function(
    gate_id,
    status,
    summary,
    evidence = list(),
    limitations = character(),
    thresholds = list(),
    diagnostics = list(),
    availability = NULL,
    result_direction = NULL,
    criterion = NULL,
    criterion_source = NULL,
    criterion_rationale = NULL,
    materiality = "not_applicable",
    adjudication_basis = NULL,
    claim_consequence = "none",
    rationale = NULL,
    error = NULL,
    started_at = NULL,
    completed_at = .now_utc()) {
  .assert_choice(gate_id, .csdg_gate_ids, "gate_id")
  .assert_scalar_string(status, "status")
  status = .csdg_map_legacy(status, .csdg_legacy_statuses, "Gate status")
  .assert_choice(status, .csdg_gate_statuses, "status")
  .assert_scalar_string(summary, "summary")
  availability = availability %||% switch(
    status,
    error = "unavailable",
    not_required = "not_applicable",
    "complete"
  )
  result_direction = result_direction %||% switch(
    status,
    supported = "supports",
    contradicted = "challenges",
    "not_evaluated"
  )
  criteria_table = if (is.list(evidence)) evidence$criteria %||% NULL else NULL
  if (is.null(criterion) && is.data.frame(criteria_table) && nrow(criteria_table)) {
    criterion_columns = intersect(
      c("criterion", "operator", "threshold", "criterion_complete", "passed"),
      names(criteria_table)
    )
    criterion = as.data.frame(criteria_table)[, criterion_columns, drop = FALSE]
  }
  if (is.null(criterion_source) && is.data.frame(criteria_table) && "criterion_source" %in% names(criteria_table)) {
    sources = unique(na.omit(as.character(criteria_table$criterion_source)))
    if (length(sources)) criterion_source = paste(sources, collapse = " | ")
  }
  if (is.null(criterion_rationale) &&
      is.data.frame(criteria_table) &&
      "criterion_rationale" %in% names(criteria_table)) {
    rationales = unique(na.omit(as.character(criteria_table$criterion_rationale)))
    if (length(rationales)) criterion_rationale = paste(rationales, collapse = " | ")
  }
  .assert_choice(availability, .csdg_evidence_availability, "availability")
  .assert_choice(result_direction, .csdg_result_directions, "result_direction")
  .assert_choice(materiality, .csdg_materiality, "materiality")
  .assert_choice(claim_consequence, .csdg_claim_consequences, "claim_consequence")
  if (!is.null(adjudication_basis)) {
    .assert_choice(adjudication_basis, .csdg_adjudication_bases, "adjudication_basis")
  }
  .assert_scalar_string(criterion_source, "criterion_source", allow_null = TRUE)
  .assert_scalar_string(criterion_rationale, "criterion_rationale", allow_null = TRUE)
  .assert_scalar_string(rationale, "rationale", allow_null = TRUE)
  if (is.null(criterion) && (!is.null(criterion_source) || !is.null(criterion_rationale))) {
    .csdg_stop("`criterion_source` and `criterion_rationale` require a non-NULL `criterion`.")
  }
  if (xor(is.null(criterion_source), is.null(criterion_rationale))) {
    .csdg_stop("Criterion provenance requires both `criterion_source` and `criterion_rationale`.")
  }
  if (!is.null(criterion) &&
      status %in% c("supported", "contradicted") &&
      (is.null(criterion_source) || is.null(criterion_rationale))) {
    .csdg_stop("A criterion used to adjudicate a gate requires a recorded source and rationale.")
  }
  structure(
    list(
      gate_id = gate_id,
      status = status,
      summary = summary,
      evidence = evidence,
      limitations = as.character(limitations),
      thresholds = thresholds,
      diagnostics = diagnostics,
      availability = availability,
      result_direction = result_direction,
      criterion = criterion,
      criterion_source = criterion_source,
      criterion_rationale = criterion_rationale,
      materiality = materiality,
      adjudication_basis = adjudication_basis,
      claim_consequence = claim_consequence,
      rationale = rationale %||% summary,
      error = error,
      started_at = started_at,
      completed_at = completed_at
    ),
    class = c("CSDGGateResult", "list")
  )
}

new_csdg_result = function(
    claim,
    measurement,
    explanation,
    config,
    plan,
    gates,
    artifacts = list(),
    metadata = list()) {
  if (!inherits(claim, "CSDGClaim")) {
    .csdg_stop("`claim` must be a CSDGClaim.")
  }
  if (!inherits(measurement, "CSDGMeasurement")) {
    .csdg_stop("`measurement` must be a CSDGMeasurement.")
  }
  if (!is.null(explanation) && !inherits(explanation, "CSDGExplanation")) {
    .csdg_stop("`explanation` must be NULL or a CSDGExplanation.")
  }
  if (!inherits(config, "CSDGConfig")) {
    .csdg_stop("`config` must be a CSDGConfig.")
  }
  if (!inherits(plan, "CSDGGatePlan")) {
    .csdg_stop("`plan` must be a CSDGGatePlan.")
  }
  if (!is.list(gates)) .csdg_stop("`gates` must be a list.")
  structure(
    list(
      claim = claim,
      measurement = measurement,
      explanation = explanation,
      config = config,
      plan = plan,
      gates = gates,
      artifacts = artifacts,
      metadata = .recursive_modify(
        list(
          package_version = .package_version(),
          created_at = .now_utc()
        ),
        metadata
      )
    ),
    class = c("CSDGResult", "list")
  )
}

#' @export
#' @noRd
print.CSDGClaim = function(x, ...) {
  cat("<CSDGClaim>", x$id, "\n")
  cat("  Statement:", x$statement, "\n")
  cat("  Types:", paste(x$claim_type, collapse = ", "), "\n")
  cat("  Model:", x$model_scope, "\n")
  cat("  Meaning:", x$meaning %||% "model_description", "\n")
  invisible(x)
}

#' @export
#' @noRd
print.CSDGClaimRelation = function(x, ...) {
  cat("<CSDGClaimRelation>", x$parent_claim_id, "->", x$claim_id, "\n")
  cat("  Claim relation:", x$relation, "\n")
  cat("  Scope relation:", x$scope_relation %||% x$context_relation, "\n")
  cat("  Revision kind:", x$revision_kind, "\n")
  print(x$scope %||% x$coordinates, ...)
  invisible(x)
}

#' @export
#' @noRd
print.CSDGEvidenceRecord = function(x, ...) {
  cat("<CSDGEvidenceRecord>", x$gate_id, "\n")
  cat("  Role:", gsub("_", " ", x$role), "\n")
  if (!is.null(x$status)) cat("  Status:", x$status, "\n")
  if (!isTRUE(x$applicable)) cat("  Not applicable to the claim (context)\n")
  if (!is.null(x$required_property)) cat("  Property:", x$required_property, "\n")
  if (!is.null(x$observation)) cat("  Observation:", x$observation, "\n")
  invisible(x)
}

#' @export
#' @noRd
print.CSDGClaimAdjudication = function(x, ...) {
  assessment = x$assessment %||% x$decision
  cat("<CSDGClaimAdjudication>\n")
  cat("  Assessment:", gsub("_", " ", assessment, fixed = TRUE), "\n")
  if (length(x$decision_options)) cat("  Decision options:", paste(x$decision_options, collapse = " or "), "\n")
  if (is.data.frame(x$properties) && nrow(x$properties)) {
    cat("  Required properties:\n")
    cat(paste0("   ", x$properties$gate_id, ": ", x$properties$status, " (", x$properties$source, ")"),
      sep = "\n")
  }
  cat("\n ", x$rationale, "\n")
  invisible(x)
}

#' @export
#' @noRd
print.CSDGMeasurement = function(x, ...) {
  cat("<CSDGMeasurement>\n")
  cat("  Outcome:", x$outcome %||% "<not specified>", "\n")
  cat("  Predictors:", length(x$predictors %||% character()), "\n")
  cat("  Verification:", x$verification$status, "\n")
  invisible(x)
}

#' @export
#' @noRd
print.CSDGExplanation = function(x, ...) {
  cat("<CSDGExplanation>\n")
  cat("  Methods:", paste(x$method_ids, collapse = ", "), "\n")
  cat("  Scope:", paste(x$scope, collapse = ", "), "\n")
  invisible(x)
}

#' @export
#' @noRd
print.CSDGConfig = function(x, ...) {
  cat("<CSDGConfig>\n")
  cat("  Seed:", x$seed, "\n")
  cat("  Resampling:", x$resampling$folds, "folds x",
      x$resampling$repeats, "repeats\n")
  invisible(x)
}

#' @export
#' @noRd
print.CSDGGatePlan = function(x, ...) {
  columns = intersect(c("gate_id", "gate_name", "required", "plan_role", "trigger"), names(x))
  print(data.table::as.data.table(x)[, columns, with = FALSE], ...)
  if (isTRUE(attr(x, "causal_design_required"))) {
    cat("Causal design required (not a gate): a causal claim requires a design with stated identification",
      "assumptions.\n")
  }
  invisible(x)
}

#' @export
#' @noRd
as.data.frame.CSDGGatePlan = function(x, ...) {
  as.data.frame(data.table::as.data.table(x), ...)
}

#' @export
#' @noRd
print.CSDGGateResult = function(x, ...) {
  cat(sprintf("<CSDGGateResult %s: %s>\n", x$gate_id, x$status))
  cat(" ", x$summary, "\n")
  if (length(x$limitations)) {
    cat("  Limitations:\n")
    cat(paste0("   - ", x$limitations, collapse = "\n"), "\n")
  }
  invisible(x)
}

#' @export
#' @noRd
print.CSDGResample = function(x, ...) {
  cat("<CSDGResample>\n")
  cat("  Task:", x$task_id, "(", x$task_type, ")\n")
  cat("  Learner:", x$learner_id, "\n")
  cat("  Iterations:", nrow(x$fold_scores), "\n")
  cat("  Predictions:", nrow(x$predictions), "\n")
  invisible(x)
}

#' @export
#' @noRd
print.CSDGRashomon = function(x, ...) {
  cat("<CSDGRashomon>\n")
  cat("  Candidates:", nrow(x$candidates), "\n")
  accepted = x$candidates$accepted
  cat(
    "  Near-equivalent:",
    if (all(is.na(accepted))) "not defined" else sum(accepted, na.rm = TRUE),
    "\n"
  )
  cat("  Primary measure:", x$primary_measure, "\n")
  invisible(x)
}

#' @export
#' @noRd
print.CSDGResult = function(x, ...) {
  cat("<CSDGResult>", x$claim$id, "\n")
  card = csdg_report_card(x)
  print(card[, .(gate_id, gate_name, required, status, summary)])
  invisible(x)
}
