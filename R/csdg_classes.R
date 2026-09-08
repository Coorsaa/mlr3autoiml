
new_gate_result = function(
    gate_id,
    status,
    summary,
    evidence = list(),
    limitations = character(),
    thresholds = list(),
    diagnostics = list(),
    error = NULL,
    started_at = NULL,
    completed_at = .now_utc()) {
  .assert_choice(gate_id, .csdg_gate_ids, "gate_id")
  .assert_choice(status, .csdg_statuses, "status")
  .assert_scalar_string(summary, "summary")
  structure(
    list(
      gate_id = gate_id,
      status = status,
      summary = summary,
      evidence = evidence,
      limitations = as.character(limitations),
      thresholds = thresholds,
      diagnostics = diagnostics,
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
  cat("  Model scope:", x$model_scope, "\n")
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
  print(data.table::as.data.table(x), ...)
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
  cat("  Near-equivalent:", sum(x$candidates$accepted), "\n")
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
