# FILE: R/report_card.R

#' @title AutoIML report card
#'
#' @description
#' Create a compact, audit-ready report card (one row per gate) from an
#' [AutoIMLResult].
#'
#' The report card includes the gate status, a short summary, and the achieved
#' **claim-scoped Interpretation Evidence Levels (IEL)**.
#'
#' @param x ([AutoIMLResult] | [AutoIML])
#'   A completed result or an AutoIML runner.
#'
#' @return A [data.table::data.table] with one row per gate.
#' @export
report_card = function(x) {
  res = if (inherits(x, "AutoIML")) x$result else x
  if (!inherits(res, "AutoIMLResult")) {
    stop("report_card() expects an AutoIML or AutoIMLResult.", call. = FALSE)
  }

  gates = res$gate_results %||% list()
  if (length(gates) == 0L) {
    return(data.table::data.table())
  }

  dt = data.table::rbindlist(
    lapply(gates, function(gr) {
      data.table::data.table(
        gate_id = gr$gate_id,
        gate_name = gr$gate_name,
        pdr = gr$pdr %||% NA_character_,
        status = gr$status %||% NA_character_,
        summary = gr$summary %||% NA_character_
      )
    }),
    fill = TRUE
  )

  # Attach IEL fields (repeat across rows for convenience)
  iel = res$iel
  if (is.list(iel)) {
    dt[, iel_overall := iel$overall %||% NA_character_]
    dt[, iel_global := iel$global %||% NA_character_]
    dt[, iel_local := iel$local %||% NA_character_]
    dt[, iel_decision := iel$decision %||% NA_character_]
  } else {
    dt[, iel_overall := as.character(iel)]
    dt[, iel_global := NA_character_]
    dt[, iel_local := NA_character_]
    dt[, iel_decision := NA_character_]
  }

  dt[, purpose := res$purpose %||% NA_character_]
  dt[, quick_start := isTRUE(res$quick_start)]

  # If Gate 0 exists, attach claim semantics / stakes as convenience columns.
  g0a = gates[["G0A"]]
  if (!is.null(g0a) && inherits(g0a$metrics, "data.table") && nrow(g0a$metrics) >= 1L) {
    m0 = g0a$metrics[1L]
    dt[, semantics := m0$semantics %||% NA_character_]
    dt[, stakes := m0$stakes %||% NA_character_]
    dt[, claim_global := m0$claim_global %||% NA]
    dt[, claim_local := m0$claim_local %||% NA]
    dt[, claim_decision := m0$claim_decision %||% NA]
  }

  dt[]
}


#' @title Extended AutoIML report card (gate-evidence matrix)
#'
#' @description
#' Returns a requirement-level gate-evidence matrix by joining the traceability
#' requirements (`inst/extdata/framework_requirements.yaml`) with executed gate
#' outputs and artifact-key presence checks.
#'
#' @param x ([AutoIMLResult] | [AutoIML])
#'   A completed result or an AutoIML runner.
#'
#' @return A [data.table::data.table] with one row per requirement ID.
#' @export
report_card_extended = function(x) {
  res = if (inherits(x, "AutoIML")) x$result else x
  if (!inherits(res, "AutoIMLResult")) {
    stop("report_card_extended() expects an AutoIML or AutoIMLResult.", call. = FALSE)
  }

  reqs_yaml = .autoiml_framework_requirements()
  vr = .autoiml_validate_framework_requirements(reqs_yaml)
  if (!isTRUE(vr$ok)) {
    stop("Invalid framework requirements: ", paste(vr$errors, collapse = " | "), call. = FALSE)
  }

  reqs = reqs_yaml$requirements
  rows = lapply(reqs, function(r) {
    gate = as.character(r$gate)
    gr = .autoiml_get_gate_result(res, gate)

    artifact_keys = if (!is.null(gr) && is.list(gr$artifacts)) names(gr$artifacts) else character(0)
    required_keys = as.character(unlist(r$artifact_fields %||% character(), use.names = FALSE))
    required_keys = required_keys[nzchar(required_keys)]

    missing_keys = setdiff(required_keys, artifact_keys)
    keys_ok = length(missing_keys) == 0L

    data.table::data.table(
      requirement_id = as.character(r$id),
      gate = gate,
      evidence_type = as.character(r$evidence_type),
      severity_if_missing = as.character(r$severity_if_missing),
      gate_present = !is.null(gr),
      gate_status = if (!is.null(gr)) as.character(gr$status %||% NA_character_) else NA_character_,
      artifact_keys_ok = keys_ok,
      missing_artifact_keys = if (!keys_ok) paste(missing_keys, collapse = ",") else "",
      requirement_text = as.character(r$requirement_text)
    )
  })

  out = data.table::rbindlist(rows, fill = TRUE)
  out[]
}


#' @title Export AutoIML audit bundle
#'
#' @description
#' Exports compact and extended report cards plus core reproducibility artifacts
#' (gate results, IEL, claim scope, session info, and traceability status).
#'
#' @param x ([AutoIMLResult] | [AutoIML])
#'   A completed result or an AutoIML runner.
#' @param dir (`character(1)`)
#'   Output directory for exported files.
#' @param prefix (`character(1)`)
#'   Filename prefix.
#'
#' @return Named list of exported file paths.
#' @export
export_audit_bundle = function(x, dir = "autoiml_audit_bundle", prefix = "autoiml") {
  checkmate::assert_string(dir, min.chars = 1L)
  checkmate::assert_string(prefix, min.chars = 1L)

  res = if (inherits(x, "AutoIML")) x$result else x
  if (!inherits(res, "AutoIMLResult")) {
    stop("export_audit_bundle() expects an AutoIML or AutoIMLResult.", call. = FALSE)
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  rc = report_card(res)
  rce = report_card_extended(res)
  tr = .autoiml_traceability_status()

  p_report = file.path(dir, paste0(prefix, "_report_card.csv"))
  p_report_ext = file.path(dir, paste0(prefix, "_report_card_extended.csv"))
  p_gate_results = file.path(dir, paste0(prefix, "_gate_results.rds"))
  p_iel = file.path(dir, paste0(prefix, "_iel.rds"))
  p_claim_scope = file.path(dir, paste0(prefix, "_claim_scope.rds"))
  p_trace = file.path(dir, paste0(prefix, "_traceability_status.rds"))
  p_session = file.path(dir, paste0(prefix, "_sessionInfo.txt"))

  data.table::fwrite(rc, p_report)
  data.table::fwrite(rce, p_report_ext)
  saveRDS(res$gate_results, p_gate_results)
  saveRDS(res$iel, p_iel)
  saveRDS(res$claim_scope, p_claim_scope)
  saveRDS(tr, p_trace)

  si = utils::capture.output(utils::sessionInfo())
  writeLines(si, p_session)

  list(
    report_card = p_report,
    report_card_extended = p_report_ext,
    gate_results = p_gate_results,
    iel = p_iel,
    claim_scope = p_claim_scope,
    traceability_status = p_trace,
    session_info = p_session
  )
}
