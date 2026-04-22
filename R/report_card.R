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
  data.table::set(dt, j = "requested_scopes", value = paste(as.character(res$iel$requested %||% c("global")), collapse = ","))

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
    gr = if (gate %in% c("IEL", "REPORT")) NULL else .autoiml_get_gate_result(res, gate)
    app = .autoiml_requirement_applicable(r, res)

    field_chk = if (isTRUE(app$applicable)) {
      if (identical(gate, "IEL")) {
        vals = .autoiml_as_list(res$iel)
        missing = character(0)
        for (k in as.character(unlist(r$artifact_fields %||% character(), use.names = FALSE))) {
          if (!k %in% names(vals) || !.autoiml_has_evidence_value(vals[[k]])) {
            missing = c(missing, paste0("iel:", k))
          }
        }
        list(ok = length(missing) == 0L, missing = missing)
      } else if (identical(gate, "REPORT")) {
        rc = report_card(res)
        missing = character(0)
        for (k in as.character(unlist(r$artifact_fields %||% character(), use.names = FALSE))) {
          if (!(k %in% names(rc))) {
            missing = c(missing, paste0("report:", k))
          }
        }
        list(ok = length(missing) == 0L, missing = missing)
      } else {
        .autoiml_requirement_fields_ok(r, gr)
      }
    } else {
      list(ok = NA, missing = character(0))
    }

    evidence_status = if (!isTRUE(app$applicable)) {
      "not_applicable"
    } else if (is.null(gr) && !gate %in% c("IEL", "REPORT")) {
      "gate_missing"
    } else if (isTRUE(field_chk$ok)) {
      "evidence_present"
    } else {
      "evidence_missing"
    }

    data.table::data.table(
      requirement_id = as.character(r$id),
      gate = gate,
      evidence_type = as.character(r$evidence_type),
      severity_if_missing = as.character(r$severity_if_missing),
      applicable = isTRUE(app$applicable),
      applicability_reason = as.character(app$reason %||% ""),
      gate_present = if (!gate %in% c("IEL", "REPORT")) !is.null(gr) else TRUE,
      gate_status = if (!is.null(gr)) as.character(gr$status %||% NA_character_) else if (gate %in% c("IEL", "REPORT")) "computed" else NA_character_,
      artifact_keys_ok = if (isTRUE(app$applicable)) isTRUE(field_chk$ok) else NA,
      missing_artifact_keys = if (isTRUE(app$applicable) && !isTRUE(field_chk$ok)) paste(field_chk$missing, collapse = ",") else "",
      evidence_status = evidence_status,
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
#' (gate results, IEL, claim scope, session info, traceability status, and the
#' reader-facing guide outputs).
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
  guide = guide_workflow(res, max_actions = 8L)

  p_report = file.path(dir, paste0(prefix, "_report_card.csv"))
  p_report_ext = file.path(dir, paste0(prefix, "_report_card_extended.csv"))
  p_guide_summary = file.path(dir, paste0(prefix, "_guide_summary.csv"))
  p_guide_actions = file.path(dir, paste0(prefix, "_guide_actions.csv"))
  p_trust_summary = file.path(dir, paste0(prefix, "_trust_summary.csv"))
  p_model_story = file.path(dir, paste0(prefix, "_model_story.csv"))
  p_reader_questions = file.path(dir, paste0(prefix, "_reader_questions.csv"))
  p_recommended_plots = file.path(dir, paste0(prefix, "_recommended_plots.txt"))
  p_gate_results = file.path(dir, paste0(prefix, "_gate_results.rds"))
  p_iel = file.path(dir, paste0(prefix, "_iel.rds"))
  p_claim_scope = file.path(dir, paste0(prefix, "_claim_scope.rds"))
  p_trace = file.path(dir, paste0(prefix, "_traceability_status.rds"))
  p_session = file.path(dir, paste0(prefix, "_sessionInfo.txt"))

  data.table::fwrite(rc, p_report)
  data.table::fwrite(rce, p_report_ext)
  data.table::fwrite(guide$summary, p_guide_summary)
  data.table::fwrite(guide$actions, p_guide_actions)
  data.table::fwrite(guide$trust_summary, p_trust_summary)
  data.table::fwrite(guide$model_story, p_model_story)
  data.table::fwrite(guide$reader_questions, p_reader_questions)
  writeLines(unique(as.character(guide$recommended_plots)), p_recommended_plots)

  saveRDS(res$gate_results, p_gate_results)
  saveRDS(res$iel, p_iel)
  saveRDS(res$claim_scope, p_claim_scope)
  saveRDS(tr, p_trace)

  si = utils::capture.output(utils::sessionInfo())
  writeLines(si, p_session)

  list(
    report_card = p_report,
    report_card_extended = p_report_ext,
    guide_summary = p_guide_summary,
    guide_actions = p_guide_actions,
    trust_summary = p_trust_summary,
    model_story = p_model_story,
    reader_questions = p_reader_questions,
    recommended_plots = p_recommended_plots,
    gate_results = p_gate_results,
    iel = p_iel,
    claim_scope = p_claim_scope,
    traceability_status = p_trace,
    session_info = p_session
  )
}
