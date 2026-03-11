#' @title Guided Next-Step Recommendations for AutoIML
#'
#' @description
#' Produces a compact, machine-readable guide with actionable next steps,
#' recommended visualizations, and current claim constraints derived from gate
#' outcomes, IEL, and claim semantics.
#'
#' @param x ([AutoIMLResult] | [AutoIML])
#'   A completed result or an AutoIML runner.
#' @param max_actions (`integer(1)`)
#'   Maximum number of actions returned.
#'
#' @return Named list with `summary`, `actions`, and `recommended_plots`.
#' @export
guide_workflow = function(x, max_actions = 6L) {
  checkmate::assert_int(max_actions, lower = 1L)

  res = if (inherits(x, "AutoIML")) x$result else x
  if (!inherits(res, "AutoIMLResult")) {
    stop("guide_workflow() expects an AutoIML or AutoIMLResult.", call. = FALSE)
  }

  rc = report_card(res)
  claim = (.autoiml_get_gate_result(res, "G0A")$artifacts$claim %||% list())
  semantics = as.character(claim$semantics %||% NA_character_)

  statuses = setNames(rc$status, rc$gate_id)

  add_action = function(buf, priority, action, why, how) {
    data.table::rbindlist(list(buf, data.table::data.table(
      priority = as.integer(priority),
      action = as.character(action),
      why = as.character(why),
      how = as.character(how)
    )), fill = TRUE)
  }

  actions = data.table::data.table(
    priority = integer(),
    action = character(),
    why = character(),
    how = character()
  )

  if (!is.na(semantics) && identical(semantics, "within_support")) {
    actions = add_action(
      actions,
      1L,
      "Prefer ALE-first global interpretation",
      "Within-support semantics emphasize descriptive, on-manifold summaries.",
      "Use `plot(type = \"g2_effect\", method = \"ale\")`; treat PDP as sensitivity with support flags."
    )
  }

  if (!is.na(semantics) && identical(semantics, "marginal_model_query")) {
    actions = add_action(
      actions,
      1L,
      "Report PDP with support diagnostics",
      "Marginal model-query claims can leave empirical support.",
      "Use `plot(type = \"g2_effect\", method = \"pdp\", show_ice = TRUE)` and inspect `gate2_tables(...)$support_check_flagged`."
    )
  }

  if (!is.null(statuses[["G6"]]) && statuses[["G6"]] %in% c("warn", "skip")) {
    actions = add_action(
      actions,
      2L,
      "Strengthen multiplicity evidence",
      "Gate 6 is incomplete or weak, limiting robustness of global/local claims.",
      "Provide more `ctx$alt_learners`, rerun, then inspect `plot(type = \"g6_summary\")`."
    )
  }

  if (!is.null(statuses[["G7A"]]) && statuses[["G7A"]] %in% c("warn", "skip")) {
    actions = add_action(
      actions,
      3L,
      "Add subgroup features for fairness/heterogeneity checks",
      "Subgroup-facing conclusions need explicit subgroup evidence.",
      "Set `ctx$sensitive_features`, rerun, inspect subgroup tables and visual diagnostics."
    )
  }

  if (isTRUE((res$iel$local %||% "IEL-0") == "IEL-0")) {
    actions = add_action(
      actions,
      4L,
      "Avoid case-level claims for now",
      "Local evidence level is IEL-0.",
      "Do not report individual-level explanations; first add local diagnostics (G4/G5/G6)."
    )
  }

  if (isTRUE((res$iel$decision %||% "IEL-0") == "IEL-0")) {
    actions = add_action(
      actions,
      5L,
      "Avoid decision recommendations",
      "Decision evidence is insufficient.",
      "Add decision specification and calibration/utility evidence before thresholded decisions."
    )
  }

  if (nrow(actions) == 0L) {
    actions = add_action(
      actions,
      9L,
      "Proceed with current claim scope",
      "No immediate blockers were detected from gate statuses.",
      "Export the audit bundle and include the recommended visuals in reporting."
    )
  }

  actions = unique(actions)
  data.table::setorderv(actions, "priority")
  actions = actions[seq_len(min(nrow(actions), as.integer(max_actions)))]

  rec_plots = c("storyboard", "g2_effect", "g2_hstats")
  if (!is.null(statuses[["G6"]]) && statuses[["G6"]] != "skip") {
    rec_plots = c(rec_plots, "g6_summary")
  }

  summary_dt = data.table::data.table(
    purpose = as.character(res$purpose %||% NA_character_),
    iel_overall = as.character(res$iel$overall %||% NA_character_),
    iel_global = as.character(res$iel$global %||% NA_character_),
    iel_local = as.character(res$iel$local %||% NA_character_),
    iel_decision = as.character(res$iel$decision %||% NA_character_),
    claim_scope_overall = as.character(res$claim_scope$overall %||% NA_character_),
    semantics = semantics
  )

  list(
    summary = summary_dt,
    actions = actions,
    recommended_plots = unique(rec_plots)
  )
}
