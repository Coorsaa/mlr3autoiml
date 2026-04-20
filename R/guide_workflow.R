#' @title Guided Next-Step Recommendations for AutoIML
#'
#' @description
#' Produces a compact, machine-readable guide with actionable next steps,
#' recommended visualizations, current claim constraints, and reader-facing
#' answers to common interpretation questions derived from gate outcomes, IEL,
#' and claim semantics.
#'
#' @param x ([AutoIMLResult] | [AutoIML])
#'   A completed result or an AutoIML runner.
#' @param max_actions (`integer(1)`)
#'   Maximum number of actions returned.
#'
#' @return Named list with `summary`, `actions`, `recommended_plots`,
#'   `trust_summary`, `model_story`, and `reader_questions`.
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
  stakes = as.character(claim$stakes %||% NA_character_)

  statuses = if (data.table::is.data.table(rc) && nrow(rc) > 0L) {
    out = as.list(as.character(rc$status))
    names(out) = as.character(rc$gate_id)
    out
  } else {
    list()
  }

  g2 = .autoiml_get_gate_result(res, "G2")
  g2_metrics = g2$metrics %||% data.table::data.table()

  trust_summary = .autoiml_trust_summary(res)
  model_story = .autoiml_model_story(res)

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

  if (!is.null(statuses[["G0B"]]) && statuses[["G0B"]] %in% c("warn", "fail")) {
    actions = add_action(
      actions,
      1L,
      "Strengthen measurement and scoring documentation",
      "Psychological interpretation depends on construct quality, subgroup comparability, and explicit missingness/scoring assumptions.",
      "Add `config$measurement` information (level, reliability/invariance notes, missingness plan, scoring pipeline) and make any remaining limitations explicit in the report card."
    )
  }

  if (!is.null(statuses[["G1"]]) && statuses[["G1"]] %in% c("warn", "fail")) {
    actions = add_action(
      actions,
      1L,
      "Address predictive adequacy before interpretation",
      "If Gate 1 is weak, interpretation claims are not yet well grounded.",
      "Inspect the primary metric, compare against the featureless baseline, revise the feature/model pipeline, and rerun before expanding interpretive claims."
    )
  }

  if (!is.na(semantics) && identical(semantics, "within_support")) {
    actions = add_action(
      actions,
      2L,
      "Prefer ALE-first global interpretation",
      "Within-support semantics emphasize descriptive, on-manifold summaries rather than off-support model queries.",
      "Use `plot(type = \"g2_effect\", method = \"ale\")`; treat PDP as a sensitivity analysis and keep support diagnostics visible."
    )
  }

  if (!is.na(semantics) && identical(semantics, "marginal_model_query")) {
    actions = add_action(
      actions,
      2L,
      "Report PDP as a model query, not as a world claim",
      "Marginal model-query claims can leave empirical support and are statements about the fitted model.",
      "Use `plot(type = \"g2_effect\", method = \"pdp\", show_ice = TRUE)` and inspect support flags before narrating any what-if pattern."
    )
  }

  if (isTRUE(.autoiml_dt_scalar(g2_metrics, "interaction_flag", FALSE))) {
    actions = add_action(
      actions,
      3L,
      "Avoid single-feature stories when interactions are material",
      "Gate 2 detected interaction structure, so one-feature narratives can be incomplete or misleading.",
      "Prioritize regionalization, interaction-aware visualizations, and language that keeps interactions explicit."
    )
  }

  simple_model_in_rashomon = .autoiml_dt_scalar(model_story, "simple_model_in_rashomon", NA)
  best_simple_model = as.character(.autoiml_dt_scalar(model_story, "best_simple_model", NA_character_))
  if (isTRUE(simple_model_in_rashomon) && !is.na(best_simple_model) && nzchar(best_simple_model)) {
    actions = add_action(
      actions,
      3L,
      "Lead with the simplest competitive model",
      "A simpler learner is competitive under the Rashomon rule, so an easier primary narrative is available.",
      sprintf("Use %s for the main explanatory story and keep the more complex learner as a robustness check.", best_simple_model)
    )
  } else if (!is.na(simple_model_in_rashomon) && !isTRUE(simple_model_in_rashomon) && !is.na(best_simple_model) && nzchar(best_simple_model)) {
    actions = add_action(
      actions,
      3L,
      "Do not over-trust the easy model just because it is easy to explain",
      "The best simple model is not competitive in the Gate 6 benchmark.",
      "Report the simple-versus-complex comparison explicitly; if the complex model is retained, keep the interpretation focused on robust global patterns rather than pretending the task is simpler than it is."
    )
  }

  if (!is.null(statuses[["G4"]]) && statuses[["G4"]] %in% c("warn", "fail")) {
    actions = add_action(
      actions,
      4L,
      "Treat local explanations as illustrative only",
      "Gate 4 raised faithfulness or local-additivity concerns.",
      "Restrict local narratives, document the attribution settings, and avoid person-specific claims that sound causal or diagnostic."
    )
  }

  if (!is.null(statuses[["G5"]]) && statuses[["G5"]] %in% c("warn", "fail")) {
    actions = add_action(
      actions,
      4L,
      "Report robust tiers rather than exact explanation ranks",
      "Gate 5 indicates instability under resampling or perturbation.",
      "Summarize stable directions or coarse importance tiers and de-emphasize fine-grained feature ordering."
    )
  }

  if (!is.null(statuses[["G6"]]) && statuses[["G6"]] %in% c("warn", "skip")) {
    actions = add_action(
      actions,
      5L,
      "Strengthen model-class sensitivity evidence",
      "Gate 6 is incomplete or weak, which limits claims about robustness across plausible alternative models.",
      "Provide more explicit `ctx$alt_learners`, rerun Gate 6, and inspect both the performance table and Rashomon summary before fixing the final explanatory story."
    )
  }

  if (!is.null(statuses[["G7A"]]) && statuses[["G7A"]] %in% c("warn", "skip")) {
    actions = add_action(
      actions,
      6L,
      "Add subgroup evidence before making subgroup-facing claims",
      "Subgroup conclusions require subgroup audits plus measurement/comparability assumptions.",
      "Set `ctx$sensitive_features`, rerun the workflow, and pair subgroup explanation claims with subgroup performance/calibration evidence."
    )
  }

  if (isTRUE((res$iel$local %||% "IEL-0") == "IEL-0")) {
    actions = add_action(
      actions,
      7L,
      "Avoid case-level claims for now",
      "Local evidence is currently IEL-0.",
      "Do not report individual-level explanations as if they were actionable or definitive; improve local diagnostics first."
    )
  }

  if (isTRUE((res$iel$decision %||% "IEL-0") == "IEL-0")) {
    actions = add_action(
      actions,
      7L,
      "Avoid decision recommendations",
      "Decision evidence is insufficient for thresholded action.",
      "Add explicit threshold/utility assumptions and stronger calibration evidence before turning scores into decisions."
    )
  }

  if (nrow(actions) == 0L) {
    actions = add_action(
      actions,
      9L,
      "Proceed with the current claim scope",
      "No immediate blocker was detected beyond the current IEL restrictions.",
      "Export the audit bundle, include the trust/model-story outputs in the paper, and keep the declared semantics visible wherever interpretations are reported."
    )
  }

  actions = unique(actions)
  data.table::setorderv(actions, c("priority", "action"))
  actions = actions[seq_len(min(nrow(actions), as.integer(max_actions)))]

  reader_questions = .autoiml_reader_questions(res, actions = actions)

  rec_plots = c("storyboard", "g2_effect", "g2_hstats", "g6_performance")
  if (!is.null(statuses[["G6"]]) && statuses[["G6"]] != "skip") {
    rec_plots = c(rec_plots, "g6_summary")
  }

  summary_dt = data.table::data.table(
    purpose = as.character(res$purpose %||% NA_character_),
    stakes = stakes,
    semantics = semantics,
    iel_overall = as.character(res$iel$overall %||% NA_character_),
    iel_global = as.character(res$iel$global %||% NA_character_),
    iel_local = as.character(res$iel$local %||% NA_character_),
    iel_decision = as.character(res$iel$decision %||% NA_character_),
    claim_scope_overall = as.character(res$claim_scope$overall %||% NA_character_),
    gate0b_status = as.character(statuses[["G0B"]] %||% NA_character_),
    gate1_status = as.character(statuses[["G1"]] %||% NA_character_),
    gate4_status = as.character(statuses[["G4"]] %||% NA_character_),
    gate5_status = as.character(statuses[["G5"]] %||% NA_character_),
    gate6_status = as.character(statuses[["G6"]] %||% NA_character_),
    gate7a_status = as.character(statuses[["G7A"]] %||% NA_character_)
  )

  list(
    summary = summary_dt,
    actions = actions,
    recommended_plots = unique(rec_plots),
    trust_summary = trust_summary,
    model_story = model_story,
    reader_questions = reader_questions
  )
}
