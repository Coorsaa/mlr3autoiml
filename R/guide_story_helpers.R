#' @title Internal story helpers for `guide_workflow()`
#' @name guide_story_helpers
#' @keywords internal
NULL

.autoiml_dt_scalar = function(x, col, default = NA) {
  if (is.null(x) || !data.table::is.data.table(x) || nrow(x) < 1L || !(col %in% names(x))) {
    return(default)
  }
  out = x[[col]][[1L]]
  if (is.null(out) || length(out) < 1L) {
    return(default)
  }
  out
}

.autoiml_measure_minimize = function(metric_id) {
  if (is.null(metric_id) || !length(metric_id) || is.na(metric_id) || !nzchar(metric_id)) {
    return(NA)
  }

  out = tryCatch(mlr3::msr(as.character(metric_id)[1L])$minimize, error = function(e) NA)
  if (!is.na(out)) {
    return(isTRUE(out))
  }

  metric_id = tolower(as.character(metric_id)[1L])
  if (grepl("auc|acc|tpr|ppv|rsq|f1", metric_id)) {
    return(FALSE)
  }
  if (grepl("loss|rmse|mse|mae|mape|brier|ece|error", metric_id)) {
    return(TRUE)
  }

  NA
}

.autoiml_guess_primary_metric = function(g1) {
  if (is.null(g1)) {
    return(NA_character_)
  }

  uncertainty = g1$artifacts$uncertainty %||% NULL
  metric_ids = character(0)
  if (data.table::is.data.table(uncertainty) && nrow(uncertainty) > 0L && "measure_id" %in% names(uncertainty)) {
    metric_ids = unique(as.character(uncertainty$measure_id))
  }

  priority = c(
    "classif.auc",
    "classif.logloss",
    "classif.bbrier",
    "regr.rmse",
    "regr.mae",
    "regr.mse"
  )
  hit = priority[priority %in% metric_ids]
  if (length(hit) > 0L) {
    return(hit[[1L]])
  }

  if (length(metric_ids) > 0L) {
    return(metric_ids[[1L]])
  }

  metrics = g1$metrics %||% NULL
  if (data.table::is.data.table(metrics) && nrow(metrics) > 0L) {
    metric_cols = setdiff(names(metrics), grep("_baseline$", names(metrics), value = TRUE))
    metric_cols = setdiff(metric_cols, c("task_type", "nclass", "positive", "utility_spec", "opt_threshold", "opt_value"))
    metric_cols = metric_cols[vapply(metric_cols, function(nm) is.numeric(metrics[[nm]]), logical(1L))]
    metric_priority = priority[priority %in% metric_cols]
    if (length(metric_priority) > 0L) {
      return(metric_priority[[1L]])
    }
    if (length(metric_cols) > 0L) {
      return(metric_cols[[1L]])
    }
  }

  NA_character_
}

.autoiml_extract_primary_performance = function(res) {
  g1 = .autoiml_get_gate_result(res, "G1")
  if (is.null(g1)) {
    return(data.table::data.table())
  }

  metric_id = .autoiml_guess_primary_metric(g1)
  uncertainty = g1$artifacts$uncertainty %||% NULL
  metrics = g1$metrics %||% NULL
  minimize = .autoiml_measure_minimize(metric_id)

  estimate = NA_real_
  ci_low = NA_real_
  ci_high = NA_real_
  baseline = NA_real_
  better_than_baseline = NA
  performance_text = "Primary predictive metric unavailable."

  if (data.table::is.data.table(uncertainty) && nrow(uncertainty) > 0L && !is.na(metric_id)) {
    row = uncertainty[measure_id == metric_id][1L]
    if (nrow(row) > 0L) {
      estimate = as.numeric(row$mean)
      ci_low = as.numeric(row$ci_low)
      ci_high = as.numeric(row$ci_high)
    }
  }

  if (data.table::is.data.table(metrics) && nrow(metrics) > 0L && !is.na(metric_id)) {
    if (metric_id %in% names(metrics) && !is.finite(estimate)) {
      estimate = as.numeric(metrics[[metric_id]][[1L]])
    }
    baseline_col = paste0(metric_id, "_baseline")
    if (baseline_col %in% names(metrics)) {
      baseline = as.numeric(metrics[[baseline_col]][[1L]])
      if (is.finite(estimate) && is.finite(baseline) && !is.na(minimize)) {
        better_than_baseline = if (minimize) estimate < baseline else estimate > baseline
      }
    }
  }

  if (is.finite(estimate) && !is.na(metric_id)) {
    if (is.finite(ci_low) && is.finite(ci_high)) {
      performance_text = sprintf("%s = %.3f (95%% CI [%.3f, %.3f]).", metric_id, estimate, ci_low, ci_high)
    } else {
      performance_text = sprintf("%s = %.3f.", metric_id, estimate)
    }
    if (is.finite(baseline)) {
      performance_text = paste0(
        performance_text,
        sprintf(" Featureless baseline = %.3f (%s).", baseline, if (isTRUE(better_than_baseline)) "better than baseline" else if (isFALSE(better_than_baseline)) "not better than baseline" else "baseline comparison uncertain")
      )
    }
  }

  data.table::data.table(
    primary_metric_id = metric_id,
    minimize = minimize,
    estimate = estimate,
    ci_low = ci_low,
    ci_high = ci_high,
    baseline = baseline,
    better_than_baseline = better_than_baseline,
    performance_text = performance_text
  )
}

.autoiml_scope_support_label = function(iel) {
  iel = as.character(iel %||% NA_character_)[1L]
  if (is.na(iel) || !nzchar(iel)) {
    return("unclear")
  }
  switch(
    iel,
    "IEL-0" = "not yet supported",
    "IEL-1" = "only weakly supported",
    "IEL-2" = "supported with explicit guardrails",
    "IEL-3" = "strongly supported",
    "unclear"
  )
}

.autoiml_complexity_rank = function(learner_id) {
  s = toupper(gsub("[^A-Z0-9 ]", " ", as.character(learner_id %||% "")))
  s = gsub("[[:space:]]+", " ", trimws(s))

  if (!nzchar(s)) {
    return(NA_real_)
  }
  if (grepl("FEATURELESS", s)) {
    return(1)
  }
  if (grepl("LOG REG|LOGISTIC|LM$|MULTINOM|NAIVE BAYES", s)) {
    return(2)
  }
  if (grepl("RPART|TREE|CART", s)) {
    return(3)
  }
  if (grepl("RANGER|RANDOM FOREST|FOREST", s)) {
    return(4)
  }
  if (grepl("XGBOOST|BOOST|SVM", s)) {
    return(5)
  }

  4
}

.autoiml_complexity_bucket = function(learner_id) {
  rk = .autoiml_complexity_rank(learner_id)
  if (is.na(rk)) {
    return(NA_character_)
  }
  if (rk <= 2) {
    return("simple")
  }
  if (rk == 3) {
    return("moderate")
  }
  "complex"
}

.autoiml_pick_best_row = function(dt, metric_col = "mean", minimize = FALSE) {
  if (!data.table::is.data.table(dt) || nrow(dt) < 1L || !(metric_col %in% names(dt))) {
    return(NULL)
  }
  vals = as.numeric(dt[[metric_col]])
  ok = is.finite(vals)
  if (!any(ok)) {
    return(NULL)
  }
  idx_pool = which(ok)
  idx = if (isTRUE(minimize)) idx_pool[which.min(vals[ok])] else idx_pool[which.max(vals[ok])]
  dt[idx]
}

.autoiml_model_story = function(res) {
  g6 = .autoiml_get_gate_result(res, "G6")
  g2 = .autoiml_get_gate_result(res, "G2")

  if (is.null(g6)) {
    return(data.table::data.table(
      benchmark_available = FALSE,
      primary_measure_id = NA_character_,
      best_model = NA_character_,
      best_model_complexity = NA_character_,
      best_simple_model = NA_character_,
      simple_model_in_rashomon = NA,
      simplest_rashomon_model = NA_character_,
      simple_gap_to_best = NA_real_,
      dependence_flag = NA,
      interaction_flag = NA,
      answer = "No model-class comparison is available yet.",
      recommendation = "Provide alternative simple and complex learners so the framework can show whether an easier model is competitive."
    ))
  }

  perf = g6$artifacts$alt_learner_performance %||% NULL
  rashomon = g6$artifacts$rashomon_set %||% NULL
  metric_id = as.character(.autoiml_dt_scalar(g6$metrics, "primary_measure_id", .autoiml_dt_scalar(g6$artifacts$rashomon_provenance, "primary_measure_id", NA_character_)))[1L]
  minimize = .autoiml_measure_minimize(metric_id)
  if (is.na(minimize)) {
    minimize = FALSE
  }

  dependence_flag = .autoiml_dt_scalar(g2$metrics %||% NULL, "dependence_flag", NA)
  interaction_flag = .autoiml_dt_scalar(g2$metrics %||% NULL, "interaction_flag", NA)

  if (!data.table::is.data.table(perf) || nrow(perf) < 1L) {
    return(data.table::data.table(
      benchmark_available = FALSE,
      primary_measure_id = metric_id,
      best_model = NA_character_,
      best_model_complexity = NA_character_,
      best_simple_model = NA_character_,
      simple_model_in_rashomon = NA,
      simplest_rashomon_model = NA_character_,
      simple_gap_to_best = NA_real_,
      dependence_flag = dependence_flag,
      interaction_flag = interaction_flag,
      answer = "Gate 6 ran, but the alternative-learner performance table is empty.",
      recommendation = "Check the candidate learner set and rerun Gate 6."
    ))
  }

  perf = data.table::copy(perf)
  perf[, learner_id := as.character(learner_id)]
  data.table::set(perf, j = "complexity_rank", value = vapply(perf[["learner_id"]], .autoiml_complexity_rank, numeric(1L)))
  data.table::set(perf, j = "complexity_bucket", value = vapply(perf[["learner_id"]], .autoiml_complexity_bucket, character(1L)))
  perf[, in_rashomon := if ("in_rashomon" %in% names(perf)) as.logical(in_rashomon) else learner_id %in% as.character(rashomon$learner_id %||% character())]

  best = .autoiml_pick_best_row(perf, minimize = minimize)
  simple = perf[perf[["complexity_bucket"]] == "simple"]
  best_simple = .autoiml_pick_best_row(simple, minimize = minimize)

  simple_in_rashomon = FALSE
  simplest_rashomon = NULL
  if (nrow(simple) > 0L) {
    simple_in_rashomon = any(as.logical(simple$in_rashomon) %in% TRUE, na.rm = TRUE)
    if (isTRUE(simple_in_rashomon)) {
      srg = simple[as.logical(in_rashomon) %in% TRUE]
      data.table::setorderv(srg, c("complexity_rank", "mean"), order = c(1L, if (isTRUE(minimize)) 1L else -1L))
      simplest_rashomon = srg[1L]
    }
  }

  simple_gap = NA_real_
  if (!is.null(best) && !is.null(best_simple) && nrow(best) > 0L && nrow(best_simple) > 0L) {
    best_val = as.numeric(best$mean[[1L]])
    simple_val = as.numeric(best_simple$mean[[1L]])
    if (is.finite(best_val) && is.finite(simple_val)) {
      simple_gap = if (isTRUE(minimize)) simple_val - best_val else best_val - simple_val
    }
  }

  answer = "No clear simple-versus-complex story available."
  recommendation = "Inspect the Gate 6 performance table and the Gate 2 structure diagnostics together."

  if (isTRUE(simple_in_rashomon) && !is.null(simplest_rashomon)) {
    answer = sprintf(
      "A simpler model (%s) is competitive under the Rashomon rule, so the main story can usually be told with the easier model.",
      as.character(simplest_rashomon$learner_id[[1L]])
    )
    recommendation = "Use the simpler competitive model for the primary narrative and keep the more complex learner as a robustness check."
  } else if (nrow(simple) > 0L) {
    if (isTRUE(interaction_flag) || isTRUE(dependence_flag)) {
      answer = "No simple model is competitive, and Gate 2 indicates structure that simple models are likely missing."
      recommendation = "Do not over-trust linear or one-feature stories. Keep the complex model, use ALE/interaction-aware summaries, and report explicitly that the simpler model underfits this task."
    } else {
      answer = "No simple model is competitive in the benchmark, even though Gate 2 does not show strong structural alarms."
      recommendation = "Treat this as empirical evidence that the easier model loses relevant predictive signal here; report the gap and keep explanations conservative."
    }
  }

  data.table::data.table(
    benchmark_available = TRUE,
    primary_measure_id = metric_id,
    best_model = as.character(.autoiml_dt_scalar(best, "learner_id", NA_character_)),
    best_model_complexity = as.character(.autoiml_dt_scalar(best, "complexity_bucket", NA_character_)),
    best_simple_model = as.character(.autoiml_dt_scalar(best_simple, "learner_id", NA_character_)),
    simple_model_in_rashomon = simple_in_rashomon,
    simplest_rashomon_model = as.character(.autoiml_dt_scalar(simplest_rashomon, "learner_id", NA_character_)),
    simple_gap_to_best = simple_gap,
    dependence_flag = dependence_flag,
    interaction_flag = interaction_flag,
    answer = answer,
    recommendation = recommendation
  )
}

.autoiml_trust_summary = function(res) {
  rc = report_card(res)
  statuses = if (data.table::is.data.table(rc) && nrow(rc) > 0L) {
    out = as.list(as.character(rc$status))
    names(out) = as.character(rc$gate_id)
    out
  } else {
    list()
  }
  perf = .autoiml_extract_primary_performance(res)

  g1_status = as.character(statuses[["G1"]] %||% NA_character_)
  g0b_status = as.character(statuses[["G0B"]] %||% NA_character_)
  g4_status = as.character(statuses[["G4"]] %||% NA_character_)
  g5_status = as.character(statuses[["G5"]] %||% NA_character_)
  g6_status = as.character(statuses[["G6"]] %||% NA_character_)
  g3_status = as.character(statuses[["G3"]] %||% NA_character_)

  iel_global = as.character(res$iel$global %||% NA_character_)
  iel_local = as.character(res$iel$local %||% NA_character_)
  iel_decision = as.character(res$iel$decision %||% NA_character_)
  requested = as.character(res$iel$requested %||% c("global"))
  scope_label = function(scope, iel) {
    if (!scope %in% requested) {
      return("not requested")
    }
    .autoiml_scope_support_label(iel)
  }

  answer = "Performance alone is never enough to trust interpretations."
  if (identical(g1_status, "fail")) {
    answer = "No. Gate 1 did not establish predictive adequacy, so interpretation should stop until the model and/or data are improved."
  } else if (identical(g1_status, "warn")) {
    answer = "Not yet. Predictive adequacy is only partially established, so interpretation should stay exploratory and conservative."
  } else if (identical(g1_status, "pass")) {
    answer = paste0(
      "Not from performance alone. Predictive adequacy is in place, but the usable interpretation scope is still constrained by the later gates: global = ",
      scope_label("global", iel_global),
      ", local = ",
      scope_label("local", iel_local),
      ", decision = ",
      scope_label("decision", iel_decision),
      "."
    )
  }

  next_step = "Use the gate pattern, not the score alone, to decide what claims to make."
  if (identical(g0b_status, "warn") || identical(g0b_status, "fail")) {
    next_step = "Strengthen measurement/scoring documentation first, because psychological interpretation depends on construct quality as well as prediction."
  } else if (identical(g4_status, "warn") || identical(g5_status, "warn") || identical(g6_status, "warn")) {
    next_step = "Keep global claims ahead of local case narratives, and report robustness limits explicitly."
  }
  if ("decision" %in% requested && (identical(iel_decision, "IEL-0") || identical(g3_status, "warn") || identical(g3_status, "fail"))) {
    next_step = paste0(next_step, " Do not convert the score into thresholded decisions until calibration and utility evidence are in place.")
  }

  data.table::data.table(
    primary_metric_id = as.character(.autoiml_dt_scalar(perf, "primary_metric_id", NA_character_)),
    primary_estimate = as.numeric(.autoiml_dt_scalar(perf, "estimate", NA_real_)),
    primary_ci_low = as.numeric(.autoiml_dt_scalar(perf, "ci_low", NA_real_)),
    primary_ci_high = as.numeric(.autoiml_dt_scalar(perf, "ci_high", NA_real_)),
    baseline = as.numeric(.autoiml_dt_scalar(perf, "baseline", NA_real_)),
    better_than_baseline = .autoiml_dt_scalar(perf, "better_than_baseline", NA),
    g0b_status = g0b_status,
    g1_status = g1_status,
    g4_status = g4_status,
    g5_status = g5_status,
    g6_status = g6_status,
    g3_status = g3_status,
    iel_global = iel_global,
    iel_local = iel_local,
    iel_decision = iel_decision,
    performance_text = as.character(.autoiml_dt_scalar(perf, "performance_text", "Primary predictive metric unavailable.")),
    answer = answer,
    next_step = next_step
  )
}

.autoiml_reader_questions = function(res, actions = NULL) {
  trust = .autoiml_trust_summary(res)
  model_story = .autoiml_model_story(res)
  claim = (.autoiml_get_gate_result(res, "G0A")$artifacts$claim %||% list())
  semantics = as.character(claim$semantics %||% NA_character_)[1L]
  requested = as.character(res$iel$requested %||% c("global"))
  g0b = .autoiml_get_gate_result(res, "G0B")
  g2 = .autoiml_get_gate_result(res, "G2")
  g4 = .autoiml_get_gate_result(res, "G4")
  g5 = .autoiml_get_gate_result(res, "G5")
  g6 = .autoiml_get_gate_result(res, "G6")
  g7a = .autoiml_get_gate_result(res, "G7A")

  action_text = "Follow the current guide actions and keep the claim scope explicit."
  if (data.table::is.data.table(actions) && nrow(actions) > 0L) {
    action_text = paste0(actions$action[seq_len(min(3L, nrow(actions)))], collapse = " | ")
  }

  g2_metrics = g2$metrics %||% data.table::data.table()
  recommended_effect_method = as.character(.autoiml_dt_scalar(g2_metrics, "recommended_effect_method", NA_character_))
  dependence_flag = .autoiml_dt_scalar(g2_metrics, "dependence_flag", NA)
  interaction_flag = .autoiml_dt_scalar(g2_metrics, "interaction_flag", NA)

  g5_metrics = g5$metrics %||% data.table::data.table()
  unstable = .autoiml_dt_scalar(g5_metrics, "any_unstable", NA)
  g6_metrics = g6$metrics %||% data.table::data.table()
  n_rashomon = as.integer(.autoiml_dt_scalar(g6_metrics, "n_models_in_rashomon", NA_integer_))

  q = list(
    data.table::data.table(
      question_id = "performance_trust",
      reader_question = "I got this level of predictive performance. Is that sufficient to trust the interpretations?",
      short_answer = as.character(.autoiml_dt_scalar(trust, "answer", "Performance alone is not enough.")),
      why = as.character(.autoiml_dt_scalar(trust, "performance_text", "Primary predictive metric unavailable.")),
      what_to_do_now = as.character(.autoiml_dt_scalar(trust, "next_step", action_text)),
      evidence = "G1 + G4-G6 + IEL"
    ),
    data.table::data.table(
      question_id = "simple_vs_complex",
      reader_question = "A simpler model is easier to interpret. When can I trust that simpler story, and when is complexity warranted?",
      short_answer = as.character(.autoiml_dt_scalar(model_story, "answer", "No simple-versus-complex answer available yet.")),
      why = paste0(
        "Best benchmarked model: ", as.character(.autoiml_dt_scalar(model_story, "best_model", NA_character_)),
        "; best simple model: ", as.character(.autoiml_dt_scalar(model_story, "best_simple_model", NA_character_)),
        "; dependence flag = ", as.character(dependence_flag),
        "; interaction flag = ", as.character(interaction_flag), "."
      ),
      what_to_do_now = as.character(.autoiml_dt_scalar(model_story, "recommendation", action_text)),
      evidence = "G2 + G6"
    ),
    data.table::data.table(
      question_id = "next_steps",
      reader_question = "What should I do next, given this data, this task, and these results?",
      short_answer = if (nzchar(action_text)) action_text else "Follow the guide actions in priority order.",
      why = "The framework is intended to turn gate outcomes into explicit next steps rather than a single pass/fail judgment.",
      what_to_do_now = "Address the highest-priority guide actions first and rerun the workflow after major changes.",
      evidence = "guide_actions"
    ),
    data.table::data.table(
      question_id = "case_level_claims",
      reader_question = "Can I make case-level claims from the local explanations?",
      short_answer = if (!"local" %in% requested) {
        "Not part of the current Claim and Semantics Card: no case-level claim is being made."
      } else if (identical(res$iel$local %||% "IEL-0", "IEL-0")) {
        "No. Local case-level claims are not yet supported by the current evidence pattern."
      } else {
        paste0("Only with guardrails. Local claims are ", .autoiml_scope_support_label(res$iel$local), ".")
      },
      why = paste0("Gate 4 = ", as.character(g4$status %||% NA_character_), "; Gate 5 = ", as.character(g5$status %||% NA_character_), "; Gate 6 = ", as.character(g6$status %||% NA_character_), "."),
      what_to_do_now = "Treat local explanations as audited model behavior summaries, not as causal facts about the person.",
      evidence = "G4 + G5 + G6 + IEL-local"
    ),
    data.table::data.table(
      question_id = "decision_use",
      reader_question = "Can I turn the model into thresholded decisions or interventions?",
      short_answer = if (!"decision" %in% requested) {
        "Not part of the current Claim and Semantics Card: the workflow is not currently supporting thresholded decisions or interventions."
      } else if (identical(res$iel$decision %||% "IEL-0", "IEL-0")) {
        "No. Decision use is blocked until calibration, thresholds, utility assumptions, and subgroup consequences are justified."
      } else {
        paste0("Only within the explicitly evaluated decision range, because decision evidence is ", .autoiml_scope_support_label(res$iel$decision), ".")
      },
      why = paste0("Gate 3 = ", as.character(.autoiml_get_gate_result(res, "G3")$status %||% NA_character_), "; decision IEL = ", as.character(res$iel$decision %||% NA_character_), "."),
      what_to_do_now = "Specify threshold ranges and utility/cost assumptions, validate calibration, and audit subgroup impacts before acting on scores.",
      evidence = "G3 + G7A + IEL-decision"
    ),
    data.table::data.table(
      question_id = "model_or_world",
      reader_question = "Are these explanations about the world, or only about the fitted model?",
      short_answer = if (identical(semantics, "within_support")) {
        "They are descriptive statements about the fitted model on observed support, not causal effects."
      } else if (identical(semantics, "marginal_model_query")) {
        "They are model-based what-if answers about the fitted model and may rely on off-support combinations."
      } else {
        "Causal or recourse claims require assumptions that are not established by this workflow alone."
      },
      why = paste0("Declared semantics = ", semantics, "."),
      what_to_do_now = "State the semantics explicitly wherever you report effects, PDPs/ALEs, or local attributions.",
      evidence = "G0A + G2 + G4"
    ),
    data.table::data.table(
      question_id = "method_choice",
      reader_question = "Should I report PDP, ALE, SHAP, or something else?",
      short_answer = if (!is.na(recommended_effect_method) && nzchar(recommended_effect_method)) {
        paste0("Start with ", toupper(recommended_effect_method), " for the main global story, because that is what Gate 2 recommends under the observed dependence structure.")
      } else {
        "Choose the effect method from Gate 2 rather than defaulting automatically to PDP or SHAP."
      },
      why = paste0("Dependence flag = ", as.character(dependence_flag), "; interaction flag = ", as.character(interaction_flag), "; semantics = ", semantics, "."),
      what_to_do_now = "Use Gate 2 for global effect choice, then use Gate 4 only for local explanations that survive the declared semantics and faithfulness checks.",
      evidence = "G2 + G4"
    ),
    data.table::data.table(
      question_id = "instability_source",
      reader_question = "If the explanations look unstable, is that because the model itself is unstable or because many near-tied models disagree?",
      short_answer = if (isTRUE(unstable) && isTRUE(is.finite(n_rashomon) && n_rashomon >= 2L)) {
        "Potentially both: Gate 5 shows within-model instability and Gate 6 shows multiplicity across near-tied models."
      } else if (isTRUE(unstable)) {
        "Primarily within-model instability: the same model class varies too much across perturbations or resamples."
      } else if (isTRUE(is.finite(n_rashomon) && n_rashomon >= 2L)) {
        "Primarily model multiplicity: several near-tied models are plausible and they may not tell the same story."
      } else {
        "No major instability source is currently dominant, but stability should still be reported explicitly."
      },
      why = paste0("Gate 5 any_unstable = ", as.character(unstable), "; Gate 6 Rashomon size = ", if (is.finite(n_rashomon)) as.character(n_rashomon) else "NA", "."),
      what_to_do_now = "Report both Gate 5 and Gate 6 so readers can see whether instability comes from perturbation sensitivity, model multiplicity, or both.",
      evidence = "G5 + G6"
    ),
    data.table::data.table(
      question_id = "subgroups_and_measurement",
      reader_question = "Can I generalize the interpretation across subgroups, and do measurement issues matter here?",
      short_answer = if (is.null(g7a)) {
        "Subgroup and audience auditing are not part of the current claim scope, so no broad subgroup generalization should be made."
      } else if (identical(g7a$status %||% NA_character_, "pass")) {
        "Only cautiously. Subgroup auditing exists, but subgroup differences still depend on measurement comparability and construct quality."
      } else {
        "Not yet. Subgroup-facing interpretation needs both subgroup audits and explicit measurement/comparability information."
      },
      why = paste0("Gate 0B = ", as.character(g0b$status %||% NA_character_), "; Gate 7A = ", as.character(g7a$status %||% NA_character_), "."),
      what_to_do_now = "Document scoring/reliability/invariance assumptions and pair subgroup explanations with subgroup performance/calibration evidence.",
      evidence = "G0B + G7A"
    )
  )

  data.table::rbindlist(q, fill = TRUE)
}
