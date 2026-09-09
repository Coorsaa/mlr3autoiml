
.wrap_gate_error = function(gate_id, started_at, error) {
  new_gate_result(
    gate_id = gate_id,
    status = "error",
    summary = sprintf("Gate execution failed: %s", conditionMessage(error)),
    limitations = "No claim support should be inferred from an errored gate.",
    error = list(
      class = class(error),
      message = conditionMessage(error)
    ),
    started_at = started_at
  )
}

.override_gate = function(gate_id, override) {
  if (inherits(override, "CSDGGateResult")) return(override)
  if (!is.list(override)) {
    return(new_gate_result(
      gate_id,
      status = "unresolved",
      summary = "Externally supplied evidence was recorded but not status-coded.",
      evidence = list(value = override),
      limitations = "The package did not compute or independently verify this evidence."
    ))
  }
  status = override$status %||% "unresolved"
  summary = override$summary %||%
    "Externally supplied evidence was recorded; its provenance must be reviewed."
  evidence = override$evidence %||%
    override[setdiff(names(override), c(
      "status", "summary", "limitations", "thresholds", "diagnostics", "availability",
      "result_direction", "criterion", "criterion_source", "criterion_rationale", "materiality",
      "adjudication_basis", "claim_consequence", "rationale", "error"
    ))]
  new_gate_result(
    gate_id = gate_id,
    status = status,
    summary = summary,
    evidence = evidence,
    availability = override$availability %||% NULL,
    result_direction = override$result_direction %||% NULL,
    criterion = override$criterion %||% NULL,
    criterion_source = override$criterion_source %||% NULL,
    criterion_rationale = override$criterion_rationale %||% NULL,
    materiality = override$materiality %||% "not_applicable",
    adjudication_basis = override$adjudication_basis %||% NULL,
    claim_consequence = override$claim_consequence %||% "none",
    rationale = override$rationale %||% NULL,
    limitations = c(
      override$limitations %||% character(),
      "Externally supplied evidence was not independently recomputed by this call."
    ),
    thresholds = override$thresholds %||% list(),
    diagnostics = override$diagnostics %||% list(),
    error = override$error %||% NULL
  )
}

.run_gate = function(gate_id, applicable, evidence_role, execute, override, code) {
  started = .now_utc()
  if (!execute) {
    return(new_gate_result(
      gate_id,
      status = if (applicable) "unresolved" else "not_applicable",
      summary = if (applicable) {
        "The gate applies to the prespecified claim but was not executed in this call."
      } else {
        "The prespecified claim does not require this gate."
      },
      availability = if (applicable) "unavailable" else "not_applicable",
      materiality = if (applicable && identical(evidence_role, "potential_defeater")) {
        "not_materialized"
      } else {
        "not_applicable"
      },
      started_at = started
    ))
  }
  result = if (!is.null(override)) {
    .override_gate(gate_id, override)
  } else tryCatch(
    force(code)(),
    error = function(e) .wrap_gate_error(gate_id, started, e)
  )
  if (is.null(result$started_at)) {
    result$started_at = started
  }
  if (applicable &&
      identical(evidence_role, "potential_defeater") &&
      identical(result$materiality, "not_applicable")) {
    result$materiality = "not_materialized"
  }
  result
}

.evaluate_g0a = function(claim) {
  fields = c(
    statement = !identical(claim$statement, "Claim statement not yet supplied."),
    target = !is.null(claim$target),
    semantics = !is.null(claim$semantics),
    population = !is.null(claim$population),
    analytic_distribution = !is.null(claim$analytic_distribution),
    model_scope = !is.null(claim$model_scope),
    setting_scope = !is.null(claim$setting_scope),
    scientific_use = !is.null(claim$scientific_use),
    explanation_design = !is.null(claim$explanation_design),
    claim_level = !is.null(claim$claim_level),
    use_claim = is.logical(claim$use_claim) && length(claim$use_claim) == 1L && !is.na(claim$use_claim),
    claim_version = !is.null(claim$claim_version),
    revision_relation = !is.null(claim$revision_relation)
  )
  missing = names(fields)[!fields]
  decision_missing = claim$completeness$decision_fields_missing
  if ("decision" %in% claim$claim_type && length(decision_missing)) {
    status = "not_met"
  } else if (length(missing)) {
    status = "unresolved"
  } else {
    status = "met"
  }
  new_gate_result(
    "G0a",
    status = status,
    summary = if (identical(status, "met")) {
      paste(
        "The versioned claim specifies its target, semantics, analytic distribution,",
        "model and setting scope, scientific use, and explanation design."
      )
    } else {
      paste(
        "Claim card is incomplete.",
        if (length(missing)) paste("Missing:", paste(missing, collapse = ", "), "."),
        if (length(decision_missing)) {
          paste("Decision fields missing:", paste(decision_missing, collapse = ", "), ".")
        }
      )
    },
    evidence = list(
      completeness = data.table::data.table(
        field = c(
          names(fields),
          if (length(decision_missing)) paste0("decision_", decision_missing) else character()
        ),
        complete = c(unname(fields), rep(FALSE, length(decision_missing)))
      ),
      claim = .card_to_list(claim)
    ),
    limitations = if (!identical(status, "met")) {
      "Interpretation must remain provisional until the claim card is completed."
    } else character()
  )
}

.evaluate_g0b = function(measurement) {
  fields = c(
    outcome = length(measurement$outcome %||% character()) > 0L,
    predictors = length(measurement$predictors %||% character()) > 0L,
    data_source = !is.null(measurement$data_source),
    sample_definition = !is.null(measurement$sample_definition),
    missingness = !is.null(measurement$missingness),
    preprocessing = !is.null(measurement$preprocessing)
  )
  missing = names(fields)[!fields]
  reviewed = measurement$verification$status != "not_checked"
  status = if (!length(missing) && reviewed) {
    "met"
  } else if (length(missing) == length(fields)) {
    "not_met"
  } else {
    "unresolved"
  }
  new_gate_result(
    "G0b",
    status = status,
    summary = if (identical(status, "met")) {
      "Measurement, sampling, missingness, and preprocessing are documented with a review artifact."
    } else {
      paste0(
        "Measurement/preprocessing evidence is incomplete",
        if (length(missing)) paste0(": ", paste(missing, collapse = ", ")) else "",
        if (!reviewed) "; preprocessing verification is marked not checked" else "",
        "."
      )
    },
    evidence = list(
      completeness = data.table::data.table(
        field = names(fields), complete = unname(fields)
      ),
      verification = measurement$verification,
      measurement = .card_to_list(measurement)
    ),
    limitations = if (!reviewed) {
      "The package records, but cannot itself verify, whether preprocessing code was reviewed."
    } else character()
  )
}

.baseline_resample = function(task, resampling, measures, seed) {
  key = if (identical(task$task_type, "regr")) {
    "regr.featureless"
  } else {
    "classif.featureless"
  }
  baseline = mlr3::lrn(
    key,
    predict_type = if (identical(task$task_type, "classif")) "prob" else "response"
  )
  csdg_resample(
    task, baseline, resampling = resampling, measures = measures,
    store_models = FALSE, seed = seed
  )
}

.pfi_stability = function(pfi, top_k) {
  tab = data.table::copy(pfi$per_iteration)
  tab[, rank := data.table::frank(-importance, ties.method = "average"),
      by = iteration]
  n_features = data.table::uniqueN(tab$feature_group)
  if (!n_features) {
    .csdg_stop("PFI stability requires at least one feature group.")
  }
  effective_top_k = min(as.integer(top_k), n_features)
  sets = split(
    tab[rank <= effective_top_k, feature_group],
    tab[rank <= effective_top_k, iteration]
  )
  if (length(sets) < 2L) {
    return(list(
      ranks = tab,
      pairwise = data.table::data.table(),
      median_jaccard = NA_real_,
      minimum_jaccard = NA_real_,
      effective_top_k = effective_top_k
    ))
  }
  pairs = utils::combn(names(sets), 2L, simplify = FALSE)
  pairwise = data.table::rbindlist(lapply(pairs, function(pair) {
    a = sets[[pair[[1L]]]]
    b = sets[[pair[[2L]]]]
    u = union(a, b)
    data.table::data.table(
      iteration_1 = as.integer(pair[[1L]]),
      iteration_2 = as.integer(pair[[2L]]),
      intersection = length(intersect(a, b)),
      union = length(u),
      jaccard = if (length(u)) length(intersect(a, b)) / length(u) else NA_real_
    )
  }))
  list(
    ranks = tab,
    pairwise = pairwise,
    median_jaccard = stats::median(pairwise$jaccard, na.rm = TRUE),
    minimum_jaccard = min(pairwise$jaccard, na.rm = TRUE),
    effective_top_k = effective_top_k
  )
}

.resolve_primary_measure = function(config, task) {
  key = config$performance$primary
  if (is.null(key)) return(.default_measures(task)[[1L]])
  if (inherits(key, "Measure")) return(key)
  mlr3::msr(key)
}

.criterion_metadata = function(config, name) {
  config$criteria[[name]] %||% NULL
}

.criterion_row = function(criterion, observed, operator, threshold, metadata = NULL) {
  complete = !is.null(metadata) &&
    .is_scalar_string(metadata$source %||% NULL) &&
    .is_scalar_string(metadata$rationale %||% NULL)
  passed = if (!complete || !is.finite(observed) || !is.finite(threshold)) {
    NA
  } else {
    switch(
      operator,
      ">=" = observed >= threshold,
      "<=" = observed <= threshold,
      .csdg_stop("Unsupported criterion operator: %s.", operator)
    )
  }
  data.table::data.table(
    criterion = criterion,
    observed = as.numeric(observed),
    operator = operator,
    threshold = as.numeric(threshold),
    criterion_source = if (complete) metadata$source else NA_character_,
    criterion_rationale = if (complete) metadata$rationale else NA_character_,
    criterion_complete = complete,
    passed = passed
  )
}

.criteria_status = function(criteria) {
  if (!nrow(criteria) || anyNA(criteria$passed)) return("unresolved")
  if (any(!criteria$passed)) "not_met" else "met"
}

.table_value = function(table, column) {
  if (!column %in% names(table) || !length(table[[column]])) return(NA_real_)
  as.numeric(table[[column]][[1L]])
}

.run_csdg_audit = function(
    task,
    learner,
    claim,
    measurement,
    explanation,
    config,
    resampling,
    candidate_learners,
    setting_group,
    subgroup,
    local_cases,
    pfi_strata,
    pfi_cluster,
    pfi_cluster_level_groups,
    evidence,
    run_gates) {
  plan = copy(csdg_gate_plan(claim, measurement, explanation))
  plan[, execute := required]
  if (!is.null(run_gates)) {
    unknown = setdiff(run_gates, .csdg_gate_ids)
    if (length(unknown)) {
      .csdg_stop("Unknown gates in `run_gates`: %s.", paste(unknown, collapse = ", "))
    }
    plan[, execute := gate_id %in% run_gates]
  }
  required = setNames(plan$required, plan$gate_id)
  evidence_role = setNames(plan$evidence_role, plan$gate_id)
  execute = setNames(plan$execute, plan$gate_id)
  evidence = evidence %||% list()
  if (!is.list(evidence)) {
    .csdg_stop("`evidence` must be a list.")
  }
  if (length(evidence) &&
      (is.null(names(evidence)) || any(!nzchar(names(evidence))) || anyDuplicated(names(evidence)))) {
    .csdg_stop("`evidence` must have unique, non-empty names.")
  }

  gates = list()
  artifacts = new.env(parent = emptyenv())

  gates$G0a = .run_gate(
    "G0a", required[["G0a"]], evidence_role[["G0a"]], execute[["G0a"]], evidence$G0a,
    function() {
    .evaluate_g0a(claim)
  })
  gates$G0b = .run_gate(
    "G0b", required[["G0b"]], evidence_role[["G0b"]], execute[["G0b"]], evidence$G0b,
    function() {
    .evaluate_g0b(measurement)
  })

  gates$G1 = .run_gate(
    "G1", required[["G1"]], evidence_role[["G1"]], execute[["G1"]], evidence$G1,
    function() {
    .require_task(task)
    .require_learner(learner)
    if (identical(task$task_type, "classif") &&
        !identical(learner$predict_type, "prob")) {
      .csdg_stop(
        "Classification learner must use `predict_type = \"prob\"` for CSDG diagnostics."
      )
    }
    rs = resampling
    if (is.null(rs)) rs = .default_resampling(task, config)
    measures = .normalize_measures(config$performance$measures, task)
    oof = csdg_resample(
      task, learner, rs, measures = measures,
      store_models = isTRUE(config$resampling$store_models),
      seed = config$seed
    )
    perf = csdg_performance(oof)
    baseline = NULL
    if (isTRUE(config$performance$baseline)) {
      baseline = .baseline_resample(
        task, oof$resampling, measures, seed = config$seed + 500000L
      )
      baseline = csdg_performance(baseline)
    }
    artifacts$oof = oof
    artifacts$performance = perf
    artifacts$baseline_performance = baseline
    primary = .resolve_primary_measure(config, task)
    primary_row = perf$summary[measure == primary$id]
    if (!nrow(primary_row)) {
      .csdg_stop("Primary measure `%s` was not included in the performance results.", primary$id)
    }
    primary_score = primary_row$mean[[1L]]
    criteria = list()
    if (!is.null(config$performance$minimum_primary_score)) {
      criteria[[length(criteria) + 1L]] = .criterion_row(
        "minimum_primary_score",
        primary_score,
        ">=",
        config$performance$minimum_primary_score,
        .criterion_metadata(config, "performance.minimum_primary_score")
      )
    }
    if (!is.null(config$performance$maximum_primary_score)) {
      criteria[[length(criteria) + 1L]] = .criterion_row(
        "maximum_primary_score",
        primary_score,
        "<=",
        config$performance$maximum_primary_score,
        .criterion_metadata(config, "performance.maximum_primary_score")
      )
    }
    if (!is.null(config$performance$minimum_baseline_improvement)) {
      baseline_row = if (is.null(baseline)) NULL else baseline$summary[measure == primary$id]
      baseline_score = if (is.null(baseline_row) || !nrow(baseline_row)) NA_real_ else baseline_row$mean[[1L]]
      improvement = if (identical(.measure_direction(primary), "minimize")) {
        baseline_score - primary_score
      } else {
        primary_score - baseline_score
      }
      criteria[[length(criteria) + 1L]] = .criterion_row(
        "minimum_baseline_improvement",
        improvement,
        ">=",
        config$performance$minimum_baseline_improvement,
        .criterion_metadata(config, "performance.minimum_baseline_improvement")
      )
    }
    criteria = data.table::rbindlist(criteria, fill = TRUE)
    status = .criteria_status(criteria)
    new_gate_result(
      "G1",
      status = status,
      summary = if (identical(status, "unresolved")) {
        "Held-out performance was computed, but claim-relevant adequacy criteria were not fully evaluated."
      } else if (identical(status, "met")) {
        "Held-out performance met all prespecified adequacy criteria."
      } else {
        "Held-out performance did not meet at least one prespecified adequacy criterion."
      },
      evidence = list(
        performance = perf,
        baseline = baseline,
        criteria = criteria,
        fold_assignments = data.table::data.table(
          iteration = seq_along(oof$test_sets),
          n_train = lengths(oof$train_sets),
          n_assessment = lengths(oof$test_sets)
        )
      ),
      thresholds = config$performance[c(
        "minimum_primary_score",
        "maximum_primary_score",
        "minimum_baseline_improvement"
      )],
      diagnostics = list(primary_measure = primary$id),
      limitations = c(
        oof$uncertainty_scope,
        "Adequacy remains claim- and loss-dependent; the package does not choose a universal cutoff."
      )
    )
  })

  gates$G2 = .run_gate(
    "G2", required[["G2"]], evidence_role[["G2"]], execute[["G2"]], evidence$G2,
    function() {
    .require_task(task)
    dep = csdg_dependence(
      task$data(cols = task$feature_names),
      features = task$feature_names,
      max_levels = config$dependence$max_levels
    )
    artifacts$dependence = dep
    new_gate_result(
      "G2",
      status = "met",
      summary = "Mixed-type dependence and observed feature support were characterized.",
      evidence = dep,
      result_direction = "descriptive",
      materiality = "not_materialized",
      limitations = dep$limitations
    )
  })

  gates$G3a = .run_gate(
    "G3a", required[["G3a"]], evidence_role[["G3a"]], execute[["G3a"]], evidence$G3a,
    function() {
    oof = artifacts$oof
    if (is.null(oof)) {
      .csdg_stop("G3a requires out-of-fold predictions from G1 or supplied evidence.")
    }
    cal = csdg_calibration(
      oof,
      bins = config$calibration$bins,
      collapse_repeats = TRUE
    )
    artifacts$calibration = cal
    criteria = list()
    calibration_criteria = list(
      maximum_brier = c("brier", "<="),
      maximum_logloss = c("logloss", "<="),
      maximum_ece = c("expected_calibration_error", "<="),
      maximum_rmse = c("rmse", "<="),
      maximum_abs_intercept = c("calibration_in_the_large", "<=")
    )
    for (name in names(calibration_criteria)) {
      threshold = config$calibration[[name]]
      if (is.null(threshold)) next
      column = calibration_criteria[[name]][[1L]]
      observed = .table_value(cal$summary, column)
      if (identical(name, "maximum_abs_intercept")) observed = abs(observed)
      criteria[[length(criteria) + 1L]] = .criterion_row(
        name,
        observed,
        calibration_criteria[[name]][[2L]],
        threshold,
        .criterion_metadata(config, paste0("calibration.", name))
      )
    }
    slope_range = config$calibration$calibration_slope_range
    if (!is.null(slope_range)) {
      slope = .table_value(cal$summary, "calibration_slope")
      criteria[[length(criteria) + 1L]] = .criterion_row(
        "minimum_calibration_slope",
        slope,
        ">=",
        slope_range[[1L]],
        .criterion_metadata(config, "calibration.minimum_slope")
      )
      criteria[[length(criteria) + 1L]] = .criterion_row(
        "maximum_calibration_slope",
        slope,
        "<=",
        slope_range[[2L]],
        .criterion_metadata(config, "calibration.maximum_slope")
      )
    }
    criteria = data.table::rbindlist(criteria, fill = TRUE)
    status = .criteria_status(criteria)
    new_gate_result(
      "G3a",
      status = status,
      summary = if (identical(status, "met")) {
        "Out-of-fold calibration met all prespecified claim-relevant criteria."
      } else if (identical(status, "unresolved")) {
        "Out-of-fold calibration was computed, but adequacy remains unresolved without claim-relevant criteria."
      } else {
        "At least one prespecified calibration criterion was not met."
      },
      evidence = list(
        calibration = cal,
        criteria = criteria
      ),
      thresholds = config$calibration,
      limitations = c(
        "Calibration is evaluated for the analytic sample and chosen prediction horizon.",
        "Binned summaries are descriptive; flexible curves and uncertainty should be used for inferential claims."
      )
    )
  })

  gates$G3b = .run_gate(
    "G3b", required[["G3b"]], evidence_role[["G3b"]], execute[["G3b"]], evidence$G3b,
    function() {
    oof = artifacts$oof
    if (is.null(oof)) {
      .csdg_stop("G3b requires out-of-fold predictions from G1 or supplied evidence.")
    }
    if (!identical(task$task_type, "classif")) {
      .csdg_stop("G3b decision-curve evidence is currently implemented for binary classification.")
    }
    decision_curve = csdg_decision_curve(
      oof,
      thresholds = config$decision$thresholds,
      collapse_repeats = TRUE
    )
    artifacts$decision_curve = decision_curve
    missing_fields = claim$completeness$decision_fields_missing
    criteria = list()
    if (!is.null(config$decision$minimum_fraction_beneficial)) {
      beneficial_fraction = mean(
        decision_curve$net_benefit_model >
          pmax(decision_curve$net_benefit_treat_all, decision_curve$net_benefit_treat_none)
      )
      criteria[[1L]] = .criterion_row(
        "minimum_fraction_beneficial",
        beneficial_fraction,
        ">=",
        config$decision$minimum_fraction_beneficial,
        .criterion_metadata(config, "decision.minimum_fraction_beneficial")
      )
    }
    criteria = data.table::rbindlist(criteria, fill = TRUE)
    status = if (length(missing_fields)) "not_met" else .criteria_status(criteria)
    new_gate_result(
      "G3b",
      status = status,
      summary = if (length(missing_fields)) {
        sprintf("The decision claim is incomplete: %s.", paste(missing_fields, collapse = ", "))
      } else if (identical(status, "met")) {
        "Decision consequences met all prespecified claim-relevant criteria."
      } else if (identical(status, "unresolved")) {
        "Decision curves were computed, but utility adequacy remains unresolved without a use-linked criterion."
      } else {
        "At least one prespecified decision criterion was not met."
      },
      evidence = list(decision_curve = decision_curve, criteria = criteria),
      thresholds = config$decision,
      limitations = c(
        "Net benefit is conditional on the declared threshold trade-off and reference strategy.",
        "Decision curves do not establish stakeholder utility, workflow feasibility, implementation benefit, or harms."
      )
    )
  })

  gates$G4 = .run_gate(
    "G4", required[["G4"]], evidence_role[["G4"]], execute[["G4"]], evidence$G4,
    function() {
    .require_task(task)
    .require_learner(learner)
    cases = local_cases %||% explanation$local_cases
    if (is.null(cases)) {
      return(new_gate_result(
        "G4",
        status = "unresolved",
        summary = "No prespecified local cases were supplied; local faithfulness was not evaluated.",
        limitations = "Local explanations must be described as illustrative, not generally validated."
      ))
    }
    held_out = is.numeric(cases) && !is.null(artifacts$oof) &&
      !is.null(artifacts$oof$models)
    if (held_out) {
      local = csdg_oof_local_surrogate(
        x = artifacts$oof,
        cases = cases,
        n_perturb = config$faithfulness$n_perturb,
        kernel_width = config$faithfulness$kernel_width,
        target_scale = config$faithfulness$target_scale,
        neighborhood_method = config$faithfulness$neighborhood_method,
        empirical_neighbors = config$faithfulness$empirical_neighbors,
        seed = config$seed,
        selection = explanation$case_selection
      )
    } else {
      local = csdg_local_surrogate(
        learner = learner,
        task = task,
        cases = cases,
        background = explanation$background,
        n_perturb = config$faithfulness$n_perturb,
        kernel_width = config$faithfulness$kernel_width,
        target_scale = config$faithfulness$target_scale,
        neighborhood_method = config$faithfulness$neighborhood_method,
        empirical_neighbors = config$faithfulness$empirical_neighbors,
        seed = config$seed
      )
      local$limitations = c(
        local$limitations,
        paste(
          "The supplied feature rows could not be linked to held-out row ids;",
          "the package cannot verify that their model excluded them during fitting."
        )
      )
    }
    artifacts$local_faithfulness = local
    criterion_state = new.env(parent = emptyenv())
    criterion_state$rows = list()
    add_local_criterion = function(name, observed, operator, threshold) {
      if (!is.null(threshold)) {
        criterion_state$rows = c(
          criterion_state$rows,
          list(.criterion_row(
            name,
            observed,
            operator,
            threshold,
            .criterion_metadata(config, paste0("faithfulness.", name))
          ))
        )
      }
    }
    add_local_criterion(
      "minimum_weighted_r2",
      min(local$summary$weighted_r2, na.rm = TRUE),
      ">=",
      config$faithfulness$min_weighted_r2
    )
    add_local_criterion(
      "maximum_weighted_rmse",
      max(local$summary$weighted_rmse, na.rm = TRUE),
      "<=",
      config$faithfulness$maximum_weighted_rmse
    )
    add_local_criterion(
      "maximum_weighted_mae",
      max(local$summary$weighted_mae, na.rm = TRUE),
      "<=",
      config$faithfulness$maximum_weighted_mae
    )
    add_local_criterion(
      "maximum_target_case_absolute_error",
      max(local$summary$target_case_absolute_error, na.rm = TRUE),
      "<=",
      config$faithfulness$maximum_target_case_absolute_error
    )
    criteria = rbindlist(criterion_state$rows, fill = TRUE)
    criteria_status = .criteria_status(criteria)
    post_hoc = identical(explanation$case_selection, "post_hoc_communication")
    status = if (identical(criteria_status, "not_met")) {
      "not_met"
    } else if (!held_out || post_hoc || identical(criteria_status, "unresolved")) {
      "unresolved"
    } else {
      "met"
    }
    new_gate_result(
      "G4",
      status = status,
      summary = if (!nrow(criteria)) {
        paste(
          "Local fidelity was computed, but adequacy remains unresolved because no use-linked",
          "relative or absolute error criterion was supplied."
        )
      } else if (post_hoc && identical(criteria_status, "met")) {
        paste(
          "Held-out local fidelity was adequate for the selected communication cases,",
          "but post hoc case selection does not establish general local faithfulness."
        )
      } else if (!held_out) {
        "Local surrogate fidelity was computed, but held-out case status could not be verified."
      } else if (identical(criteria_status, "met")) {
        "All evaluated held-out local neighborhoods met the supplied claim-relevant fidelity criteria."
      } else {
        "At least one evaluated held-out local neighborhood did not meet a supplied claim-relevant criterion."
      },
      evidence = c(local, list(criteria = criteria)),
      thresholds = config$faithfulness[c(
        "min_weighted_r2", "maximum_weighted_rmse", "maximum_weighted_mae",
        "maximum_target_case_absolute_error"
      )],
      limitations = local$limitations
    )
  })

  gates$G5 = .run_gate(
    "G5", required[["G5"]], evidence_role[["G5"]], execute[["G5"]], evidence$G5,
    function() {
    oof = artifacts$oof
    if (is.null(oof)) {
      .csdg_stop("G5 requires stored fold models and splits from G1 or supplied evidence.")
    }
    pfi = csdg_fold_pfi(
      oof,
      feature_groups = explanation$feature_groups,
      loss = config$stability$loss,
      repetitions = config$stability$pfi_repetitions,
      strata = pfi_strata,
      cluster = pfi_cluster,
      cluster_level_groups = pfi_cluster_level_groups,
      seed = config$seed + 1000000L
    )
    stability = .pfi_stability(pfi, config$stability$top_k)
    artifacts$pfi = pfi
    artifacts$pfi_stability = stability
    median_j = stability$median_jaccard
    stability_cutoff = config$stability$min_top_k_overlap
    stability_criteria = if (is.null(stability_cutoff)) {
      data.table()
    } else {
      .criterion_row(
        "minimum_median_jaccard",
        median_j,
        ">=",
        stability_cutoff,
        .criterion_metadata(config, "stability.min_top_k_overlap")
      )
    }
    status = .criteria_status(stability_criteria)
    new_gate_result(
      "G5",
      status = status,
      summary = if (!is.finite(median_j)) {
        paste(
          "Held-out permutation importance was computed,",
          "but too few iterations were available for rank-stability assessment."
        )
      } else if (is.null(stability_cutoff)) {
        sprintf(
          "Median pairwise top-%d Jaccard agreement was %.3f; no claim-relevant adequacy criterion was supplied.",
          stability$effective_top_k, median_j
        )
      } else {
        sprintf(
          "Median pairwise top-%d Jaccard agreement across iterations was %.3f.",
          stability$effective_top_k, median_j
        )
      },
      evidence = list(pfi = pfi, stability = stability, criteria = stability_criteria),
      thresholds = list(
        requested_top_k = config$stability$top_k,
        effective_top_k = stability$effective_top_k,
        minimum_median_jaccard = stability_cutoff
      ),
      limitations = pfi$limitations
    )
  })

  gates$G6a = .run_gate(
    "G6a", required[["G6a"]], evidence_role[["G6a"]], execute[["G6a"]], evidence$G6a,
    function() {
    if (is.null(candidate_learners)) {
      return(new_gate_result(
        "G6a",
        status = "unresolved",
        summary = "No prespecified candidate learners were supplied for model-multiplicity assessment.",
        limitations = "A selected-model explanation cannot be generalized to a model class without comparison models."
      ))
    }
    if (is.null(config$generalization$rashomon_tolerance_absolute) &&
        is.null(config$generalization$rashomon_tolerance_relative)) {
      return(new_gate_result(
        "G6a",
        status = "unresolved",
        summary = "Candidate learners were supplied, but no claim-relevant near-equivalence rule was declared.",
        limitations = "Model-multiplicity evidence requires a substantively justified predictive equivalence rule."
      ))
    }
    tolerance_metadata = .criterion_metadata(config, "generalization.rashomon_tolerance")
    if (is.null(tolerance_metadata)) {
      return(new_gate_result(
        "G6a",
        status = "unresolved",
        summary = paste(
          "A near-equivalence tolerance was supplied without a recorded source and rationale;",
          "no accepted model set was constructed."
        ),
        limitations = "A numerical tolerance is not a universal or self-justifying model-adequacy criterion."
      ))
    }
    learners = candidate_learners
    if (is.null(names(learners)) || !any(vapply(
      learners,
      function(candidate) identical(candidate$id, learner$id),
      logical(1L)
    ))) {
      learners = c(list(focal = learner), learners)
    }
    primary = .resolve_primary_measure(config, task)
    rs = resampling %||% artifacts$oof$resampling
    model_evidence = csdg_rashomon(
      task,
      learners,
      rs,
      primary_measure = primary,
      tolerance_absolute = config$generalization$rashomon_tolerance_absolute,
      tolerance_relative = config$generalization$rashomon_tolerance_relative,
      tolerance_source = tolerance_metadata$source,
      tolerance_rationale = tolerance_metadata$rationale,
      reference_learner = config$generalization$rashomon_reference_learner,
      seed = config$seed + 2000000L
    )
    accepted_names = model_evidence$candidates[accepted == TRUE, learner_name]
    if (length(explanation$method_ids) && length(accepted_names)) {
      model_pfi = lapply(seq_along(accepted_names), function(index) {
        name = accepted_names[[index]]
        csdg_fold_pfi(
          model_evidence$resamples[[name]],
          feature_groups = explanation$feature_groups,
          loss = config$stability$loss,
          repetitions = config$stability$pfi_repetitions,
          strata = pfi_strata,
          cluster = pfi_cluster,
          cluster_level_groups = pfi_cluster_level_groups,
          seed = config$seed + 2100000L + index * 10000L
        )
      })
      names(model_pfi) = accepted_names
      model_evidence$pfi = model_pfi
      model_evidence$explanation_agreement = csdg_rashomon_agreement(
        model_evidence,
        model_pfi,
        top_k = config$stability$top_k
      )
    }
    artifacts$rashomon = model_evidence
    focal = model_evidence$candidates[learner_id == learner$id]
    focal_present = nrow(focal) > 0L
    focal_accepted = focal_present && any(focal$accepted %in% TRUE)
    agreement = model_evidence$explanation_agreement$pairwise_top_k %||% data.table()
    median_jaccard = if (nrow(agreement)) median(agreement$jaccard, na.rm = TRUE) else NA_real_
    agreement_cutoff = config$stability$min_top_k_overlap
    agreement_criterion = if (is.null(agreement_cutoff)) {
      data.table()
    } else {
      .criterion_row(
        "minimum_median_jaccard",
        median_jaccard,
        ">=",
        agreement_cutoff,
        .criterion_metadata(config, "stability.min_top_k_overlap")
      )
    }
    status = if (!focal_present) {
      "unresolved"
    } else if (!focal_accepted) {
      "not_met"
    } else {
      .criteria_status(agreement_criterion)
    }
    new_gate_result(
      "G6a",
      status = status,
      summary = if (!focal_present) {
        "The focal learner was not identifiable among the model-multiplicity candidates."
      } else if (!focal_accepted) {
        "The focal learner was outside the prespecified near-equivalence set."
      } else if (!is.finite(median_jaccard)) {
        "Fewer than two accepted models were available for explanation comparison."
      } else if (is.null(agreement_cutoff)) {
        sprintf(
          "Median accepted-model top-%d Jaccard agreement was %.3f; no claim-relevant criterion was supplied.",
          config$stability$top_k,
          median_jaccard
        )
      } else {
        sprintf("Median accepted-model top-%d Jaccard agreement was %.3f.", config$stability$top_k, median_jaccard)
      },
      evidence = list(model_generalization = model_evidence, criteria = agreement_criterion),
      thresholds = list(
        rashomon_tolerance_absolute = config$generalization$rashomon_tolerance_absolute,
        rashomon_tolerance_relative = config$generalization$rashomon_tolerance_relative,
        rashomon_reference_learner = config$generalization$rashomon_reference_learner,
        minimum_median_jaccard = config$stability$min_top_k_overlap
      ),
      diagnostics = list(
        focal_present = focal_present,
        focal_accepted = focal_accepted,
        median_jaccard = median_jaccard
      ),
      limitations = c(
        "Near-equivalent models are defined by the prespecified predictive tolerance.",
        "Explanation portability is evaluated only for the accepted candidates and perturbation design."
      )
    )
  })

  gates$G6b = .run_gate(
    "G6b", required[["G6b"]], evidence_role[["G6b"]], execute[["G6b"]], evidence$G6b,
    function() {
    if (is.null(setting_group)) {
      return(new_gate_result(
        "G6b",
        status = "unresolved",
        summary = "No observed-setting identifier was supplied for setting-boundary assessment.",
        limitations = "Observed-setting refits cannot establish performance in an unobserved setting."
      ))
    }
    setting_evidence = csdg_leave_one_group_out(
      task,
      learner,
      setting_group,
      measures = config$performance$measures,
      seed = config$seed + 3000000L
    )
    artifacts$setting_transport = setting_evidence
    primary = .resolve_primary_measure(config, task)
    transport_key = if (isTRUE(primary$minimize)) {
      "generalization.maximum_transport_score"
    } else {
      "generalization.minimum_transport_score"
    }
    transport_metadata = .criterion_metadata(config, transport_key)
    threshold_status = csdg_transport_status(
      setting_evidence$scores,
      measure = primary$id,
      direction = if (isTRUE(primary$minimize)) "minimize" else "maximize",
      minimum_transport_score = config$generalization$minimum_transport_score,
      maximum_transport_score = config$generalization$maximum_transport_score,
      criterion_source = transport_metadata$source %||% NULL,
      criterion_rationale = transport_metadata$rationale %||% NULL
    )
    new_gate_result(
      "G6b",
      status = threshold_status$status,
      summary = if (identical(threshold_status$status, "met")) {
        "Observed-setting performance met the prespecified claim-relevant criterion."
      } else if (identical(threshold_status$status, "not_met")) {
        "Observed-setting performance did not meet the prespecified claim-relevant criterion."
      } else {
        "Observed-setting refits were computed, but setting adequacy remains unresolved without a criterion."
      },
      evidence = list(setting_generalization = setting_evidence, setting_threshold = threshold_status),
      thresholds = list(
        minimum_transport_score = config$generalization$minimum_transport_score,
        maximum_transport_score = config$generalization$maximum_transport_score
      ),
      limitations = c(
        "Setting refits characterize only the observed settings and declared setting unit.",
        "They do not establish causal transport or performance in a newly sampled setting."
      )
    )
  })

  gates$G7a = .run_gate(
    "G7a", required[["G7a"]], evidence_role[["G7a"]], execute[["G7a"]], evidence$G7a,
    function() {
    oof = artifacts$oof
    if (is.null(oof)) {
      .csdg_stop("G7a requires selected-model out-of-fold predictions from G1.")
    }
    if (is.null(subgroup)) {
      return(new_gate_result(
        "G7a",
        status = "unresolved",
        summary = "Prespecified subgroup identifiers were not supplied.",
        limitations = "No subgroup or fairness conclusion should be inferred."
      ))
    }
    subgroup_evidence = csdg_subgroup_metrics(
      oof,
      subgroup = subgroup,
      min_n = config$subgroup$min_n,
      threshold = config$subgroup$thresholds[[1L]],
      bootstrap_repetitions = config$subgroup$conditional_bootstrap,
      seed = config$seed + 700000L
    )
    artifacts$subgroups = subgroup_evidence
    criteria = list()
    if (!is.null(config$subgroup$maximum_gap)) {
      metric = config$subgroup$metric
      if (!metric %in% names(subgroup_evidence$metrics)) {
        .csdg_stop("Configured subgroup metric `%s` is unavailable.", metric)
      }
      values = subgroup_evidence$metrics[[metric]]
      values = values[is.finite(values)]
      observed_gap = if (length(values)) max(values) - min(values) else NA_real_
      criteria[[1L]] = .criterion_row(
        "maximum_subgroup_gap",
        observed_gap,
        "<=",
        config$subgroup$maximum_gap,
        .criterion_metadata(config, "subgroup.maximum_gap")
      )
    }
    criteria = data.table::rbindlist(criteria, fill = TRUE)
    insufficient_n = any(subgroup_evidence$metrics$below_minimum_n)
    status = if (insufficient_n || !nrow(criteria)) {
      "unresolved"
    } else {
      .criteria_status(criteria)
    }
    new_gate_result(
      "G7a",
      status = status,
      summary = if (identical(status, "met")) {
        "Technical subgroup behavior met all prespecified claim-relevant criteria."
      } else if (identical(status, "not_met")) {
        "At least one prespecified technical subgroup criterion was not met."
      } else if (insufficient_n) {
        "At least one subgroup was below the prespecified minimum sample size."
      } else {
        "Technical subgroup behavior was summarized, but adequacy remains unresolved without a criterion."
      },
      evidence = list(subgroup_audit = subgroup_evidence, criteria = criteria),
      thresholds = list(
        subgroup_metric = config$subgroup$metric,
        maximum_subgroup_gap = config$subgroup$maximum_gap,
        minimum_subgroup_n = config$subgroup$min_n
      ),
      limitations = subgroup_evidence$limitations %||% character()
    )
  })

  gates$G7b = .run_gate(
    "G7b", required[["G7b"]], evidence_role[["G7b"]], execute[["G7b"]], evidence$G7b,
    function() {
    audience_evidence = evidence$audience_evidence %||% NULL
    if (is.null(audience_evidence)) {
      return(new_gate_result(
        "G7b",
        status = "unresolved",
        summary = "Audience, workflow, implementation, utility, and harm evidence was not supplied.",
        limitations = "Model diagnostics do not substitute for evidence about people or work systems."
      ))
    }
    supplied_status = audience_evidence$status %||% "unresolved"
    if (!supplied_status %in% c("met", "not_met", "unresolved")) {
      supplied_status = "unresolved"
    }
    new_gate_result(
      "G7b",
      status = supplied_status,
      summary = audience_evidence$summary %||% paste(
        "Audience and workflow evidence was recorded; its substantive adequacy remains an analyst judgment."
      ),
      evidence = list(audience_use_evidence = audience_evidence),
      limitations = c(
        audience_evidence$limitations %||% character(),
        "The package records this evidence but cannot infer human understanding, appropriate reliance, or benefit."
      )
    )
  })

  new_csdg_result(
    claim = claim,
    measurement = measurement,
    explanation = explanation,
    config = config,
    plan = plan,
    gates = gates,
    artifacts = as.list(artifacts, all.names = TRUE),
    metadata = list(
      task_id = if (!is.null(task)) task$id else NULL,
      learner_id = if (!is.null(learner)) learner$id else NULL,
      run_gates = run_gates %||% .csdg_gate_ids
    )
  )
}

#' @rdname csdg_audit
#' @export
csdg_audit = function(
    task,
    learner,
    claim,
    measurement,
    explanation = NULL,
    config = csdg_config(),
    resampling = NULL,
    candidate_learners = NULL,
    setting_group = NULL,
    subgroup = NULL,
    local_cases = NULL,
    pfi_strata = NULL,
    pfi_cluster = NULL,
    pfi_cluster_level_groups = NULL,
    evidence = list(),
    run_gates = NULL,
    output_dir = NULL,
    ...) {
  dots = list(...)
  .assert_named_dots(dots)
  known_dots = c(
    "explanations", "learners", "transport_group", "setting",
    "subgroups", "audit_data", "cases", "gate_evidence"
  )
  unknown_dots = setdiff(names(dots), known_dots)
  if (length(unknown_dots)) {
    .csdg_stop("Unknown `csdg_audit()` argument(s): %s.", paste(unknown_dots, collapse = ", "))
  }
  .require_task(task)
  .require_learner(learner)
  checkmate::assert_class(claim, "CSDGClaim")
  checkmate::assert_class(measurement, "CSDGMeasurement")
  checkmate::assert_class(config, "CSDGConfig")
  checkmate::assert_list(evidence)
  if (length(evidence)) .assert_named_list(evidence, "evidence")

  explanation = explanation %||% dots$explanations
  candidate_learners = candidate_learners %||% dots$learners
  setting_group = setting_group %||% dots$transport_group %||% dots$setting
  subgroup = subgroup %||% dots$subgroups %||% dots$audit_data
  local_cases = local_cases %||% dots$cases
  if (!is.null(dots$gate_evidence)) .assert_named_list(dots$gate_evidence, "gate_evidence")
  evidence = .recursive_modify(evidence, dots$gate_evidence %||% list())
  if (length(evidence)) .assert_named_list(evidence, "evidence")

  if (inherits(candidate_learners, "Learner")) {
    candidate_learners = stats::setNames(list(candidate_learners), candidate_learners$id)
  }

  checkmate::assert_true(
    is.null(resampling) || inherits(resampling, c("Resampling", "CSDGGroupedResampling")),
    .var.name = "resampling"
  )
  checkmate::assert_true(
    is.null(candidate_learners) || is.list(candidate_learners),
    .var.name = "candidate_learners"
  )
  if (is.list(candidate_learners) && length(candidate_learners)) {
    checkmate::assert_true(
      all(vapply(candidate_learners, .is_mlr3_learner, logical(1L))),
      .var.name = "candidate_learners"
    )
  }
  if (!is.null(setting_group)) checkmate::assert_atomic(setting_group)
  if (!is.null(pfi_strata)) checkmate::assert_atomic(pfi_strata)
  if (!is.null(pfi_cluster)) checkmate::assert_atomic(pfi_cluster)
  checkmate::assert_character(
    pfi_cluster_level_groups,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    .var.name = "pfi_cluster_level_groups"
  )
  checkmate::assert_true(is.null(subgroup) || is.atomic(subgroup) || is.data.frame(subgroup), .var.name = "subgroup")
  checkmate::assert_true(
    is.null(local_cases) || checkmate::test_integerish(local_cases, any.missing = FALSE, min.len = 1L) ||
      checkmate::test_data_frame(local_cases, min.rows = 1L),
    .var.name = "local_cases"
  )
  if (!is.null(run_gates)) {
    checkmate::assert_character(run_gates, any.missing = FALSE, min.len = 1L, unique = TRUE)
    checkmate::assert_subset(run_gates, .csdg_gate_ids, empty.ok = FALSE)
  }
  if (!is.null(output_dir)) {
    checkmate::assert_string(output_dir, min.chars = 1L)
  }

  explanation = explanation %||% csdg_explanation()
  checkmate::assert_class(explanation, "CSDGExplanation")

  result = .run_csdg_audit(
    task = task,
    learner = learner,
    claim = claim,
    measurement = measurement,
    explanation = explanation,
    config = config,
    resampling = resampling,
    candidate_learners = candidate_learners,
    setting_group = setting_group,
    subgroup = subgroup,
    local_cases = local_cases,
    pfi_strata = pfi_strata,
    pfi_cluster = pfi_cluster,
    pfi_cluster_level_groups = pfi_cluster_level_groups,
    evidence = evidence,
    run_gates = run_gates
  )
  if (!is.null(output_dir)) csdg_export(result, output_dir)
  result
}

#' Stateful CSDG audit runner
#'
#' `CSDGAudit` stores a claim-scoped audit specification and provides methods to run, reset, and export it.
#'
#' @usage NULL
#' @format An [R6::R6Class] object.
#' @return A new `CSDGAudit` object.
#' @export
CSDGAudit = R6::R6Class(
  "CSDGAudit",
  public = list(
    #' @field task
    #' An [mlr3::Task].
    task = NULL,

    #' @field learner
    #' The focal [mlr3::Learner].
    learner = NULL,

    #' @field claim
    #' A `CSDGClaim` object.
    claim = NULL,

    #' @field measurement
    #' A `CSDGMeasurement` object.
    measurement = NULL,

    #' @field explanation
    #' A `CSDGExplanation` object.
    explanation = NULL,

    #' @field config
    #' A `CSDGConfig` object.
    config = NULL,

    #' @field inputs
    #' Named optional inputs forwarded to [csdg_audit()].
    inputs = NULL,

    #' @field result
    #' The latest `CSDGResult`, or `NULL` before the audit is run.
    result = NULL,

    #' @description
    #' Create a stateful CSDG audit runner.
    #'
    #' @param task An [mlr3::Task].
    #' @param learner The focal [mlr3::Learner].
    #' @param claim A `CSDGClaim` object.
    #' @param measurement A `CSDGMeasurement` object.
    #' @param explanation A `CSDGExplanation` object or `NULL`.
    #' @param config A `CSDGConfig` object.
    #' @param ... Named optional inputs forwarded to [csdg_audit()].
    #' @return The `CSDGAudit` object, invisibly.
    initialize = function(
        task,
        learner,
        claim,
        measurement,
        explanation = NULL,
        config = csdg_config(),
        ...) {
      inputs = list(...)
      .require_task(task)
      .require_learner(learner)
      checkmate::assert_class(claim, "CSDGClaim")
      checkmate::assert_class(measurement, "CSDGMeasurement")
      checkmate::assert_class(config, "CSDGConfig")
      if (!is.null(explanation)) checkmate::assert_class(explanation, "CSDGExplanation")
      .assert_named_dots(inputs)
      self$task = task
      self$learner = learner
      self$claim = claim
      self$measurement = measurement
      self$explanation = explanation %||% csdg_explanation()
      self$config = config
      self$inputs = inputs
      invisible(self)
    },

    #' @description
    #' Run the configured audit.
    #'
    #' @param ... Named inputs that override values supplied during initialization.
    #' @return The resulting `CSDGResult`, invisibly.
    run = function(...) {
      dots = list(...)
      .assert_named_dots(dots)
      args = .recursive_modify(self$inputs, dots)
      self$result = do.call(
        csdg_audit,
        c(
          list(
            task = self$task,
            learner = self$learner,
            claim = self$claim,
            measurement = self$measurement,
            explanation = self$explanation,
            config = self$config
          ),
          args
        )
      )
      invisible(self$result)
    },

    #' @description
    #' Clear the stored result without changing the audit specification.
    #'
    #' @return The `CSDGAudit` object, invisibly.
    reset = function() {
      self$result = NULL
      invisible(self)
    },

    #' @description
    #' Export the stored result with [csdg_export()].
    #'
    #' @param path Parent export directory.
    #' @param prefix Stable bundle prefix.
    #' @param ... Additional arguments passed to [csdg_export()].
    #' @return The bundle path, invisibly.
    export = function(path, prefix = NULL, ...) {
      checkmate::assert_string(path, min.chars = 1L)
      if (!is.null(prefix)) checkmate::assert_string(prefix, min.chars = 1L)
      .assert_named_dots(list(...))
      if (is.null(self$result)) {
        .csdg_stop("Run the audit before exporting.")
      }
      csdg_export(self$result, path = path, prefix = prefix, ...)
    },

    #' @description
    #' Print the runner state.
    #'
    #' @param ... Unused.
    #' @return The `CSDGAudit` object, invisibly.
    print = function(...) {
      cat("<CSDGAudit>\n")
      cat("  Claim:", self$claim$id, "\n")
      cat("  State:", if (is.null(self$result)) "not run" else "complete", "\n")
      invisible(self)
    }
  )
)
