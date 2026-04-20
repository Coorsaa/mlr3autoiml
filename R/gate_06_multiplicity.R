#' @title Gate 6: Multiplicity and Transport
#'
#' @description
#' Implements multiplicity (Rashomon effect) and transportability diagnostics.
#'
#' \strong{Multiplicity:} Evaluates performance across alternative model classes and
#' applies a conservative Rashomon set selection using the 1-SE rule by default.
#' When multiple models achieve near-tie performance, computes explanation dispersion
#' via permutation importance rank correlations.
#'
#' \strong{Transportability:} Assesses subgroup performance heterogeneity if a Task
#' `group` role is set, providing early warning for distribution shift issues.
#'
#' @name Gate6Multiplicity
#' @keywords internal
NULL

Gate6Multiplicity = R6::R6Class(
  "Gate6Multiplicity",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G6",
        name = "Multiplicity and transport",
        pdr = "D->R"
      )
    },

    run = function(ctx) {
      task = ctx$task
      pred = ctx$pred

      cfg = ctx$multiplicity %||% list()
      .autoiml_assert_known_names(
        cfg,
        c("enabled", "max_alt_learners", "rashomon_rule", "importance_n", "importance_max_features", "require_transport_for_high_stakes"),
        "ctx$multiplicity"
      )
      enabled = isTRUE(cfg$enabled)
      benchmarkable_n = 0L
      group_col = task$col_roles$group %||% character()
      has_group_role = length(group_col) == 1L
      claim = .autoiml_as_list(ctx$claim)
      stakes = tolower(as.character(claim$stakes %||% "medium")[1L])
      purpose = as.character(claim$purpose %||% ctx$purpose %||% "exploratory")[1L]
      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))
      require_transport_for_high_stakes = isTRUE(cfg$require_transport_for_high_stakes %||% TRUE)

      # ----------------------------
      # 6a) Multiplicity (Rashomon)
      # ----------------------------
      perf_tbl = NULL
      rashomon_tbl = NULL
      imp_tbl = NULL
      rankcorr_tbl = NULL

      multiplicity_flag = NA
      primary_id = NA_character_
      rashomon_rule = cfg$rashomon_rule %||% "1se"
      rashomon_threshold = NA_real_

      if (enabled) {
        # Ensure we have an alternative learner set.
        if (is.null(ctx$alt_learners) || !is.list(ctx$alt_learners) || length(ctx$alt_learners) == 0L) {
          # (Usually populated by config defaults; keep a final fallback here.)
          max_n = cfg$max_alt_learners %||% 5L
          ctx$alt_learners = .autoiml_default_alt_learners(task, ctx$learner, max_n = max_n)
        }

        if (is.list(ctx$alt_learners) && length(ctx$alt_learners) >= 2L) {
          resampling = ctx$resampling$clone(deep = TRUE)
          resampling$instantiate(task)

          primary_id = ctx$primary_measure_id %||% private$.primary_measure_id(task)
          msr = mlr3::msr(primary_id)
          need_prob = inherits(task, "TaskClassif") && private$.needs_prob(primary_id)

          # Filter and prepare learners for benchmarking
          learners_to_benchmark = list()
          for (lr in ctx$alt_learners) {
            lr2 = lr$clone(deep = TRUE)

            # If measure needs probabilities, skip learners that cannot provide them.
            if (inherits(task, "TaskClassif") && private$.needs_prob(primary_id)) {
              if ("prob" %in% lr2$predict_types) {
                lr2$predict_type = "prob"
              } else {
                next
              }
            }

            learners_to_benchmark[[length(learners_to_benchmark) + 1L]] = lr2
          }
          benchmarkable_n = length(learners_to_benchmark)

          perf_tbl = NULL
          bmr = NULL
          if (length(learners_to_benchmark) >= 2L) {
            # Use mlr3::benchmark for efficient parallel evaluation
            # Store models to reuse for importance computation later
            design = mlr3::benchmark_grid(
              tasks = list(task),
              learners = learners_to_benchmark,
              resamplings = list(resampling)
            )
            bmr = mlr3::benchmark(design, store_models = TRUE)

            # Aggregate fold scores per learner
            scores_dt = bmr$score(list(msr))
            perf_tbl = scores_dt[, {
              v = get(primary_id)
              v = v[is.finite(v)]
              if (length(v) < 2L) {
                NULL
              } else {
                m = mean(v, na.rm = TRUE)
                s = stats::sd(v, na.rm = TRUE)
                se = s / sqrt(length(v))
                ci = 1.96 * se
                data.table::data.table(
                  # learner_id = learner_id[1L],
                  measure_id = primary_id,
                  n_folds = length(v),
                  mean = as.numeric(m),
                  sd = as.numeric(s),
                  se = as.numeric(se),
                  ci_low = as.numeric(m - ci),
                  ci_high = as.numeric(m + ci)
                )
              }
            }, by = learner_id] # [, !"learner_id"]
            perf_tbl = as.data.table(perf_tbl)
          }
          if (!is.null(perf_tbl) && nrow(perf_tbl) >= 2L) {
            minimize = isTRUE(msr$minimize)

            best_mean = if (minimize) min(perf_tbl$mean, na.rm = TRUE) else max(perf_tbl$mean, na.rm = TRUE)
            best_se = perf_tbl[mean == best_mean, min(se, na.rm = TRUE)]
            if (!is.finite(best_se)) best_se = stats::median(perf_tbl$se, na.rm = TRUE)

            # 1-SE Rashomon rule by default
            threshold = if (minimize) (best_mean + best_se) else (best_mean - best_se)
            rashomon_threshold = as.numeric(threshold)
            perf_tbl[, rashomon_threshold := threshold]

            perf_tbl[, in_rashomon := if (minimize) mean <= threshold else mean >= threshold]
            multiplicity_flag = sum(perf_tbl$in_rashomon, na.rm = TRUE) >= 2L

            rashomon_tbl = perf_tbl[in_rashomon == TRUE][order(if (minimize) mean else -mean)]
          }
        }
      }

      # If we have a Rashomon set with >=2 learners: compute explanation dispersion via PFI
      if (!is.null(rashomon_tbl) && nrow(rashomon_tbl) >= 2L && !is.null(bmr)) {
        importance_n = as.integer(cfg$importance_n %||% min(task$nrow, 800L))
        importance_max_features = as.integer(cfg$importance_max_features %||% 15L)

        rows = task$row_ids
        if (length(rows) > importance_n) {
          set.seed(.autoiml_gate_seed(ctx, self$id))
          rows = sample(rows, importance_n)
        }

        feats = private$.select_features_for_importance(task, max_features = importance_max_features)
        msr = mlr3::msr(primary_id)

        imp_list = list()
        learners_in = rashomon_tbl$learner_id

        # Extract trained learners from benchmark result
        # bmr$score() returns a data.table with a 'learner' column containing trained learners
        scores_dt = bmr$score()

        # Build lookup: for each learner_id, get one trained learner from any fold
        trained_learners = list()
        for (lid in learners_in) {
          # Get the first trained learner for this learner_id
          idx = which(scores_dt$learner_id == lid)[1L]
          if (!is.na(idx)) {
            trained_learners[[lid]] = scores_dt$learner[[idx]]
          }
        }

        for (lid in learners_in) {
          lr_trained = trained_learners[[lid]]
          if (is.null(lr_trained) || is.null(lr_trained$model)) {
            next
          }

          imp_dt = private$.perm_importance(task, lr_trained, rows = rows, features = feats, measure = msr)
          imp_dt[, learner_id := lid]
          imp_list[[lid]] = imp_dt
        }

        imp_tbl = data.table::rbindlist(imp_list, fill = TRUE)

        # Pairwise rank correlations of importances (Kendall)
        if (!is.null(imp_tbl) && nrow(imp_tbl) > 0L) {
          wide = data.table::dcast(imp_tbl, feature ~ learner_id, value.var = "importance")
          mat = as.matrix(wide[, -1, with = FALSE])
          colnames(mat) = names(wide)[-1]
          cor_list = list()
          lids = colnames(mat)

          for (i in seq_along(lids)) {
            for (j in seq_along(lids)) {
              if (j <= i) next
              xi = mat[, i]
              xj = mat[, j]
              ok = is.finite(xi) & is.finite(xj)
              if (sum(ok) < 3L) next
              cor_list[[length(cor_list) + 1L]] = data.table::data.table(
                learner_i = lids[i],
                learner_j = lids[j],
                kendall_tau = stats::cor(xi[ok], xj[ok], method = "kendall")
              )
            }
          }
          rankcorr_tbl = data.table::rbindlist(cor_list, fill = TRUE)
        }
      }

      explanation_multiplicity = data.table::data.table(
        n_models_in_rashomon = if (!is.null(rashomon_tbl)) nrow(rashomon_tbl) else 0L,
        n_features_compared = if (!is.null(imp_tbl)) data.table::uniqueN(imp_tbl$feature) else 0L,
        median_kendall_tau = if (!is.null(rankcorr_tbl) && nrow(rankcorr_tbl) > 0L) stats::median(rankcorr_tbl$kendall_tau, na.rm = TRUE) else NA_real_,
        min_kendall_tau = if (!is.null(rankcorr_tbl) && nrow(rankcorr_tbl) > 0L) min(rankcorr_tbl$kendall_tau, na.rm = TRUE) else NA_real_
      )

      # ----------------------------
      # 6b) Transportability via group role
      # ----------------------------
      transport_tbl = NULL
      transport_flag = NA

      if (length(group_col) == 1L && !is.null(pred)) {
        groups = task$data(cols = group_col)[[group_col]]

        if (inherits(pred, "PredictionClassif")) {
          if (!is.null(pred$prob)) {
            msr_ll = mlr3::msr("classif.logloss")
            dt = data.table::data.table(group = groups)
            transport_tbl = dt[, {
              idx = .I
              prob = as.data.frame(pred$prob[idx, , drop = FALSE])
              resp = colnames(prob)[max.col(as.matrix(prob), ties.method = "first")]
              p = mlr3::PredictionClassif$new(
                row_ids = seq_along(idx),
                truth = pred$truth[idx],
                response = factor(resp, levels = task$class_names),
                prob = prob
              )
              .(n = length(idx), logloss = msr_ll$score(p, task))
            }, by = group]

            rng = range(transport_tbl$logloss, na.rm = TRUE)
            transport_flag = is.finite(rng[2] - rng[1]) &&
              (rng[2] - rng[1]) > 0.10 * stats::median(transport_tbl$logloss, na.rm = TRUE)
          }
        } else if (inherits(pred, "PredictionRegr")) {
          y = pred$truth
          yhat = pred$response
          # Direct RMSE computation per group
          transport_tbl = data.table::data.table(group = groups, y = y, yhat = yhat)[
            , .(n = .N, rmse = sqrt(mean((y - yhat)^2))), by = group
          ]
          rng = range(transport_tbl$rmse, na.rm = TRUE)
          transport_flag = is.finite(rng[2] - rng[1]) &&
            (rng[2] - rng[1]) > 0.10 * stats::median(transport_tbl$rmse, na.rm = TRUE)
        }
      }

      # ----------------------------
      # Summarize / status
      # ----------------------------
      status = "pass"
      summary = "Multiplicity and transport checks did not raise major concerns (given available evidence)."
      multiplicity_assessed = !is.null(perf_tbl)
      transport_assessed = !is.null(transport_tbl)

      if (!enabled) {
        status = "skip"
        summary = "Multiplicity/transport disabled by configuration (ctx$multiplicity$enabled = FALSE)."
      } else if (!multiplicity_assessed && !transport_assessed) {
        status = "skip"
        reasons = character()
        if (benchmarkable_n < 2L) {
          reasons = c(reasons, "Multiplicity not run: need at least two benchmarkable alternative learners")
        }
        if (!has_group_role) {
          reasons = c(reasons, "Transport not run: Task group role is not set")
        } else if (is.null(pred)) {
          reasons = c(reasons, "Transport not run: missing prediction artifacts from Gate 1")
        }
        if (length(reasons) < 1L) {
          reasons = "Multiplicity/transport not assessed due to insufficient evaluable artifacts"
        }
        summary = paste(reasons, collapse = "; ")
      } else if (isTRUE(multiplicity_flag) || isTRUE(transport_flag)) {
        status = "warn"
        summary = "Evidence of multiplicity (Rashomon set contains multiple near-tie models) and/or limited transportability across groups; scope interpretive claims accordingly."
      } else if (!multiplicity_assessed || !transport_assessed) {
        assessed = character()
        not_assessed = character()
        if (multiplicity_assessed) assessed = c(assessed, "multiplicity") else not_assessed = c(not_assessed, "multiplicity")
        if (transport_assessed) assessed = c(assessed, "transport") else not_assessed = c(not_assessed, "transport")
        summary = paste0(
          "Partial Gate 6 coverage: assessed ", paste(assessed, collapse = " + "),
          "; not assessed ", paste(not_assessed, collapse = " + "),
          "."
        )
      }

      if (isTRUE(high_stakes) && isTRUE(require_transport_for_high_stakes) && !isTRUE(transport_assessed)) {
        status = "fail"
        summary = "High-stakes claims require transport assessment in Gate 6, but transport evidence is missing."
      }

      metrics = data.table::data.table(
        multiplicity_flag = multiplicity_flag,
        transport_flag = transport_flag,
        multiplicity_assessed = multiplicity_assessed,
        transport_assessed = transport_assessed,
        primary_measure_id = primary_id,
        rashomon_rule = rashomon_rule,
        high_stakes = high_stakes,
        require_transport_for_high_stakes = require_transport_for_high_stakes
      )

      rashomon_provenance = data.table::data.table(
        enabled = enabled,
        rashomon_rule = as.character(rashomon_rule),
        rashomon_threshold = rashomon_threshold,
        primary_measure_id = as.character(primary_id),
        benchmarkable_n = benchmarkable_n,
        n_alt_learners_declared = if (is.list(ctx$alt_learners)) length(ctx$alt_learners) else 0L,
        importance_n = as.integer(cfg$importance_n %||% min(task$nrow, 800L)),
        importance_max_features = as.integer(cfg$importance_max_features %||% 15L),
        seed = as.integer(ctx$seed %||% NA_integer_)
      )

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = metrics,
        artifacts = list(
          alt_learner_performance = perf_tbl,
          rashomon_set = rashomon_tbl,
          rashomon_importance = imp_tbl,
          rashomon_rankcorr = rankcorr_tbl,
          group_performance = transport_tbl,
          rashomon_provenance = rashomon_provenance,
          explanation_multiplicity = explanation_multiplicity
        )
      )
    }
  ),

  private = list(
    .primary_measure_id = function(task) {
      if (inherits(task, "TaskRegr")) {
        return("regr.rmse")
      }
      # Default to logloss for classification (works for multiclass)
      return("classif.logloss")
    },

    .needs_prob = function(measure_id) {
      measure_id %in% c("classif.logloss", "classif.auc", "classif.bbrier")
    },

    .select_features_for_importance = function(task, max_features = 15L) {
      max_features = as.integer(max_features)
      feats = task$feature_names
      if (length(feats) <= max_features) {
        return(feats)
      }

      ft = tryCatch(task$feature_types, error = function(e) NULL)
      num = if (!is.null(ft)) ft$id[ft$type %in% c("numeric", "integer")] else feats
      if (length(num) == 0L) {
        return(utils::head(feats, max_features))
      }

      X = task$data(cols = num)
      vars = vapply(num, function(f) stats::var(X[[f]], na.rm = TRUE), numeric(1))
      num[order(vars, decreasing = TRUE)][seq_len(min(max_features, length(num)))]
    },

    .score_prediction = function(task, measure, truth, pred_prob = NULL, pred_response = NULL) {
      if (inherits(task, "TaskRegr")) {
        p = mlr3::PredictionRegr$new(row_ids = seq_along(truth), truth = truth, response = pred_response)
        return(measure$score(p, task))
      }

      # Classification
      if (!is.null(pred_prob)) {
        prob = as.matrix(pred_prob)
        response = colnames(prob)[max.col(prob, ties.method = "first")]
        p = mlr3::PredictionClassif$new(
          row_ids = seq_along(truth),
          truth = truth,
          response = factor(response, levels = task$class_names),
          prob = prob
        )
        return(measure$score(p, task))
      }

      # Fallback: response-only measures
      p = mlr3::PredictionClassif$new(
        row_ids = seq_along(truth),
        truth = truth,
        response = pred_response
      )
      measure$score(p, task)
    },

    .perm_importance = function(task, learner, rows, features, measure) {
      # Evaluate on a fixed subset for comparability across learners
      X = data.table::as.data.table(task$data(rows = rows, cols = features))
      truth = task$truth(rows = rows)

      # Baseline
      p0 = learner$predict_newdata(X, task = task)
      base_score = if (inherits(task, "TaskRegr")) {
        private$.score_prediction(task, measure, truth, pred_response = p0$response)
      } else {
        private$.score_prediction(task, measure, truth, pred_prob = p0$prob)
      }

      out = vector("list", length(features))
      minimize = isTRUE(measure$minimize)

      for (j in seq_along(features)) {
        f = features[j]
        Xp = data.table::copy(X)
        idx = sample.int(nrow(Xp))
        data.table::set(Xp, j = f, value = Xp[[f]][idx])

        p1 = learner$predict_newdata(Xp, task = task)
        score1 = if (inherits(task, "TaskRegr")) {
          private$.score_prediction(task, measure, truth, pred_response = p1$response)
        } else {
          private$.score_prediction(task, measure, truth, pred_prob = p1$prob)
        }

        delta = if (minimize) (score1 - base_score) else (base_score - score1)

        out[[j]] = data.table::data.table(
          feature = f,
          importance = as.numeric(delta)
        )
      }

      data.table::rbindlist(out)
    }
  )
)
