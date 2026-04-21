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
        c("enabled", "max_alt_learners", "rashomon_rule", "epsilon", "importance_n", "importance_max_features", "require_transport_for_high_stakes", "group_col", "transport_mode", "transport_measure_id"),
        "ctx$multiplicity"
      )
      enabled = isTRUE(cfg$enabled)
      benchmarkable_n = 0L
      group_col = as.character(cfg$group_col %||% (task$col_roles$group %||% character()))
      if (length(group_col) > 1L) group_col = group_col[1L]
      has_group_role = length(group_col) == 1L
      claim = .autoiml_as_list(ctx$claim)
      stakes = tolower(as.character(claim$stakes %||% "medium")[1L])
      purpose = as.character(claim$purpose %||% ctx$purpose %||% "exploratory")[1L]
      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))
      require_transport_for_high_stakes = isTRUE(cfg$require_transport_for_high_stakes %||% TRUE)
      transport_mode = as.character(cfg$transport_mode %||% "group_performance")[1L]
      if (!transport_mode %in% c("group_performance", "loco")) {
        stop("ctx$multiplicity$transport_mode must be one of {'group_performance','loco'}.", call. = FALSE)
      }
      transport_measure_id = as.character(cfg$transport_measure_id %||% private$.default_transport_measure_id(task))[1L]

      # ----------------------------
      # 6a) Multiplicity (Rashomon)
      # ----------------------------
      perf_tbl = NULL
      rashomon_tbl = NULL
      imp_tbl = NULL
      rankcorr_tbl = NULL
      predictive_multiplicity = NULL

      multiplicity_flag = NA
      primary_id = NA_character_
      rashomon_rule = cfg$rashomon_rule %||% "1se"
      rashomon_threshold = NA_real_
      rashomon_epsilon = suppressWarnings(as.numeric(cfg$epsilon %||% NA_real_))
      if (!rashomon_rule %in% c("1se", "epsilon")) {
        stop("ctx$multiplicity$rashomon_rule must be one of {'1se','epsilon'}.", call. = FALSE)
      }
      if (identical(rashomon_rule, "epsilon") && !is.finite(rashomon_epsilon)) {
        stop("ctx$multiplicity$epsilon must be a finite numeric value when rashomon_rule='epsilon'.", call. = FALSE)
      }

      benchmark_specs = list()

      if (enabled) {
        # Ensure we have an alternative learner set.
        if (is.null(ctx$alt_learners) || !is.list(ctx$alt_learners) || length(ctx$alt_learners) == 0L) {
          max_n = cfg$max_alt_learners %||% 5L
          ctx$alt_learners = .autoiml_default_alt_learners(task, ctx$learner, max_n = max_n)
        }

        benchmark_specs = private$.benchmark_learners(task, ctx$learner, ctx$alt_learners, primary_measure_id = ctx$primary_measure_id)
        benchmarkable_n = length(benchmark_specs)

        if (length(benchmark_specs) >= 2L) {
          resampling = private$.benchmark_resampling(ctx, task)

          primary_id = ctx$primary_measure_id %||% private$.primary_measure_id(task)
          msr = mlr3::msr(primary_id)

          perf_tbl = NULL
          bmr = NULL
          if (length(benchmark_specs) >= 2L) {
            design = mlr3::benchmark_grid(
              tasks = list(task),
              learners = lapply(benchmark_specs, `[[`, "learner"),
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

            threshold = if (identical(rashomon_rule, "epsilon")) {
              if (minimize) (best_mean + rashomon_epsilon) else (best_mean - rashomon_epsilon)
            } else {
              if (minimize) (best_mean + best_se) else (best_mean - best_se)
            }
            rashomon_threshold = as.numeric(threshold)
            perf_tbl[, rashomon_threshold := threshold]
            perf_tbl[, rashomon_rule := rashomon_rule]
            perf_tbl[, epsilon := if (is.finite(rashomon_epsilon)) rashomon_epsilon else NA_real_]

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

      if (!is.null(rashomon_tbl) && nrow(rashomon_tbl) >= 2L) {
        predictive_multiplicity = private$.predictive_multiplicity(
          task = task,
          rashomon_ids = rashomon_tbl$learner_id,
          benchmark_specs = benchmark_specs
        )
      }

      explanation_multiplicity = data.table::data.table(
        n_models_in_rashomon = if (!is.null(rashomon_tbl)) nrow(rashomon_tbl) else 0L,
        n_features_compared = if (!is.null(imp_tbl)) data.table::uniqueN(imp_tbl$feature) else 0L,
        median_kendall_tau = if (!is.null(rankcorr_tbl) && nrow(rankcorr_tbl) > 0L) stats::median(rankcorr_tbl$kendall_tau, na.rm = TRUE) else NA_real_,
        min_kendall_tau = if (!is.null(rankcorr_tbl) && nrow(rankcorr_tbl) > 0L) min(rankcorr_tbl$kendall_tau, na.rm = TRUE) else NA_real_,
        predictive_p95_range = if (!is.null(predictive_multiplicity) && nrow(predictive_multiplicity) > 0L) predictive_multiplicity$p95_range[[1L]] else NA_real_
      )

      # ----------------------------
      # 6b) Transportability via group role
      # ----------------------------
      transport_tbl = NULL
      transport_flag = NA
      shift_assessment = NULL

      if (length(group_col) == 1L && nzchar(group_col)) {
        if (identical(transport_mode, "loco")) {
          shift_assessment = private$.transport_loco(
            ctx = ctx,
            task = task,
            benchmark_specs = benchmark_specs,
            group_col = group_col,
            measure_id = transport_measure_id
          )
          transport_tbl = shift_assessment$transport
        } else if (!is.null(pred)) {
          shift_assessment = private$.transport_group_performance(
            task = task,
            pred = pred,
            group_col = group_col,
            measure_id = transport_measure_id
          )
          transport_tbl = shift_assessment$transport
        }
      }

      if (!is.null(transport_tbl) && nrow(transport_tbl) > 0L) {
        score_col = intersect(c("score", "logloss", "rmse", "auc"), names(transport_tbl))[1L]
        vals = transport_tbl[[score_col]]
        vals = vals[is.finite(vals)]
        if (length(vals) > 1L) {
          rng = range(vals, na.rm = TRUE)
          transport_flag = is.finite(rng[2] - rng[1]) &&
            (rng[2] - rng[1]) > 0.10 * stats::median(abs(vals), na.rm = TRUE)
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
        transport_mode = transport_mode,
        transport_measure_id = transport_measure_id,
        rashomon_rule = rashomon_rule,
        high_stakes = high_stakes,
        require_transport_for_high_stakes = require_transport_for_high_stakes
      )

      rashomon_provenance = data.table::data.table(
        enabled = enabled,
        rashomon_rule = as.character(rashomon_rule),
        epsilon = if (is.finite(rashomon_epsilon)) rashomon_epsilon else NA_real_,
        rashomon_threshold = rashomon_threshold,
        primary_measure_id = as.character(primary_id),
        transport_mode = transport_mode,
        transport_measure_id = transport_measure_id,
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
          predictive_multiplicity = predictive_multiplicity,
          group_performance = transport_tbl,
          shift_assessment = shift_assessment,
          rashomon_provenance = rashomon_provenance,
          explanation_multiplicity = explanation_multiplicity
        )
      )
    }
  ),

  private = list(
    .benchmark_resampling = function(ctx, task) {
      if (!is.null(ctx$rr) && !is.null(ctx$rr$resampling)) {
        return(ctx$rr$resampling$clone(deep = TRUE))
      }
      resampling = ctx$resampling$clone(deep = TRUE)
      has_instance = tryCatch(as.integer(resampling$iters) > 0L, error = function(e) FALSE)
      if (!isTRUE(has_instance)) {
        resampling$instantiate(task)
      }
      resampling
    },

    .benchmark_learners = function(task, primary_learner, alt_learners, primary_measure_id = NULL) {
      measure_id = as.character(primary_measure_id %||% private$.primary_measure_id(task))[1L]
      learners_raw = c(list(primary_learner), unname(alt_learners %||% list()))
      out = list()
      seen_ids = character()

      for (lr in learners_raw) {
        if (is.null(lr)) next
        lr2 = lr$clone(deep = TRUE)
        if (inherits(task, "TaskClassif") && private$.needs_prob(measure_id)) {
          if ("prob" %in% lr2$predict_types) {
            lr2$predict_type = "prob"
          } else {
            next
          }
        }

        lid = as.character(lr2$id %||% class(lr2)[1L])
        if (!nzchar(lid) || lid %in% seen_ids) next
        seen_ids = c(seen_ids, lid)
        out[[length(out) + 1L]] = list(id = lid, learner = lr2)
      }

      names(out) = vapply(out, `[[`, character(1L), "id")
      out
    },

    .primary_measure_id = function(task) {
      if (inherits(task, "TaskRegr")) {
        return("regr.rmse")
      }
      # Default to logloss for classification (works for multiclass)
      return("classif.logloss")
    },

    .default_transport_measure_id = function(task) {
      if (inherits(task, "TaskRegr")) {
        return("regr.rsq")
      }
      if (inherits(task, "TaskClassif") && length(task$class_names) == 2L) {
        return("classif.auc")
      }
      "classif.logloss"
    },

    .needs_prob = function(measure_id) {
      measure_id %in% c("classif.logloss", "classif.auc", "classif.bbrier")
    },

    .predictive_multiplicity = function(task, rashomon_ids, benchmark_specs) {
      specs = benchmark_specs[rashomon_ids]
      specs = Filter(Negate(is.null), specs)
      if (length(specs) < 2L) {
        return(NULL)
      }

      pred_mat = vapply(specs, function(spec) {
        lr = spec$learner$clone(deep = TRUE)
        lr$train(task)
        .autoiml_predict_score(lr, task$data(rows = task$row_ids, cols = task$feature_names), task = task)
      }, numeric(task$nrow))

      if (!is.matrix(pred_mat) || ncol(pred_mat) < 2L) {
        return(NULL)
      }

      row_range = apply(pred_mat, 1L, function(x) diff(range(x, na.rm = TRUE)))
      data.table::data.table(
        n_models = ncol(pred_mat),
        mean_range = mean(row_range, na.rm = TRUE),
        median_range = stats::median(row_range, na.rm = TRUE),
        p95_range = stats::quantile(row_range, probs = 0.95, na.rm = TRUE, names = FALSE),
        max_range = max(row_range, na.rm = TRUE)
      )
    },

    .transport_group_performance = function(task, pred, group_col, measure_id) {
      groups = task$data(cols = group_col)[[group_col]]
      if (is.null(groups)) {
        return(NULL)
      }

      if (inherits(pred, "PredictionClassif")) {
        if (is.null(pred$prob)) {
          return(NULL)
        }
        msr = mlr3::msr(measure_id)
        prob_mat = .autoiml_pred_prob_matrix(pred, task = task)
        row_ids = pred$row_ids %||% task$row_ids
        dt = data.table::data.table(group = groups)
        transport_tbl = dt[, {
          idx = .I
          prob = prob_mat[idx, , drop = FALSE]
          resp = colnames(prob)[max.col(prob, ties.method = "first")]
          p = mlr3::PredictionClassif$new(
            row_ids = row_ids[idx],
            truth = pred$truth[idx],
            response = factor(resp, levels = task$class_names),
            prob = prob
          )
          .(n = length(idx), score = msr$score(p, task))
        }, by = group]
      } else if (inherits(pred, "PredictionRegr")) {
        msr = mlr3::msr(measure_id)
        transport_tbl = data.table::data.table(group = groups, truth = pred$truth, response = pred$response)[
          , {
            p = mlr3::PredictionRegr$new(row_ids = seq_len(.N), truth = truth, response = response)
            .(n = .N, score = msr$score(p, task))
          },
          by = group
        ]
      } else {
        return(NULL)
      }

      list(
        mode = "group_performance",
        group_col = group_col,
        measure_id = measure_id,
        transport = transport_tbl
      )
    },

    .transport_loco = function(ctx, task, benchmark_specs, group_col, measure_id) {
      groups = task$data(cols = group_col)[[group_col]]
      if (is.null(groups)) {
        return(NULL)
      }

      group_values = unique(groups)
      group_values = group_values[!is.na(group_values)]
      if (length(group_values) < 2L) {
        return(NULL)
      }

      if (length(benchmark_specs) < 1L) {
        benchmark_specs = private$.benchmark_learners(task, ctx$learner, ctx$alt_learners, primary_measure_id = ctx$primary_measure_id)
      }
      if (length(benchmark_specs) < 1L) {
        return(NULL)
      }

      primary_id = ctx$learner$id %||% names(benchmark_specs)[1L]
      learner_spec = benchmark_specs[[primary_id]] %||% benchmark_specs[[1L]]
      msr = mlr3::msr(measure_id)

      rows_by_group = split(task$row_ids, groups)
      out = vector("list", length(rows_by_group))
      names(out) = names(rows_by_group)

      for (gid in names(rows_by_group)) {
        test_rows = rows_by_group[[gid]]
        train_rows = setdiff(task$row_ids, test_rows)
        if (length(train_rows) < 2L || length(test_rows) < 1L) next

        lr = learner_spec$learner$clone(deep = TRUE)
        train_task = task$clone(deep = TRUE)$filter(train_rows)
        test_task = task$clone(deep = TRUE)$filter(test_rows)
        lr$train(train_task)
        pred = lr$predict(test_task)
        score = msr$score(pred, test_task)

        out[[gid]] = data.table::data.table(
          group = gid,
          n_train = length(train_rows),
          n_test = length(test_rows),
          learner_id = learner_spec$id,
          score = as.numeric(score)
        )
      }

      transport_tbl = data.table::rbindlist(out, fill = TRUE)
      if (nrow(transport_tbl) < 1L) {
        return(NULL)
      }

      list(
        mode = "loco",
        group_col = group_col,
        measure_id = measure_id,
        learner_id = learner_spec$id,
        transport = transport_tbl
      )
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
      X = data.table::as.data.table(task$data(rows = rows, cols = task$feature_names))
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
