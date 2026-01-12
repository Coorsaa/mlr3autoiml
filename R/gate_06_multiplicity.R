#' Gate 6: Multiplicity and transport
#'
#' Implements:
#' - Multiplicity: performance across alternative model classes and a conservative
#'   Rashomon set selection ("1-SE rule" by default).
#' - Transportability: subgroup performance heterogeneity if a Task `group` role is set.
#'
#' @keywords internal
#' @noRd
Gate6Multiplicity = R6::R6Class(
  "Gate6Multiplicity",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G6",
        name = "Multiplicity and transport",
        pdr = "D→R"
      )
    },

    run = function(ctx) {
      task = ctx$task
      pred = ctx$pred

      cfg = ctx$multiplicity %||% list()
      enabled = isTRUE(cfg$enabled)

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

          # Compute fold scores per learner
          out = list()
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

            rr = mlr3::resample(task, lr2, resampling, store_models = FALSE)
            sc = rr$score(list(msr))
            v = sc[[primary_id]]
            v = v[is.finite(v)]
            if (length(v) < 2L) next

            m = mean(v, na.rm = TRUE)
            s = stats::sd(v, na.rm = TRUE)
            se = s / sqrt(length(v))
            ci = 1.96 * se

            out[[lr$id]] = data.table::data.table(
              learner_id = lr$id,
              measure_id = primary_id,
              n_folds = length(v),
              mean = as.numeric(m),
              sd = as.numeric(s),
              se = as.numeric(se),
              ci_low = as.numeric(m - ci),
              ci_high = as.numeric(m + ci)
            )
          }

          perf_tbl = data.table::rbindlist(out, fill = TRUE)
          if (!is.null(perf_tbl) && nrow(perf_tbl) >= 2L) {
            minimize = isTRUE(msr$minimize)

            best_mean = if (minimize) min(perf_tbl$mean, na.rm = TRUE) else max(perf_tbl$mean, na.rm = TRUE)
            best_se = perf_tbl[mean == best_mean, min(se, na.rm = TRUE)]
            if (!is.finite(best_se)) best_se = stats::median(perf_tbl$se, na.rm = TRUE)

            # 1-SE Rashomon rule by default
            threshold = if (minimize) (best_mean + best_se) else (best_mean - best_se)
            perf_tbl[, rashomon_threshold := threshold]

            perf_tbl[, in_rashomon := if (minimize) mean <= threshold else mean >= threshold]
            multiplicity_flag = sum(perf_tbl$in_rashomon, na.rm = TRUE) >= 2L

            rashomon_tbl = perf_tbl[in_rashomon == TRUE][order(if (minimize) mean else -mean)]
          }
        }
      }

      # If we have a Rashomon set with >=2 learners: compute explanation dispersion via PFI
      if (!is.null(rashomon_tbl) && nrow(rashomon_tbl) >= 2L) {
        importance_n = as.integer(cfg$importance_n %||% min(task$nrow, 800L))
        importance_max_features = as.integer(cfg$importance_max_features %||% 15L)

        rows = task$row_ids
        if (length(rows) > importance_n) {
          set.seed(as.integer(ctx$seed %||% 1L))
          rows = sample(rows, importance_n)
        }

        feats = private$.select_features_for_importance(task, max_features = importance_max_features)
        msr = mlr3::msr(primary_id)

        imp_list = list()
        learners_in = rashomon_tbl$learner_id

        for (lid in learners_in) {
          lr = ctx$alt_learners[[which(vapply(ctx$alt_learners, function(x) x$id == lid, logical(1)))[1L]]]
          if (is.null(lr)) next
          lr2 = lr$clone(deep = TRUE)

          if (inherits(task, "TaskClassif") && private$.needs_prob(primary_id) && "prob" %in% lr2$predict_types) {
            lr2$predict_type = "prob"
          }

          lr2$train(task)
          imp_dt = private$.perm_importance(task, lr2, rows = rows, features = feats, measure = msr)
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

      # ----------------------------
      # 6b) Transportability via group role
      # ----------------------------
      transport_tbl = NULL
      transport_flag = NA

      group_col = task$col_roles$group %||% character()
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
          transport_tbl = data.table::data.table(group = groups, y = y, yhat = yhat)[
            , .(n = .N, rmse = .autoiml_rmse(y, yhat)), by = group
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

      if (!enabled) {
        status = "skip"
        summary = "Multiplicity/transport not assessed (ctx$multiplicity$enabled = FALSE)."
      } else if (is.null(perf_tbl) && is.null(transport_tbl)) {
        status = "skip"
        summary = "Multiplicity/transport not assessed (provide ctx$alt_learners and/or set a Task group role)."
      } else if (isTRUE(multiplicity_flag) || isTRUE(transport_flag)) {
        status = "warn"
        summary = "Evidence of multiplicity (Rashomon set contains multiple near-tie models) and/or limited transportability across groups; scope interpretive claims accordingly."
      }

      metrics = data.table::data.table(
        multiplicity_flag = multiplicity_flag,
        transport_flag = transport_flag,
        primary_measure_id = primary_id,
        rashomon_rule = rashomon_rule
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
          group_performance = transport_tbl
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
        prob = as.data.frame(pred_prob)
        response = colnames(prob)[max.col(as.matrix(prob), ties.method = "first")]
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
      p0 = learner$predict_newdata(X)
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

        p1 = learner$predict_newdata(Xp)
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
