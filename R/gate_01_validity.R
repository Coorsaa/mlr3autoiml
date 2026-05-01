# FILE: R/gate_01_validity.R

#' @title Gate 1: Modeling and Data Validity (Preflight)
#'
#' @description
#' Establishes predictive adequacy before explanation. This gate:
#' \itemize{
#'   \item Validates learner configuration (requires probabilistic predictions for classification)
#'   \item Runs honest resampling for performance estimation
#'   \item Compares against a featureless baseline
#'   \item Trains the final model on full data for downstream gates
#' }
#'
#' For multiclass tasks with learners lacking native multiclass support,
#' automatically wraps with one-vs-rest (OVR) if mlr3pipelines is available.
#'
#' @name Gate1Validity
#' @keywords internal
NULL

Gate1Validity = R6::R6Class(
  "Gate1Validity",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G1",
        name = "Modeling and data validity (preflight)",
        pdr = "P"
      )
    },

    run = function(ctx) {
      task = ctx$task
      learner = ctx$learner$clone(deep = TRUE)
      resampling = ctx$resampling$clone(deep = TRUE)
      validation_cfg = .autoiml_as_list(ctx$validation)
      .autoiml_assert_known_names(validation_cfg, c("split_policy", "cluster_var", "time_var", "site_var"), "ctx$validation")

      preflight_msgs = character()

      # configure predict_type if needed
      if (inherits(task, "TaskClassif")) {
        if ("prob" %in% learner$predict_types) {
          learner$predict_type = "prob"
        } else {
          return(GateResult$new(
            gate_id = self$id, gate_name = self$name, pdr = self$pdr,
            status = "fail",
            summary = "Learner does not support probabilistic predictions required for calibration/utility checks.",
            metrics = NULL,
            messages = sprintf("Supported predict_types: %s", paste(learner$predict_types, collapse = ", "))
          ))
        }

        # If the task is multiclass but the learner lacks multiclass support, try to wrap it in OVR.
        if (length(task$class_names) > 2L && !("multiclass" %in% learner$properties)) {
          if (.autoiml_require_pkg("mlr3pipelines")) {
            base_id = learner$id
            g = mlr3pipelines::pipeline_ovr(learner)
            learner = mlr3pipelines::GraphLearner$new(g, id = paste0("ovr.", base_id), predict_type = "prob")
            preflight_msgs = c(preflight_msgs, "Learner wrapped with mlr3pipelines::pipeline_ovr() for multiclass support (one-vs-rest).")

            # Persist the wrapped learner in the shared context for downstream gates
            ctx$learner = learner
          } else {
            return(GateResult$new(
              gate_id = self$id, gate_name = self$name, pdr = self$pdr,
              status = "fail",
              summary = "Task is multiclass but learner lacks multiclass support. Install 'mlr3pipelines' for automatic one-vs-rest wrapping, or supply a multiclass-capable learner.",
              metrics = NULL,
              messages = c("Missing optional dependency: mlr3pipelines.")
            ))
          }
        }
      }

      if (!is.null(ctx$seed)) set.seed(.autoiml_gate_seed(ctx, self$id))

      # instantiate resampling only when no fixed split instance was supplied
      has_instance = tryCatch(as.integer(resampling$iters) > 0L, error = function(e) FALSE)
      if (!isTRUE(has_instance)) {
        resampling$instantiate(task)
      }

      # choose default measures
      measures = private$default_measures(task)
      rr = mlr3::resample(task, learner, resampling, store_models = TRUE, store_backends = FALSE)

      scores = rr$score(measures)
      # NOTE: In current mlr3 versions, `$aggregate()` returns a *named numeric()* (not a data.table).
      # Keep the vector for comparisons, but convert to a 1-row data.table for reporting.
      agg = rr$aggregate(measures)
      agg_dt = private$agg_to_dt(agg)
      pred = rr$prediction(predict_sets = "test")

      # ---- Plausible-values pooling (Rubin's rules), v0.0.5 ----
      # If the user passes additional plausible-value targets via
      #   ctx$plausible_values$pv_tasks  : list of mlr3 TaskRegr (one per extra PV)
      # we train one extra resample per PV and pool per-fold metrics across PVs
      # using Rubin's rules on the same instantiated resampling splits:
      #   T = W + (1 + 1/m) * B
      pv_pool = NULL
      pv_cfg = .autoiml_as_list(ctx$plausible_values)
      .autoiml_assert_known_names(pv_cfg, c("pv_tasks"), "ctx$plausible_values")
      pv_tasks = pv_cfg$pv_tasks %??% list()
      if (inherits(pv_tasks, "Task")) pv_tasks = list(pv_tasks)
      if (!is.list(pv_tasks)) {
        stop("ctx$plausible_values$pv_tasks must be NULL, a TaskRegr, or a list of TaskRegr objects.", call. = FALSE)
      }
      if (length(pv_tasks) > 0L && !inherits(task, "TaskRegr")) {
        stop("ctx$plausible_values$pv_tasks is only supported for regression tasks.", call. = FALSE)
      }
      if (length(pv_tasks) > 0L) {
        base_row_ids = task$row_ids
        base_features = task$feature_names
        per_pv_scores = vector("list", length = length(pv_tasks) + 1L)
        per_pv_scores[[1L]] = scores
        for (k in seq_along(pv_tasks)) {
          tk = pv_tasks[[k]]
          if (!inherits(tk, "TaskRegr")) {
            stop(sprintf("ctx$plausible_values$pv_tasks[[%d]] must inherit from TaskRegr.", k), call. = FALSE)
          }
          if (!identical(tk$row_ids, base_row_ids)) {
            stop(sprintf("ctx$plausible_values$pv_tasks[[%d]] must use the same row_ids as ctx$task so Gate 1 can reuse the instantiated resampling splits.", k), call. = FALSE)
          }
          if (!identical(tk$feature_names, base_features)) {
            stop(sprintf("ctx$plausible_values$pv_tasks[[%d]] must use the same feature set as ctx$task.", k), call. = FALSE)
          }

          # Reuse the exact same instantiated folds as the primary task so
          # between-PV variance reflects outcome uncertainty rather than new split noise.
          rsk = resampling$clone(deep = TRUE)
          rrk = mlr3::resample(tk, learner, rsk, store_models = FALSE, store_backends = FALSE)
          per_pv_scores[[k + 1L]] = rrk$score(measures)
        }
        pool_rows = vector("list", length = length(measures))
        names(pool_rows) = vapply(measures, function(m) m$id, character(1L))
        for (mid in vapply(measures, function(m) m$id, character(1L))) {
          pv_fold_means = vapply(per_pv_scores, function(s) {
            v = as.numeric(s[[mid]])
            v = v[is.finite(v)]
            if (length(v) < 1L) {
              return(NA_real_)
            }
            mean(v)
          }, numeric(1L))
          pv_fold_within_var = vapply(per_pv_scores, function(s) {
            v = as.numeric(s[[mid]])
            v = v[is.finite(v)]
            if (length(v) < 2L) {
              return(NA_real_)
            }
            stats::var(v) / length(v)
          }, numeric(1L))

          pv_fold_means = pv_fold_means[is.finite(pv_fold_means)]
          pv_fold_within_var = pv_fold_within_var[is.finite(pv_fold_within_var)]

          m_pv = length(pv_fold_means)
          mean_val = if (m_pv > 0L) mean(pv_fold_means) else NA_real_
          within_var = if (length(pv_fold_within_var) > 0L) mean(pv_fold_within_var) else NA_real_
          between_var = if (m_pv > 1L) stats::var(pv_fold_means) else 0
          total_var = if (is.finite(within_var)) within_var + (1 + 1 / max(m_pv, 1L)) * between_var else NA_real_
          se_pooled = if (is.finite(total_var) && total_var >= 0) sqrt(total_var) else NA_real_
          df_pooled = private$rubin_df(within_var = within_var, between_var = between_var, m = m_pv)
          tcrit = if (is.finite(df_pooled)) stats::qt(0.975, df = df_pooled) else stats::qnorm(0.975)
          ci_delta = if (is.finite(se_pooled) && is.finite(tcrit)) tcrit * se_pooled else NA_real_

          pool_rows[[mid]] = data.table::data.table(
            measure_id = mid,
            n_pv = m_pv,
            pooled_mean = mean_val,
            pooled_se = se_pooled,
            within_var = within_var,
            between_var = as.numeric(between_var),
            total_var = total_var,
            df = df_pooled,
            ci_low = if (is.finite(ci_delta)) mean_val - ci_delta else NA_real_,
            ci_high = if (is.finite(ci_delta)) mean_val + ci_delta else NA_real_
          )
        }
        pv_pool = data.table::rbindlist(pool_rows, fill = TRUE)
      }
      # ---- end PV pooling ----

      measure_ids = vapply(measures, function(m) m$id, character(1L))
      metric_cols = intersect(measure_ids, names(scores))
      uncertainty = if (length(metric_cols) > 0L) {
        data.table::rbindlist(lapply(metric_cols, function(mid) {
          vals = as.numeric(scores[[mid]])
          vals = vals[is.finite(vals)]
          if (length(vals) < 1L) {
            return(data.table::data.table(
              measure_id = mid,
              n = 0L,
              mean = NA_real_,
              sd = NA_real_,
              se = NA_real_,
              ci_low = NA_real_,
              ci_high = NA_real_
            ))
          }
          n = length(vals)
          m = mean(vals)
          s = if (n > 1L) stats::sd(vals) else NA_real_
          se = if (n > 1L) s / sqrt(n) else NA_real_
          tcrit = if (n > 1L) stats::qt(0.975, df = n - 1L) else NA_real_
          ci = if (n > 1L && is.finite(se) && is.finite(tcrit)) tcrit * se else NA_real_
          data.table::data.table(
            measure_id = mid,
            n = n,
            mean = m,
            sd = s,
            se = se,
            ci_low = if (is.finite(ci)) m - ci else NA_real_,
            ci_high = if (is.finite(ci)) m + ci else NA_real_
          )
        }), fill = TRUE)
      } else {
        data.table::data.table()
      }

      split_policy = as.character(validation_cfg$split_policy %??% "resampling")
      leakage_checklist = data.table::data.table(
        split_policy = split_policy,
        has_cluster_var = isTRUE(!is.null(validation_cfg$cluster_var) && nzchar(as.character(validation_cfg$cluster_var)[1L])),
        has_time_var = isTRUE(!is.null(validation_cfg$time_var) && nzchar(as.character(validation_cfg$time_var)[1L])),
        has_site_var = isTRUE(!is.null(validation_cfg$site_var) && nzchar(as.character(validation_cfg$site_var)[1L])),
        preprocessing_nested = TRUE,
        oof_predictions_available = !is.null(pred)
      )

      # baseline: featureless learner if available
      baseline_msg = NULL
      baseline_ok = NA
      baseline_agg = NULL
      baseline_agg_dt = NULL
      primary_metric = private$primary_metric(task)

      baseline_lr = NULL
      baseline_scores_dt = NULL
      if (inherits(task, "TaskClassif")) {
        baseline_lr = mlr3::lrn("classif.featureless", predict_type = "prob")
      } else if (inherits(task, "TaskRegr")) {
        baseline_lr = mlr3::lrn("regr.featureless")
      }

      if (!is.null(baseline_lr)) {
        rr0 = mlr3::resample(task, baseline_lr, resampling, store_models = FALSE, store_backends = TRUE)
        baseline_scores_dt = data.table::as.data.table(rr0$score(measures))
        baseline_agg = rr0$aggregate(measures)
        baseline_agg_dt = private$agg_to_dt(baseline_agg)

        # Compare on primary metric
        baseline_ok = private$compare_primary(agg, baseline_agg, primary_metric, task)
        if (is.na(baseline_ok)) {
          baseline_msg = sprintf("Featureless baseline comparison on %s could not be computed (missing/undefined metric values).", primary_metric)
        } else {
          baseline_msg = sprintf("Compared against featureless baseline on %s.", primary_metric)
        }
      }

      # Fit final model on full task for downstream gates
      final_model = learner$clone(deep = TRUE)
      final_model$train(task)

      # store in context for downstream gates
      ctx$rr = rr
      ctx$pred = pred
      ctx$measures = measures
      ctx$primary_measure_id = primary_metric
      ctx$score_dt = scores
      ctx$agg = agg
      ctx$agg_dt = agg_dt
      ctx$baseline_agg = baseline_agg
      ctx$baseline_agg_dt = baseline_agg_dt
      ctx$final_model = final_model

      # heuristic pass/fail
      status = "pass"
      summ = "Predictive adequacy established with honest resampling."

      if (isFALSE(baseline_ok)) {
        status = "warn"
        summ = "Model only weakly improves over featureless baseline; interpretation claims should be conservative."
      }

      # if performance is missing, warn
      if (length(agg) == 0L) {
        status = "fail"
        summ = "No performance results available; cannot establish predictive adequacy."
      }

      metrics = data.table::copy(agg_dt)
      if (!is.null(baseline_agg_dt)) {
        bdt = data.table::copy(baseline_agg_dt)
        data.table::setnames(bdt, old = names(bdt), new = paste0(names(bdt), "_baseline"))
        metrics = cbind(metrics, bdt)
      }

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summ,
        metrics = metrics,
        artifacts = list(
          scores = scores,
          baseline_scores = baseline_scores_dt,
          rr = rr,
          uncertainty = uncertainty,
          leakage_checklist = leakage_checklist,
          validation_design = data.table::data.table(
            resampling_id = resampling$id,
            split_policy = split_policy,
            n_splits = rr$iters
          ),
          pv_pool = pv_pool # NULL unless ctx$plausible_values$pv_tasks supplied
        ),
        messages = c(baseline_msg, preflight_msgs)
      )
    }
  ),
  private = list(
    agg_to_dt = function(x) {
      # Convert an mlr3 aggregate() result to a 1-row data.table for printing/reporting.
      # In mlr3 >= 1.0, ResampleResult$aggregate() returns a named numeric().
      if (is.null(x)) {
        return(NULL)
      }

      if (is.numeric(x) && !is.null(names(x))) {
        return(data.table::as.data.table(as.list(x)))
      }

      if (is.data.frame(x)) {
        return(data.table::as.data.table(x))
      }

      # fallback
      tryCatch(data.table::as.data.table(x), error = function(e) NULL)
    },

    rubin_df = function(within_var, between_var, m) {
      if (!is.numeric(m) || length(m) != 1L || m < 1L) {
        return(NA_real_)
      }
      if (!is.finite(between_var) || between_var <= 0 || m <= 1L) {
        return(Inf)
      }
      if (is.finite(within_var) && within_var > 0) {
        rel_increase = ((1 + 1 / m) * between_var) / within_var
        if (is.finite(rel_increase) && rel_increase > 0) {
          return((m - 1) * (1 + 1 / rel_increase)^2)
        }
        return(Inf)
      }
      m - 1
    },

    default_measures = function(task) {
      if (inherits(task, "TaskClassif")) {
        # Use AUC when binary; otherwise use logloss + classification error
        nclass = length(task$class_names)
        ms = list(mlr3::msr("classif.ce"))
        if (nclass == 2L) {
          # In current mlr3, brier score measures are split into
          #   * classif.bbrier (binary)
          #   * classif.mbrier (multiclass)
          ms = c(ms, list(mlr3::msr("classif.auc"), mlr3::msr("classif.bbrier"), mlr3::msr("classif.logloss")))
        } else {
          ms = c(ms, list(mlr3::msr("classif.logloss")))
        }
        return(ms)
      }
      list(mlr3::msr("regr.rmse"), mlr3::msr("regr.rsq"))
    },

    primary_metric = function(task) {
      if (inherits(task, "TaskClassif")) {
        if (length(task$class_names) == 2L) {
          return("classif.auc")
        }
        return("classif.logloss")
      }
      "regr.rmse"
    },

    compare_primary = function(agg, baseline_agg, metric_id, task) {
      # returns TRUE if meaningfully better than baseline, FALSE if not, NA if cannot compare
      if (is.null(agg) || is.null(baseline_agg)) {
        return(NA)
      }

      # aggregate() returns named numeric() in mlr3 (see ResampleResult docs).
      if (!is.numeric(agg) || is.null(names(agg)) || length(agg) == 0L) {
        return(NA)
      }
      if (!is.numeric(baseline_agg) || is.null(names(baseline_agg)) || length(baseline_agg) == 0L) {
        return(NA)
      }

      if (!metric_id %in% names(agg) || !metric_id %in% names(baseline_agg)) {
        return(NA)
      }

      m = unname(agg[[metric_id]])
      b = unname(baseline_agg[[metric_id]])

      if (length(m) != 1L || length(b) != 1L) {
        return(NA)
      }
      if (!is.finite(m) || !is.finite(b)) {
        return(NA)
      }

      # direction: lower is better for losses; higher for AUC/RSQ
      minimize = metric_id %in% c("regr.rmse", "classif.logloss", "classif.ce", "classif.bbrier", "classif.mbrier")
      if (minimize) {
        return(isTRUE(m < b * 0.99)) # 1% improvement heuristic
      } else {
        return(isTRUE(m > b + 0.01)) # +0.01 absolute for AUC/RSQ heuristic
      }
    }
  )
)
