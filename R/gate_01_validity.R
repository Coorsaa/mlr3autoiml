# FILE: R/gate_01_validity.R
#' Gate 1: Modeling and data validity (preflight)
#' @keywords internal
#' @noRd
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

      if (!is.null(ctx$seed)) set.seed(ctx$seed)

      # instantiate resampling (important for reproducibility)
      resampling$instantiate(task)

      # choose default measures
      measures = private$default_measures(task)
      rr = mlr3::resample(task, learner, resampling, store_models = TRUE, store_backends = TRUE)

      scores = rr$score(measures)
      # NOTE: In current mlr3 versions, `$aggregate()` returns a *named numeric()* (not a data.table).
      # Keep the vector for comparisons, but convert to a 1-row data.table for reporting.
      agg = rr$aggregate(measures)
      agg_dt = private$agg_to_dt(agg)
      pred = rr$prediction(predict_sets = "test")

      # baseline: featureless learner if available
      baseline_msg = NULL
      baseline_ok = NA
      baseline_agg = NULL
      baseline_agg_dt = NULL
      primary_metric = private$primary_metric(task)

      baseline_lr = NULL
      if (inherits(task, "TaskClassif")) {
        baseline_lr = mlr3::lrn("classif.featureless", predict_type = "prob")
      } else if (inherits(task, "TaskRegr")) {
        baseline_lr = mlr3::lrn("regr.featureless")
      }

      if (!is.null(baseline_lr)) {
        rr0 = mlr3::resample(task, baseline_lr, resampling, store_models = FALSE, store_backends = TRUE)
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
        artifacts = list(scores = scores, rr = rr),
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
