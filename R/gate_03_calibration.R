# FILE: R/gate_03_calibration.R
#' Gate 3: Calibration and decision utility
#'
#' Implements calibration and decision-utility diagnostics.
#'
#' - Binary classification: logistic calibration intercept/slope, ECE, reliability curve,
#'   and decision curve analysis (net benefit).
#' - Multiclass classification: one-vs-rest calibration/utility checks per class
#'   (intercept/slope, ECE, reliability, OVR net benefit) plus overall logloss and
#'   multiclass Brier score.
#'
#' @keywords internal
#' @noRd
Gate3Calibration = R6::R6Class(
  "Gate3Calibration",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G3",
        name = "Calibration and decision utility",
        pdr = "R"
      )
    },

    run = function(ctx) {
      task = ctx$task
      purpose = ctx$purpose %||% "exploratory"
      cfg = ctx$calibration %||% list()

      thresholds = cfg$thresholds %||% seq(0.01, 0.99, by = 0.01)
      bins = as.integer(cfg$bins %||% 10L)

      pred = ctx$pred
      in_sample = FALSE

      # fallback if Gate 1 didn't store oof predictions
      if (is.null(pred)) {
        model = ctx$final_model
        if (is.null(model)) {
          model = ctx$learner$clone(deep = TRUE)
          model$train(task)
          ctx$final_model = model
        }
        pred = model$predict(task)
        in_sample = TRUE
      }

      # --------------------------------------------------------------------
      # Regression
      if (inherits(task, "TaskRegr")) {
        y = pred$truth
        yhat = pred$response
        rmse = .autoiml_rmse(y, yhat)

        status = if (isTRUE(in_sample)) "warn" else "pass"
        summary = if (isTRUE(in_sample)) {
          "Regression: basic error metrics computed on in-sample predictions (no OOF predictions available)."
        } else {
          "Regression: basic error metrics computed (calibration/utility mainly relevant for probabilistic forecasts)."
        }

        metrics = data.table::data.table(
          rmse = rmse,
          n = length(y),
          in_sample = in_sample
        )

        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = status,
          summary = summary,
          metrics = metrics
        ))
      }

      # --------------------------------------------------------------------
      # Classification
      if (!inherits(task, "TaskClassif") || !inherits(pred, "PredictionClassif")) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = "skip",
          summary = "Calibration/utility checks are only implemented for classification and regression tasks.",
          metrics = NULL
        ))
      }

      nclass = length(task$class_names)

      # --------------------------------------------------------------------
      # Binary classification
      if (nclass == 2L) {
        pos = task$positive %||% task$class_names[2L]
        truth01 = as.integer(pred$truth == pos)
        p_hat = as.numeric(pred$prob[[pos]])

        cal = .autoiml_calibration_glm(truth01, p_hat)
        ece = .autoiml_ece_binary(truth01, p_hat, bins = bins)
        brier = .autoiml_brier(truth01, p_hat)
        ll = .autoiml_logloss(truth01, p_hat)
        auc = .autoiml_auc(truth01, p_hat)

        rel = .autoiml_reliability_curve_binary(truth01, p_hat, bins = bins)
        dca = .autoiml_dca(truth01, p_hat, thresholds = thresholds)
        prev = mean(truth01)
        dca[, nb_treat_all := prev - (1 - prev) * threshold / (1 - threshold)]
        dca[, nb_treat_none := 0]

        # Heuristic gate decision
        status = "pass"
        if (isTRUE(in_sample)) status <- "warn"

        # calibration concern heuristics
        if (is.finite(ece) && ece > 0.10) status <- "warn"
        if (is.finite(cal$slope) && (cal$slope < 0.8 || cal$slope > 1.2)) status <- "warn"

        summary = "Binary calibration/utility checks computed (intercept/slope, ECE, reliability, net benefit)."
        if (isTRUE(in_sample)) {
          summary = paste0(summary, " Note: predictions are in-sample (no OOF predictions available).")
        }

        metrics = data.table::data.table(
          task_type = "classif",
          nclass = nclass,
          positive = pos,
          auc = auc,
          brier = brier,
          logloss = ll,
          ece = ece,
          cal_intercept = cal$intercept,
          cal_slope = cal$slope,
          in_sample = in_sample
        )

        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = status,
          summary = summary,
          metrics = metrics,
          artifacts = list(
            reliability = rel,
            dca = dca
          ),
          messages = c(
            "If using a fixed decision threshold, report threshold selection rationale and validate net benefit with decision-analytic assumptions."
          )
        ))
      }

      # --------------------------------------------------------------------
      # Multiclass classification: one-vs-rest checks
      prob = .autoiml_pred_prob_matrix(pred, task)
      truth = pred$truth

      overall_logloss = .autoiml_logloss_multiclass(truth, prob)
      overall_mbrier = .autoiml_mbrier(truth, prob)

      per_class = lapply(task$class_names, function(cl) {
        truth01 = as.integer(truth == cl)
        p_hat = as.numeric(prob[, cl])

        cal = .autoiml_calibration_glm(truth01, p_hat)
        ece = .autoiml_ece_binary(truth01, p_hat, bins = bins)
        auc = .autoiml_auc(truth01, p_hat)
        brier = .autoiml_brier(truth01, p_hat)
        ll = .autoiml_logloss(truth01, p_hat)
        prev = mean(truth01)

        rel = .autoiml_reliability_curve_binary(truth01, p_hat, bins = bins)
        dca = .autoiml_dca(truth01, p_hat, thresholds = thresholds)
        dca[, nb_treat_all := prev - (1 - prev) * threshold / (1 - threshold)]
        dca[, nb_treat_none := 0]

        list(
          metrics = data.table::data.table(
            class = cl,
            prevalence = prev,
            auc_ovr = auc,
            brier_ovr = brier,
            logloss_ovr = ll,
            ece_ovr = ece,
            cal_intercept = cal$intercept,
            cal_slope = cal$slope
          ),
          reliability = rel,
          dca = dca
        )
      })

      per_class_metrics = data.table::rbindlist(lapply(per_class, `[[`, "metrics"), fill = TRUE)
      rel_list = setNames(lapply(seq_along(per_class), function(i) per_class[[i]]$reliability), task$class_names)
      dca_list = setNames(lapply(seq_along(per_class), function(i) per_class[[i]]$dca), task$class_names)

      max_ece = max(per_class_metrics$ece_ovr, na.rm = TRUE)
      slope_rng = range(per_class_metrics$cal_slope, na.rm = TRUE)

      status = "pass"
      if (isTRUE(in_sample)) status <- "warn"
      if (is.finite(max_ece) && max_ece > 0.10) status <- "warn"
      if (all(is.finite(slope_rng)) && (slope_rng[1] < 0.7 || slope_rng[2] > 1.3)) status <- "warn"

      summary = "Multiclass classification: one-vs-rest calibration and decision-utility checks computed per class."
      if (isTRUE(in_sample)) {
        summary = paste0(summary, " Note: predictions are in-sample (no OOF predictions available).")
      }

      metrics = data.table::data.table(
        task_type = "classif",
        nclass = nclass,
        overall_logloss = overall_logloss,
        overall_mbrier = overall_mbrier,
        max_ece_ovr = max_ece,
        slope_min = slope_rng[1],
        slope_max = slope_rng[2],
        in_sample = in_sample
      )

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = metrics,
        artifacts = list(
          per_class = per_class_metrics,
          reliability = rel_list,
          dca = dca_list
        ),
        messages = c(
          "OVR calibration/utility curves are provided per class; for deployment-grade multiclass calibration consider dedicated multiclass calibration (e.g., Dirichlet, temperature scaling) and validate on OOF / external data."
        )
      )
    }
  )
)
