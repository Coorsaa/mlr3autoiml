# FILE: R/gate_03_calibration.R

#' @title Gate 3: Calibration and Decision Utility
#'
#' @description
#' Implements calibration and decision-utility diagnostics for classification tasks.
#'
#' For \strong{decision support}, calibration alone is insufficient: users should specify
#' decision utilities/costs (or a justified threshold policy) and evaluate
#' downstream consequences (utility curves / DCA).
#'
#' @section Currently Implemented:
#' \describe{
#'   \item{Binary classification}{Calibration intercept/slope, ECE, reliability curve,
#'     decision curve analysis (net benefit), and cost-/utility-sensitive threshold sweep.}
#'   \item{Multiclass classification}{One-vs-rest calibration/utility checks per class
#'     (intercept/slope, ECE, reliability, OVR net benefit) plus overall logloss and
#'     multiclass Brier score.}
#'   \item{Regression}{Basic error metrics (calibration/utility mainly relevant for
#'     probabilistic forecasts).}
#' }
#'
#' @name Gate3Calibration
#' @keywords internal
NULL

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
      purpose = ctx$purpose %??% "exploratory"
      cfg = ctx$calibration %??% list()
      .autoiml_assert_known_names(cfg, c("thresholds", "bins"), "ctx$calibration")

      claim = ctx$claim %??% list()
      claims = (claim$claims %??% list())
      decision_claim = isTRUE(claims$decision %??% FALSE)

      decision_spec = (claim$decision_spec %??% list())
      .autoiml_assert_known_names(decision_spec, c("thresholds", "costs", "utility", "positive_class"), "ctx$claim$decision_spec")
      claim_thr = decision_spec$thresholds %??% NULL
      thresholds = claim_thr %??% cfg$thresholds %??% seq(0.01, 0.99, by = 0.01)

      bins = as.integer(cfg$bins %??% 10L)

      # Utility / cost specification (optional but expected for decision support)
      costs = .autoiml_as_list(decision_spec$costs)
      utility = .autoiml_as_list(decision_spec$utility)

      # normalize missing entries
      costs$tp = as.numeric(costs$tp %??% 0)
      costs$tn = as.numeric(costs$tn %??% 0)
      costs$fp = as.numeric(costs$fp %??% NA_real_)
      costs$fn = as.numeric(costs$fn %??% NA_real_)

      utility$tp = as.numeric(utility$tp %??% NA_real_)
      utility$tn = as.numeric(utility$tn %??% NA_real_)
      utility$fp = as.numeric(utility$fp %??% NA_real_)
      utility$fn = as.numeric(utility$fn %??% NA_real_)

      has_costs = is.finite(costs$fp) && is.finite(costs$fn)
      has_utility = all(is.finite(c(utility$tp, utility$tn, utility$fp, utility$fn)))

      utility_spec = if (isTRUE(has_utility)) "utility" else if (isTRUE(has_costs)) "costs" else "none"

      thr_num = suppressWarnings(as.numeric(thresholds))
      thr_num = thr_num[is.finite(thr_num) & thr_num > 0 & thr_num < 1]
      thr_num = sort(unique(thr_num))
      decision_range = data.table::data.table(
        decision_claim = isTRUE(decision_claim),
        utility_spec = utility_spec,
        n_thresholds = length(thr_num),
        thr_min = if (length(thr_num) > 0L) min(thr_num) else NA_real_,
        thr_max = if (length(thr_num) > 0L) max(thr_num) else NA_real_
      )

      pred = ctx$pred
      if (is.null(pred)) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = "fail",
          summary = "Gate 3 requires out-of-fold predictions from Gate 1.",
          metrics = NULL,
          artifacts = list(),
          messages = c("Run Gate 1 validity before calibration/decision utility diagnostics.")
        ))
      }

      # --------------------------------------------------------------------
      # Regression
      if (inherits(task, "TaskRegr")) {
        y = pred$truth
        yhat = pred$response
        # Use mlr3 Prediction scoring directly
        rmse = pred$score(mlr3::msr("regr.rmse"))

        status = "pass"
        summary = "Regression: basic error metrics computed (calibration/utility mainly relevant for probabilistic forecasts)."

        metrics = data.table::data.table(
          rmse = rmse,
          n = length(y)
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
        pos = task$positive %??% task$class_names[2L]
        truth01 = as.integer(pred$truth == pos)
        p_hat = as.numeric(pred$prob[, pos])

        cal = .autoiml_calibration_glm(truth01, p_hat)
        ece = .autoiml_ece_binary(truth01, p_hat, bins = bins)

        # Use mlr3 Prediction scoring directly for standard metrics
        brier = pred$score(mlr3::msr("classif.bbrier"))
        ll = pred$score(mlr3::msr("classif.logloss"))
        auc = pred$score(mlr3::msr("classif.auc"))

        rel = .autoiml_reliability_curve_binary(truth01, p_hat, bins = bins)
        dca = .autoiml_dca(truth01, p_hat, thresholds = thresholds)
        prev = mean(truth01)
        dca[, nb_treat_all := prev - (1 - prev) * threshold / (1 - threshold)]
        dca[, nb_treat_none := 0]

        # ---- cost-/utility-sensitive threshold sweep ---------------------
        thr = suppressWarnings(as.numeric(thresholds))
        thr = thr[is.finite(thr) & thr > 0 & thr < 1]
        thr = sort(unique(thr))

        util_curve = NULL
        thr_opt = NA_real_
        thr_opt_value = NA_real_

        if (length(thr) >= 1L && (utility_spec != "none")) {
          n = length(truth01)
          util_curve = mlr3misc::map_dtr(thr, function(t) {
            yhat = as.integer(p_hat >= t)
            tp = sum(yhat == 1L & truth01 == 1L)
            fp = sum(yhat == 1L & truth01 == 0L)
            tn = sum(yhat == 0L & truth01 == 0L)
            fn = sum(yhat == 0L & truth01 == 1L)

            out = data.table::data.table(
              threshold = t,
              tp = tp, fp = fp, tn = tn, fn = fn,
              tpr = if (sum(truth01 == 1L) > 0) tp / sum(truth01 == 1L) else NA_real_,
              fpr = if (sum(truth01 == 0L) > 0) fp / sum(truth01 == 0L) else NA_real_
            )

            if (utility_spec == "costs") {
              cost_total = costs$tp * tp + costs$fp * fp + costs$tn * tn + costs$fn * fn
              out[, expected_cost := cost_total / n]
            } else if (utility_spec == "utility") {
              util_total = utility$tp * tp + utility$fp * fp + utility$tn * tn + utility$fn * fn
              out[, expected_utility := util_total / n]
            }
            out
          }, .fill = TRUE)

          if (utility_spec == "costs") {
            best = util_curve[which.min(expected_cost)][1L]
            thr_opt = best$threshold
            thr_opt_value = best$expected_cost
          } else if (utility_spec == "utility") {
            best = util_curve[which.max(expected_utility)][1L]
            thr_opt = best$threshold
            thr_opt_value = best$expected_utility
          }
        }

        # Heuristic gate decision
        status = "pass"

        # calibration concern heuristics
        if (is.finite(ece) && ece > 0.10) status <- "warn"
        if (is.finite(cal$slope) && (cal$slope < 0.8 || cal$slope > 1.2)) status <- "warn"

        # decision-support concern heuristics
        msgs = character()
        if (isTRUE(decision_claim) && utility_spec == "none") {
          status = "warn"
          msgs = c(msgs, "Decision claim requested but no utility/cost specification provided; threshold recommendations are not cost-sensitive without explicit utilities/costs.")
        }

        summary = "Binary calibration/utility checks computed (intercept/slope, ECE, reliability, net benefit, cost-/utility sweep)."
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
          utility_spec = utility_spec,
          opt_threshold = thr_opt,
          opt_value = thr_opt_value
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
            dca = dca,
            utility_curve = util_curve,
            utility_spec = list(costs = costs, utility = utility, type = utility_spec),
            decision_range = decision_range
          ),
          messages = c(
            msgs,
            "For decision support, justify threshold policy (utilities/costs, prevalence, constraints) and validate net benefit / expected utility on OOF or external data."
          )
        ))
      }

      # --------------------------------------------------------------------
      # Multiclass classification: one-vs-rest checks
      prob = .autoiml_pred_prob_matrix(pred, task)
      truth = pred$truth

      # Use mlr3 Prediction scoring for overall metrics
      overall_logloss = pred$score(mlr3::msr("classif.logloss"))
      overall_mbrier = pred$score(mlr3::msr("classif.mbrier"))

      # Per-class one-vs-rest metrics (need manual computation for OvR AUC etc.)
      per_class = lapply(task$class_names, function(cl) {
        truth01 = as.integer(truth == cl)
        p_hat = as.numeric(prob[, cl])

        cal = .autoiml_calibration_glm(truth01, p_hat)
        ece = .autoiml_ece_binary(truth01, p_hat, bins = bins)
        # One-vs-rest AUC/Brier need custom computation (mlr3 doesn't provide OvR directly)
        truth_factor = factor(truth01, levels = c(0L, 1L))
        auc = mlr3measures::auc(truth_factor, p_hat, positive = "1")
        brier = mlr3measures::bbrier(truth_factor, p_hat, positive = "1")
        ll = mlr3measures::logloss(truth_factor, cbind("0" = 1 - p_hat, "1" = p_hat))
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

      per_class_metrics = mlr3misc::map_dtr(per_class, `[[`, "metrics", .fill = TRUE)
      rel_list = setNames(lapply(seq_along(per_class), function(i) per_class[[i]]$reliability), task$class_names)
      dca_list = setNames(lapply(seq_along(per_class), function(i) per_class[[i]]$dca), task$class_names)

      max_ece = max(per_class_metrics$ece_ovr, na.rm = TRUE)
      slope_rng = range(per_class_metrics$cal_slope, na.rm = TRUE)

      status = "pass"
      if (is.finite(max_ece) && max_ece > 0.10) status <- "warn"
      if (all(is.finite(slope_rng)) && (slope_rng[1] < 0.7 || slope_rng[2] > 1.3)) status <- "warn"

      msgs = character()
      if (isTRUE(decision_claim) && utility_spec == "none") {
        status = "warn"
        msgs = c(msgs, "Decision claim requested but no utility/cost specification provided; multiclass cost-sensitive decision analysis is not implemented here.")
      }

      summary = "Multiclass classification: one-vs-rest calibration and decision-utility checks computed per class."

      metrics = data.table::data.table(
        task_type = "classif",
        nclass = nclass,
        overall_logloss = overall_logloss,
        overall_mbrier = overall_mbrier,
        max_ece_ovr = max_ece,
        slope_min = slope_rng[1],
        slope_max = slope_rng[2],
        utility_spec = utility_spec
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
          dca = dca_list,
          utility_spec = list(costs = costs, utility = utility, type = utility_spec),
          decision_range = decision_range
        ),
        messages = c(
          msgs,
          "OVR calibration/utility curves are provided per class; for deployment-grade multiclass calibration consider dedicated multiclass calibration (e.g., Dirichlet, temperature scaling) and validate on OOF / external data."
        )
      )
    }
  )
)
