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
      .autoiml_assert_known_names(
        cfg,
        c("thresholds", "bins", "maximum_ece", "calibration_slope_range", "maximum_abs_intercept"),
        "ctx$calibration"
      )

      maximum_ece = cfg$maximum_ece %??% NULL
      calibration_slope_range = cfg$calibration_slope_range %??% NULL
      maximum_abs_intercept = cfg$maximum_abs_intercept %??% NULL
      checkmate::assert_number(maximum_ece, lower = 0, finite = TRUE, null.ok = TRUE)
      checkmate::assert_numeric(
        calibration_slope_range,
        lower = 0,
        finite = TRUE,
        len = 2L,
        null.ok = TRUE
      )
      checkmate::assert_number(maximum_abs_intercept, lower = 0, finite = TRUE, null.ok = TRUE)
      if (!is.null(calibration_slope_range) && calibration_slope_range[[1L]] > calibration_slope_range[[2L]]) {
        stop("`ctx$calibration$calibration_slope_range` must be ordered from lower to upper.", call. = FALSE)
      }
      calibration_criteria_declared = any(vapply(
        list(maximum_ece, calibration_slope_range, maximum_abs_intercept),
        Negate(is.null),
        logical(1L)
      ))

      claim = ctx$claim %??% list()
      claims = (claim$claims %??% list())
      decision_claim = isTRUE(claims$decision %??% FALSE)

      decision_spec = (claim$decision_spec %??% list())
      .autoiml_assert_known_names(
        decision_spec,
        c("thresholds", "costs", "utility", "positive_class"),
        "ctx$claim$decision_spec"
      )

      # DCA is always evaluated over the full [0, 1] range.
      # decision_spec$thresholds defines only the decision-relevant shading region.
      thresholds = cfg$thresholds %??% seq(0, 1, by = 0.01)

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

      # decision_range for plot shading: derived from decision_spec$thresholds only
      shade_num = if (!is.null(decision_spec$thresholds)) {
        v = suppressWarnings(as.numeric(decision_spec$thresholds))
        v[is.finite(v) & v > 0 & v < 1]
      } else numeric(0)

      decision_range = data.table::data.table(
        decision_claim = isTRUE(decision_claim),
        utility_spec = utility_spec,
        n_thresholds = length(shade_num),
        thr_min = if (length(shade_num) > 0L) min(shade_num) else NA_real_,
        thr_max = if (length(shade_num) > 0L) max(shade_num) else NA_real_
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

        status = "warn"
        summary = paste(
          "Regression error was computed, but this compatibility gate does not adjudicate regression calibration.",
          "Use the claim-scoped CSDG calibration module when a regression calibration claim is in scope."
        )

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

        rel = .autoiml_reliability_curve_boot(truth01, p_hat, bins = bins, B = 200L)
        dca = .autoiml_dca_boot(truth01, p_hat, thresholds = thresholds, B = 200L)
        prev = mean(truth01)
        dca[, nb_treat_all := ifelse(threshold >= 1, NA_real_, prev - (1 - prev) * threshold / (1 - threshold))]
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

        calibration_concerns = character()
        if (!is.null(maximum_ece) && is.finite(ece) && ece > maximum_ece) {
          calibration_concerns = c(calibration_concerns, "ECE exceeds the declared maximum")
        }
        if (!is.null(calibration_slope_range) && is.finite(cal$slope) &&
            (cal$slope < calibration_slope_range[[1L]] || cal$slope > calibration_slope_range[[2L]])) {
          calibration_concerns = c(calibration_concerns, "calibration slope is outside the declared range")
        }
        if (!is.null(maximum_abs_intercept) && is.finite(cal$intercept) &&
            abs(cal$intercept) > maximum_abs_intercept) {
          calibration_concerns = c(calibration_concerns, "absolute calibration intercept exceeds the declared maximum")
        }
        status = if (!calibration_criteria_declared || length(calibration_concerns)) "warn" else "pass"

        # decision-support concern heuristics
        msgs = character()
        if (isTRUE(decision_claim) && utility_spec == "none") {
          status = "warn"
          msgs = c(
            msgs,
            paste(
              "Decision claim requested but no utility/cost specification was provided;",
              "threshold recommendations are not cost-sensitive without explicit utilities/costs."
            )
          )
        }

        summary = if (calibration_criteria_declared) {
          paste(
            paste(
              "Binary calibration and decision-utility diagnostics were computed against",
              "the declared calibration criteria."
            ),
            if (length(calibration_concerns)) {
              paste(calibration_concerns, collapse = "; ")
            } else {
              "No declared criterion was exceeded."
            }
          )
        } else {
          paste(
            "Binary calibration and decision-utility diagnostics were computed.",
            "Calibration adequacy remains unadjudicated because no claim-specific criteria were declared."
          )
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
          calibration_criteria_declared = calibration_criteria_declared,
          maximum_ece_criterion = maximum_ece %??% NA_real_,
          minimum_slope_criterion = if (is.null(calibration_slope_range)) NA_real_ else calibration_slope_range[[1L]],
          maximum_slope_criterion = if (is.null(calibration_slope_range)) NA_real_ else calibration_slope_range[[2L]],
          maximum_abs_intercept_criterion = maximum_abs_intercept %??% NA_real_,
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
            if (!calibration_criteria_declared) {
              paste(
                "Declare claim- and use-specific calibration criteria to adjudicate adequacy;",
                "the package supplies no universal cutoff."
              )
            } else if (length(calibration_concerns)) {
              paste("Declared calibration concern(s):", paste(calibration_concerns, collapse = "; "))
            },
            paste(
              "For decision support, justify the threshold policy through utilities, costs, prevalence, and constraints,",
              "and validate net benefit or expected utility on out-of-fold or external data."
            )
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
        dca[, nb_treat_all := ifelse(threshold >= 1, NA_real_, prev - (1 - prev) * threshold / (1 - threshold))]
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

      calibration_concerns = character()
      if (!is.null(maximum_ece) && is.finite(max_ece) && max_ece > maximum_ece) {
        calibration_concerns = c(calibration_concerns, "maximum one-vs-rest ECE exceeds the declared maximum")
      }
      if (!is.null(calibration_slope_range) && all(is.finite(slope_rng)) &&
          (slope_rng[[1L]] < calibration_slope_range[[1L]] ||
            slope_rng[[2L]] > calibration_slope_range[[2L]])) {
        calibration_concerns = c(calibration_concerns, "one-vs-rest calibration slopes exceed the declared range")
      }
      if (!is.null(maximum_abs_intercept)) {
        maximum_observed_abs_intercept = max(abs(per_class_metrics$cal_intercept), na.rm = TRUE)
        if (is.finite(maximum_observed_abs_intercept) && maximum_observed_abs_intercept > maximum_abs_intercept) {
          calibration_concerns = c(
            calibration_concerns,
            "maximum absolute one-vs-rest calibration intercept exceeds the declared maximum"
          )
        }
      }
      status = if (!calibration_criteria_declared || length(calibration_concerns)) "warn" else "pass"

      msgs = character()
      if (isTRUE(decision_claim) && utility_spec == "none") {
        status = "warn"
        msgs = c(
          msgs,
          paste(
            "Decision claim requested but no utility/cost specification was provided;",
            "multiclass cost-sensitive decision analysis is not implemented here."
          )
        )
      }

      summary = if (calibration_criteria_declared) {
        paste(
          paste(
            "Multiclass one-vs-rest calibration and decision-utility diagnostics were computed",
            "against the declared criteria."
          ),
          if (length(calibration_concerns)) {
            paste(calibration_concerns, collapse = "; ")
          } else {
            "No declared criterion was exceeded."
          }
        )
      } else {
        paste(
          "Multiclass one-vs-rest calibration and decision-utility diagnostics were computed.",
          "Calibration adequacy remains unadjudicated because no claim-specific criteria were declared."
        )
      }

      metrics = data.table::data.table(
        task_type = "classif",
        nclass = nclass,
        overall_logloss = overall_logloss,
        overall_mbrier = overall_mbrier,
        max_ece_ovr = max_ece,
        slope_min = slope_rng[1],
        slope_max = slope_rng[2],
        calibration_criteria_declared = calibration_criteria_declared,
        maximum_ece_criterion = maximum_ece %??% NA_real_,
        minimum_slope_criterion = if (is.null(calibration_slope_range)) NA_real_ else calibration_slope_range[[1L]],
        maximum_slope_criterion = if (is.null(calibration_slope_range)) NA_real_ else calibration_slope_range[[2L]],
        maximum_abs_intercept_criterion = maximum_abs_intercept %??% NA_real_,
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
          if (!calibration_criteria_declared) {
            paste(
              "Declare claim- and use-specific calibration criteria to adjudicate adequacy;",
              "the package supplies no universal cutoff."
            )
          } else if (length(calibration_concerns)) {
            paste("Declared calibration concern(s):", paste(calibration_concerns, collapse = "; "))
          },
          paste(
            "One-vs-rest calibration and utility curves are provided per class; for deployment-grade multiclass",
            "calibration, consider a dedicated method and validate it on out-of-fold or external data."
          )
        )
      )
    }
  )
)
