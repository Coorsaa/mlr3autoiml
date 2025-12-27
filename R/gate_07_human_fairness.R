#' Gate 7: Human factors and fairness
#' @keywords internal
#' @noRd
Gate7HumanFairness = R6::R6Class(
  "Gate7HumanFairness",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G7",
        name = "Human factors and fairness",
        pdr = "R"
      )
    },

    run = function(ctx) {
      task = ctx$task
      pred = ctx$pred

      if (is.null(pred)) {
        return(GateResult$new(
          gate_id = self$id, gate_name = self$name, pdr = self$pdr,
          status = "skip",
          summary = "No out-of-fold predictions available; run Gate 1 first.",
          metrics = NULL
        ))
      }

      sensitive = ctx$sensitive_features %||% task$col_roles$stratum %||% character()
      sensitive = unique(sensitive)
      sensitive = sensitive[nzchar(sensitive)]

      if (length(sensitive) == 0L) {
        return(GateResult$new(
          gate_id = self$id, gate_name = self$name, pdr = self$pdr,
          status = "skip",
          summary = "No sensitive/stratum features specified; set ctx$sensitive_features or Task column roles.",
          metrics = NULL
        ))
      }

      out = list()
      flags = list()

      for (s in sensitive) {
        g = task$data(cols = s)[[s]]
        if (inherits(pred, "PredictionClassif") && length(task$class_names) == 2L) {
          pos = task$positive %||% task$class_names[2L]
          truth01 = as.integer(pred$truth == pos)
          p_hat = .autoiml_pred_numeric(pred, task)
          dt = data.table::data.table(group = g, truth01 = truth01, p_hat = p_hat)

          perf = dt[, .(
            n = .N,
            prev = mean(truth01),
            auc = .autoiml_auc(truth01, p_hat),
            brier = .autoiml_brier(truth01, p_hat)
          ), by = group]

          # calibration per group if enough positives/negatives
          cal = dt[, {
            n1 = sum(truth01 == 1L)
            n0 = sum(truth01 == 0L)
            if (n1 >= 10 && n0 >= 10) {
              c0 = .autoiml_calibration_glm(truth01, p_hat)
              list(intercept = c0$intercept, slope = c0$slope)
            } else {
              list(intercept = NA_real_, slope = NA_real_)
            }
          }, by = group]

          perf = merge(perf, cal, by = "group", all.x = TRUE)
          perf[, feature := s]
          out[[s]] = perf

          rng = range(perf$auc, na.rm = TRUE)
          flags[[s]] = is.finite(rng[2] - rng[1]) && (rng[2] - rng[1]) > 0.05
        } else if (inherits(pred, "PredictionRegr")) {
          y = pred$truth
          yhat = pred$response
          dt = data.table::data.table(group = g, y = y, yhat = yhat)
          perf = dt[, .(
            n = .N,
            rmse = .autoiml_rmse(y, yhat),
            bias = mean(yhat - y, na.rm = TRUE)
          ), by = group]
          perf[, feature := s]
          out[[s]] = perf

          rng = range(perf$rmse, na.rm = TRUE)
          flags[[s]] = is.finite(rng[2] - rng[1]) && (rng[2] - rng[1]) > 0.20 * median(perf$rmse, na.rm = TRUE)
        }
      }

      fairness_tbl = data.table::rbindlist(out, fill = TRUE)
      any_flag = any(unlist(flags), na.rm = TRUE)

      status = if (any_flag) "warn" else "pass"
      summary = if (any_flag) {
        "Potential subgroup disparities detected; report subgroup metrics, consider mitigation, and evaluate impacts on end users."
      } else {
        "No substantial subgroup disparities detected (based on provided sensitive/stratum features and available metrics)."
      }

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = data.table::data.table(any_disparity_flag = any_flag, n_sensitive_features = length(sensitive)),
        artifacts = list(subgroup_table = fairness_tbl, disparity_flags = flags),
        messages = c("Human-factors evaluation (comprehension, overtrust, workflow fit) is not automated; export explanations and run user studies where appropriate.")
      )
    }
  )
)
