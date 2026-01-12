# FILE: R/gate_07a_subgroups.R
#' Gate 7A: Subgroup / measurement audit
#'
#' In the updated framework, subgroup audits are a default expectation for claims that
#' might generalize to people/groups:
#' - performance
#' - calibration (and, for decision support, consequences / utility)
#'
#' This gate operates on *declared* subgroup variables (e.g., `ctx$sensitive_features`)
#' or `task$col_roles$stratum`.
#'
#' @keywords internal
#' @noRd
Gate7aSubgroups = R6::R6Class(
  "Gate7aSubgroups",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G7A",
        name = "Subgroups / measurement audit",
        pdr = "R"
      )
    },

    run = function(ctx) {
      task = ctx$task
      claim = ctx$claim %||% list()
      purpose = claim$purpose %||% ctx$purpose %||% "exploratory"
      purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))

      stakes = tolower(as.character(claim$stakes %||% "medium")[1L])
      if (!stakes %in% c("low", "medium", "high")) stakes = "medium"
      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))

      claims = (claim$claims %||% list())
      decision_claim = isTRUE(claims$decision %||% FALSE)

      decision_spec = (claim$decision_spec %||% list())

      # subgroup variables: explicit declaration preferred
      group_vars = ctx$sensitive_features %||% task$col_roles$stratum %||% character()
      group_vars = unique(as.character(group_vars))
      group_vars = group_vars[nzchar(group_vars)]

      if (length(group_vars) < 1L) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = if (isTRUE(high_stakes) || purpose != "exploratory") "warn" else "skip",
          summary = "No subgroup variables declared (ctx$sensitive_features or task stratum roles). Provide subgroup variables to audit heterogeneity and (for decision use) differential consequences.",
          metrics = NULL
        ))
      }

      pred = ctx$pred
      in_sample = FALSE
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

      Xg = data.table::as.data.table(task$data(cols = group_vars))

      # ---- regression -----------------------------------------------------
      if (inherits(task, "TaskRegr")) {
        y = pred$truth
        yhat = pred$response

        out = data.table::rbindlist(lapply(group_vars, function(g) {
          gg = Xg[[g]]
          levs = unique(gg)
          data.table::rbindlist(lapply(levs, function(lv) {
            idx = which(gg == lv)
            data.table::data.table(
              group_var = g,
              group = as.character(lv),
              n = length(idx),
              rmse = .autoiml_rmse(y[idx], yhat[idx])
            )
          }), fill = TRUE)
        }), fill = TRUE)

        status = if (isTRUE(in_sample)) "warn" else "pass"
        summary = "Subgroup audit computed (regression RMSE by group)."
        if (isTRUE(in_sample)) summary = paste0(summary, " Note: predictions are in-sample.")

        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = status,
          summary = summary,
          metrics = data.table::data.table(n_groups = nrow(out), n_group_vars = length(group_vars), in_sample = in_sample),
          artifacts = list(subgroup = out),
          messages = c("Interpret subgroup differences cautiously: they can reflect sampling variability, construct non-comparability, and/or distribution shift.")
        ))
      }

      # ---- classification -------------------------------------------------
      if (!inherits(task, "TaskClassif") || !inherits(pred, "PredictionClassif")) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = "skip",
          summary = "Subgroup audit currently supports regression and classification tasks.",
          metrics = NULL
        ))
      }

      nclass = length(task$class_names)

      # Binary classification subgroup audit (most common)
      if (nclass == 2L) {
        pos = task$positive %||% task$class_names[2L]
        truth01 = as.integer(pred$truth == pos)
        p_hat = as.numeric(pred$prob[, pos])

        # Utility/cost spec (optional)
        costs = (decision_spec$costs %||% list())
        if (is.environment(costs)) costs <- as.list(costs)
        if (!is.list(costs)) costs <- list()

        utility = (decision_spec$utility %||% list())
        if (is.environment(utility)) utility <- as.list(utility)
        if (!is.list(utility)) utility <- list()

        costs$fp = as.numeric(costs$fp %||% NA_real_)
        costs$fn = as.numeric(costs$fn %||% NA_real_)

        utility$tp = as.numeric(utility$tp %||% NA_real_)
        utility$tn = as.numeric(utility$tn %||% NA_real_)
        utility$fp = as.numeric(utility$fp %||% NA_real_)
        utility$fn = as.numeric(utility$fn %||% NA_real_)

        has_costs = is.finite(costs$fp) && is.finite(costs$fn)
        has_utility = all(is.finite(c(utility$tp, utility$tn, utility$fp, utility$fn)))
        utility_spec = if (isTRUE(has_utility)) "utility" else if (isTRUE(has_costs)) "costs" else "none"

        thresholds = suppressWarnings(as.numeric(decision_spec$thresholds %||% seq(0.01, 0.99, by = 0.01)))
        thresholds = thresholds[is.finite(thresholds) & thresholds > 0 & thresholds < 1]
        thresholds = sort(unique(thresholds))

        # choose an "overall" operating threshold if possible
        thr_use = NA_real_
        if (utility_spec != "none" && length(thresholds) >= 1L) {
          # pick threshold that optimizes overall expected cost/utility on the provided predictions
          vals = vapply(thresholds, function(t) {
            yhat = as.integer(p_hat >= t)
            tp = sum(yhat == 1L & truth01 == 1L)
            fp = sum(yhat == 1L & truth01 == 0L)
            tn = sum(yhat == 0L & truth01 == 0L)
            fn = sum(yhat == 0L & truth01 == 1L)
            n = length(truth01)
            if (utility_spec == "costs") {
              cost_total = (costs$fp * fp) + (costs$fn * fn)
              return(cost_total / n)
            }
            util_total = utility$tp * tp + utility$fp * fp + utility$tn * tn + utility$fn * fn
            return(util_total / n)
          }, numeric(1L))

          if (utility_spec == "costs") {
            thr_use = thresholds[which.min(vals)][1L]
          } else {
            thr_use = thresholds[which.max(vals)][1L]
          }
        }

        # group metrics
        subgroup = data.table::rbindlist(lapply(group_vars, function(g) {
          gg = Xg[[g]]
          levs = unique(gg)
          data.table::rbindlist(lapply(levs, function(lv) {
            idx = which(gg == lv)
            if (length(idx) < 5L) {
              return(NULL)
            }

            cal = .autoiml_calibration_glm(truth01[idx], p_hat[idx])
            ece = .autoiml_ece_binary(truth01[idx], p_hat[idx], bins = 10L)
            auc = .autoiml_auc(truth01[idx], p_hat[idx])
            brier = .autoiml_brier(truth01[idx], p_hat[idx])
            prev = mean(truth01[idx])

            out = data.table::data.table(
              group_var = g,
              group = as.character(lv),
              n = length(idx),
              prevalence = prev,
              auc = auc,
              brier = brier,
              ece = ece,
              cal_intercept = cal$intercept,
              cal_slope = cal$slope
            )

            # utility/cost at selected threshold
            if (utility_spec != "none" && is.finite(thr_use)) {
              yhat = as.integer(p_hat[idx] >= thr_use)
              tp = sum(yhat == 1L & truth01[idx] == 1L)
              fp = sum(yhat == 1L & truth01[idx] == 0L)
              tn = sum(yhat == 0L & truth01[idx] == 0L)
              fn = sum(yhat == 0L & truth01[idx] == 1L)
              nloc = length(idx)

              out[, `:=`(thr_use = thr_use, tp = tp, fp = fp, tn = tn, fn = fn)]
              if (utility_spec == "costs") {
                out[, expected_cost := (costs$fp * fp + costs$fn * fn) / nloc]
              } else if (utility_spec == "utility") {
                out[, expected_utility := (utility$tp * tp + utility$fp * fp + utility$tn * tn + utility$fn * fn) / nloc]
              }
            }

            out
          }), fill = TRUE)
        }), fill = TRUE)

        status = if (isTRUE(in_sample)) "warn" else "pass"
        if (isTRUE(in_sample)) status <- "warn"

        # Flag large subgroup calibration dispersion heuristically
        if (any(is.finite(subgroup$ece) & subgroup$ece > 0.10, na.rm = TRUE)) status <- "warn"

        summary = "Subgroup audit computed (binary classification performance + calibration; utility if specified)."
        if (isTRUE(in_sample)) summary = paste0(summary, " Note: predictions are in-sample.")

        msgs = c(
          "If subgroup gaps are observed, consider (i) measurement non-comparability, (ii) differential missingness, (iii) label shift, (iv) model misspecification, and (v) decision policy differences."
        )
        if (isTRUE(decision_claim) && utility_spec == "none") {
          msgs = c(msgs, "Decision claim requested but no utility/cost spec provided: subgroup utility comparisons are omitted.")
        }

        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = status,
          summary = summary,
          metrics = data.table::data.table(
            n_group_vars = length(group_vars),
            n_rows = task$nrow,
            utility_spec = utility_spec,
            thr_use = thr_use,
            in_sample = in_sample
          ),
          artifacts = list(subgroup = subgroup),
          messages = msgs
        ))
      }

      # Multiclass: provide basic group prevalence + logloss by group (OVR curves handled in Gate 3).
      prob = .autoiml_pred_prob_matrix(pred, task)
      truth = pred$truth

      subgroup = data.table::rbindlist(lapply(group_vars, function(g) {
        gg = Xg[[g]]
        levs = unique(gg)
        data.table::rbindlist(lapply(levs, function(lv) {
          idx = which(gg == lv)
          if (length(idx) < 5L) {
            return(NULL)
          }
          data.table::data.table(
            group_var = g,
            group = as.character(lv),
            n = length(idx),
            logloss = .autoiml_logloss_multiclass(truth[idx], prob[idx, , drop = FALSE]),
            mbrier = .autoiml_mbrier(truth[idx], prob[idx, , drop = FALSE])
          )
        }), fill = TRUE)
      }), fill = TRUE)

      status = if (isTRUE(in_sample)) "warn" else "pass"
      summary = "Subgroup audit computed (multiclass logloss/mbrier by group)."
      if (isTRUE(in_sample)) summary = paste0(summary, " Note: predictions are in-sample.")

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = data.table::data.table(n_group_vars = length(group_vars), in_sample = in_sample),
        artifacts = list(subgroup = subgroup),
        messages = c("Subgroup audit is descriptive: confirm whether measurement/comparability assumptions hold before attributing subgroup differences to substantive effects.")
      )
    }
  )
)
