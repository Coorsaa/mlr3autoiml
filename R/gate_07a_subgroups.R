# FILE: R/gate_07a_subgroups.R

#' @title Gate 7A: Subgroup / Measurement Audit
#'
#' @description
#' In the updated framework, subgroup audits are a default expectation for claims that
#' might generalize to people/groups. This gate assesses:
#' \itemize{
#'   \item Subgroup-specific performance metrics
#'   \item Subgroup-specific calibration (and, for decision support, consequences / utility)
#' }
#'
#' The gate operates on \emph{declared} subgroup variables (e.g., `ctx$sensitive_features`)
#' or `task$col_roles$stratum`.
#'
#' For high-stakes decision/deployment contexts, missing subgroup declarations are
#' treated as a hard evidentiary failure.
#'
#' @section Binary Classification:
#' Computes per-group AUC, Brier score, ECE, calibration intercept/slope, and
#' (if utility/cost specification provided) expected utility or cost.
#'
#' @section Regression:
#' Computes per-group RMSE.
#'
#' @name Gate7aSubgroups
#' @keywords internal
NULL

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
      claim = ctx$claim %??% list()
      .autoiml_assert_known_names(
        .autoiml_as_list(claim),
        c(
          "purpose", "claims", "semantics", "stakes", "audience",
          "decision_spec", "actionability", "causal_assumptions", "human_factors_evidence",
          "target_population", "setting", "time_horizon", "transport_boundary",
          "intended_use", "intended_non_use", "prohibited_interpretations",
          "decision_policy_rationale"
        ),
        "ctx$claim"
      )
      purpose = claim$purpose %??% ctx$purpose %??% "exploratory"
      purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))

      stakes = tolower(as.character(claim$stakes %??% "medium")[1L])
      if (!stakes %in% c("low", "medium", "high")) stakes = "medium"
      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))

      claims = (claim$claims %??% list())
      decision_claim = isTRUE(claims$decision %??% FALSE)

      decision_spec = (claim$decision_spec %??% list())
      .autoiml_assert_known_names(.autoiml_as_list(decision_spec), c("thresholds", "costs", "utility", "positive_class"), "ctx$claim$decision_spec")

      has_content = function(x) {
        if (is.null(x)) {
          return(FALSE)
        }
        vals = as.character(unlist(x, use.names = FALSE))
        vals = vals[!is.na(vals)]
        any(nzchar(trimws(vals)))
      }

      meas = .autoiml_as_list(ctx$measurement)
      measurement_invariance_present = isTRUE(has_content(meas$invariance))

      # subgroup variables: explicit declaration preferred
      group_vars = ctx$sensitive_features %??% task$col_roles$stratum %??% character()
      group_vars = unique(as.character(group_vars))
      group_vars = group_vars[nzchar(group_vars)]

      if (length(group_vars) < 1L) {
        status = if (isTRUE(high_stakes)) "fail" else if (purpose != "exploratory") "warn" else "skip"
        summary = if (isTRUE(high_stakes)) {
          "High-stakes use requires declared subgroup variables (ctx$sensitive_features or task stratum roles)."
        } else {
          "No subgroup variables declared (ctx$sensitive_features or task stratum roles). Provide subgroup variables to audit heterogeneity and (for decision use) differential consequences."
        }

        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = status,
          summary = summary,
          metrics = data.table::data.table(
            required = isTRUE(high_stakes),
            provided = FALSE,
            stakes = stakes,
            purpose = purpose
          )
        ))
      }

      if (isTRUE(high_stakes) && !isTRUE(measurement_invariance_present)) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = "fail",
          summary = "High-stakes subgroup claims require measurement comparability evidence (ctx$measurement$invariance).",
          metrics = data.table::data.table(
            required = TRUE,
            provided = FALSE,
            measurement_invariance_present = FALSE,
            stakes = stakes,
            purpose = purpose
          )
        ))
      }

      pred = ctx$pred
      if (is.null(pred)) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = "fail",
          summary = "Gate 7A requires prediction artifacts from Gate 1.",
          metrics = NULL
        ))
      }

      Xg = data.table::as.data.table(task$data(cols = group_vars))

      # ---- regression -----------------------------------------------------
      if (inherits(task, "TaskRegr")) {
        y = pred$truth
        yhat = pred$response

        out = mlr3misc::map_dtr(group_vars, function(g) {
          gg = Xg[[g]]
          levs = unique(gg)
          mlr3misc::map_dtr(levs, function(lv) {
            idx = which(gg == lv)
            yi = y[idx]; yhati = yhat[idx]
            ss_res = sum((yi - yhati)^2)
            ss_tot = sum((yi - mean(yi))^2)
            data.table::data.table(
              group_var = g,
              group = as.character(lv),
              n = length(idx),
              mean_y = mean(yi),
              rmse = sqrt(mean((yi - yhati)^2)),
              r2 = if (ss_tot < .Machine$double.eps) NA_real_ else 1 - ss_res / ss_tot
            )
          }, .fill = TRUE)
        }, .fill = TRUE)

        status = "pass"
        summary = "Subgroup audit computed (regression RMSE, R\u00b2, mean_y by group)."

        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = status,
          summary = summary,
          metrics = data.table::data.table(n_groups = nrow(out), n_group_vars = length(group_vars)),
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
        pos = task$positive %??% task$class_names[2L]
        truth01 = as.integer(pred$truth == pos)
        p_hat = as.numeric(pred$prob[, pos])

        # Utility/cost spec (optional)
        costs = .autoiml_as_list(decision_spec$costs)
        utility = .autoiml_as_list(decision_spec$utility)

        costs$fp = as.numeric(costs$fp %??% NA_real_)
        costs$fn = as.numeric(costs$fn %??% NA_real_)

        utility$tp = as.numeric(utility$tp %??% NA_real_)
        utility$tn = as.numeric(utility$tn %??% NA_real_)
        utility$fp = as.numeric(utility$fp %??% NA_real_)
        utility$fn = as.numeric(utility$fn %??% NA_real_)

        has_costs = is.finite(costs$fp) && is.finite(costs$fn)
        has_utility = all(is.finite(c(utility$tp, utility$tn, utility$fp, utility$fn)))
        utility_spec = if (isTRUE(has_utility)) "utility" else if (isTRUE(has_costs)) "costs" else "none"

        thresholds = suppressWarnings(as.numeric(decision_spec$thresholds %??% seq(0.01, 0.99, by = 0.01)))
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
        subgroup = mlr3misc::map_dtr(group_vars, function(g) {
          gg = Xg[[g]]
          levs = unique(gg)
          mlr3misc::map_dtr(levs, function(lv) {
            idx = which(gg == lv)
            if (length(idx) < 5L) {
              return(NULL)
            }

            cal = .autoiml_calibration_glm(truth01[idx], p_hat[idx])
            ece = .autoiml_ece_binary(truth01[idx], p_hat[idx], bins = 10L)
            # Use mlr3measures directly for subgroup metrics
            truth_sub = factor(truth01[idx], levels = c(0L, 1L))
            auc = mlr3measures::auc(truth_sub, p_hat[idx], positive = "1")
            brier = mlr3measures::bbrier(truth_sub, p_hat[idx], positive = "1")
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
          }, .fill = TRUE)
        }, .fill = TRUE)

        status = "pass"

        # Flag large subgroup calibration dispersion heuristically
        if (any(is.finite(subgroup$ece) & subgroup$ece > 0.10, na.rm = TRUE)) status <- "warn"

        summary = "Subgroup audit computed (binary classification performance + calibration; utility if specified)."

        subgroup_expl_stability = NULL
        if (!is.null(ctx$final_model)) {
          model = ctx$final_model
          feat_types = tryCatch(task$feature_types, error = function(e) NULL)
          num_feats = if (!is.null(feat_types)) {
            intersect(feat_types$id[feat_types$type %in% c("numeric", "integer")], task$feature_names)
          } else {
            character(0)
          }
          num_feats = utils::head(num_feats, 3L)

          score_prob = function(y01, p) {
            y01 = as.integer(y01)
            if (length(unique(y01)) >= 2L) {
              yf = factor(y01, levels = c(0L, 1L))
              return(mlr3measures::auc(yf, p, positive = "1"))
            }
            yf = factor(y01, levels = c(0L, 1L))
            b = mlr3measures::bbrier(yf, p, positive = "1")
            1 - b
          }

          expl_rows = list()
          k = 0L

          for (g in group_vars) {
            gg = Xg[[g]]
            levs = unique(gg)
            for (lv in levs) {
              idx = which(gg == lv)
              if (length(idx) < 10L) next

              Xsub = task$data(rows = pred$row_ids[idx], cols = task$feature_names)
              ysub = truth01[idx]
              psub = as.numeric(model$predict_newdata(Xsub)$prob[, pos])
              base = score_prob(ysub, psub)

              for (f in num_feats) {
                Xperm = data.table::as.data.table(Xsub)
                Xperm[[f]] = sample(Xperm[[f]])
                pperm = as.numeric(model$predict_newdata(Xperm)$prob[, pos])
                sperm = score_prob(ysub, pperm)

                k = k + 1L
                expl_rows[[k]] = data.table::data.table(
                  group_var = g,
                  group = as.character(lv),
                  feature = f,
                  importance = base - sperm
                )
              }
            }
          }

          if (length(expl_rows) > 0L) {
            subgroup_expl_stability = data.table::rbindlist(expl_rows, fill = TRUE)
          }
        }

        expl_stability_summary = if (!is.null(subgroup_expl_stability) && nrow(subgroup_expl_stability) > 0L) {
          subgroup_expl_stability[, .(
            importance_mean = mean(importance, na.rm = TRUE),
            importance_sd_across_groups = stats::sd(importance, na.rm = TRUE)
          ), by = feature][order(-importance_mean)]
        } else {
          data.table::data.table()
        }

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
            measurement_invariance_present = measurement_invariance_present,
            explanation_stability_available = nrow(expl_stability_summary) > 0L
          ),
          artifacts = list(
            subgroup = subgroup,
            subgroup_explanation_stability = subgroup_expl_stability,
            subgroup_explanation_stability_summary = expl_stability_summary
          ),
          messages = msgs
        ))
      }

      # Multiclass: provide basic group prevalence + logloss by group (OVR curves handled in Gate 3).
      prob = .autoiml_pred_prob_matrix(pred, task)
      truth = pred$truth

      subgroup = mlr3misc::map_dtr(group_vars, function(g) {
        gg = Xg[[g]]
        levs = unique(gg)
        mlr3misc::map_dtr(levs, function(lv) {
          idx = which(gg == lv)
          if (length(idx) < 5L) {
            return(NULL)
          }
          # Use mlr3measures directly for subgroup metrics
          data.table::data.table(
            group_var = g,
            group = as.character(lv),
            n = length(idx),
            logloss = mlr3measures::logloss(truth[idx], prob[idx, , drop = FALSE]),
            mbrier = mlr3measures::mbrier(truth[idx], prob[idx, , drop = FALSE])
          )
        }, .fill = TRUE)
      }, .fill = TRUE)

      status = "pass"
      summary = "Subgroup audit computed (multiclass logloss/mbrier by group)."

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = data.table::data.table(
          n_group_vars = length(group_vars),
          measurement_invariance_present = measurement_invariance_present,
          explanation_stability_available = FALSE
        ),
        artifacts = list(
          subgroup = subgroup,
          subgroup_explanation_stability = data.table::data.table(),
          subgroup_explanation_stability_summary = data.table::data.table()
        ),
        messages = c("Subgroup audit is descriptive: confirm whether measurement/comparability assumptions hold before attributing subgroup differences to substantive effects.")
      )
    }
  )
)
