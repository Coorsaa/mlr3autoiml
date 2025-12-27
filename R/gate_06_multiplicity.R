#' Gate 6: Multiplicity and transport
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

      # 6a) Multiplicity via alternative learners (optional)
      perf_tbl = NULL
      multiplicity_flag = NA
      if (!is.null(ctx$alt_learners) && is.list(ctx$alt_learners) && length(ctx$alt_learners) > 0L) {
        resampling = ctx$resampling$clone(deep = TRUE)
        resampling$instantiate(task)
        measures = ctx$measures %||% list(mlr3::msr("classif.auc"))

        primary = if (inherits(task, "TaskRegr")) "regr.rmse" else if (inherits(task, "TaskClassif") && length(task$class_names) == 2L) "classif.auc" else "classif.logloss"

        out = list()
        for (lr in ctx$alt_learners) {
          lr2 = lr$clone(deep = TRUE)
          if (inherits(task, "TaskClassif") && "prob" %in% lr2$predict_types) lr2$predict_type <- "prob"
          rr = mlr3::resample(task, lr2, resampling, store_models = FALSE)
          agg = rr$aggregate(measures)
          out[[lr$id]] = data.table::data.table(learner_id = lr$id, primary = primary, value = agg[[primary]])
        }
        perf_tbl = data.table::rbindlist(out, fill = TRUE)
        if (nrow(perf_tbl) > 1L) {
          best = if (primary %in% c("regr.rmse", "classif.logloss")) min(perf_tbl$value, na.rm = TRUE) else max(perf_tbl$value, na.rm = TRUE)
          tol = if (primary %in% c("regr.rmse", "classif.logloss")) best * 1.01 else best - 0.01
          near_tie = if (primary %in% c("regr.rmse", "classif.logloss")) perf_tbl$value <= tol else perf_tbl$value >= tol
          multiplicity_flag = sum(near_tie, na.rm = TRUE) >= 2L
        }
      }

      # 6b) Transportability via subgroup performance heterogeneity (using group role if present)
      transport_tbl = NULL
      transport_flag = NA
      group_col = task$col_roles$group %||% character()
      if (length(group_col) == 1L && !is.null(pred)) {
        groups = task$data(cols = group_col)[[group_col]]
        if (inherits(pred, "PredictionClassif") && length(task$class_names) == 2L) {
          pos = task$positive %||% task$class_names[2L]
          truth01 = as.integer(pred$truth == pos)
          p_hat = .autoiml_pred_numeric(pred, task)
          transport_tbl = data.table::data.table(group = groups, truth01 = truth01, p_hat = p_hat)[
            , .(n = .N, auc = .autoiml_auc(truth01, p_hat)), by = group
          ]
          rng = range(transport_tbl$auc, na.rm = TRUE)
          transport_flag = is.finite(rng[2] - rng[1]) && (rng[2] - rng[1]) > 0.10
        } else if (inherits(pred, "PredictionRegr")) {
          y = pred$truth
          yhat = pred$response
          transport_tbl = data.table::data.table(group = groups, y = y, yhat = yhat)[
            , .(n = .N, rmse = .autoiml_rmse(y, yhat)), by = group
          ]
          rng = range(transport_tbl$rmse, na.rm = TRUE)
          transport_flag = is.finite(rng[2] - rng[1]) && (rng[2] - rng[1]) > 0.10 * median(transport_tbl$rmse, na.rm = TRUE)
        }
      }

      status = "pass"
      summary = "Multiplicity and transport checks did not raise major concerns (given available evidence)."

      if (isTRUE(multiplicity_flag) || isTRUE(transport_flag)) {
        status = "warn"
        summary = "Evidence of multiplicity (near-tie models) and/or limited transportability across groups; scope interpretive claims to validated model(s) and populations."
      }

      if (is.null(perf_tbl) && is.null(transport_tbl)) {
        status = "skip"
        summary = "Multiplicity/transport not assessed (provide ctx$alt_learners and/or set a Task group role)."
      }

      metrics = data.table::data.table(
        multiplicity_flag = multiplicity_flag,
        transport_flag = transport_flag
      )

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = metrics,
        artifacts = list(alt_learner_performance = perf_tbl, group_performance = transport_tbl)
      )
    }
  )
)
