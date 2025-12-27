#' Gate 4: Faithfulness to the model (local & surrogates)
#' @keywords internal
#' @import data.table
#' @noRd
Gate4Faithfulness = R6::R6Class(
  "Gate4Faithfulness",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G4",
        name = "Faithfulness to the model (local & surrogates)",
        pdr = "D"
      )
    },

    run = function(ctx) {
      task = ctx$task
      model = ctx$final_model
      if (is.null(model)) {
        model = ctx$learner$clone(deep = TRUE)
        model$train(task)
        ctx$final_model = model
      }

      # Fit a simple global surrogate to mimic model predictions
      X = task$data(cols = task$feature_names)
      pr = model$predict(task)
      yhat = .autoiml_pred_numeric(pr, task)

      df = data.table::as.data.table(X)
      df[, .yhat_model := yhat]

      # Use linear surrogate by default to keep dependencies minimal
      # (users can extend to rpart / rule lists via optional packages)
      formula = stats::as.formula(paste(".yhat_model ~", paste(task$feature_names, collapse = " + ")))
      fit = tryCatch(stats::lm(formula, data = df), error = function(e) NULL)

      if (is.null(fit)) {
        return(GateResult$new(
          gate_id = self$id, gate_name = self$name, pdr = self$pdr,
          status = "warn",
          summary = "Could not fit linear surrogate (likely due to non-numeric features); consider preprocessing via mlr3pipelines and/or using an alternative surrogate learner.",
          metrics = NULL
        ))
      }

      # fidelity: R^2 between surrogate predictions and model predictions
      yhat_sur = stats::predict(fit, newdata = df)
      r2 = 1 - sum((yhat - yhat_sur)^2, na.rm = TRUE) / sum((yhat - mean(yhat, na.rm = TRUE))^2, na.rm = TRUE)
      rmse = .autoiml_rmse(yhat, yhat_sur)

      status = "pass"
      summary = "Surrogate fidelity suggests explanations can be meaningfully interpreted as approximations to the fitted model."
      if (!is.finite(r2) || r2 < 0.8) {
        status = "warn"
        summary = "Low surrogate fidelity: global approximations may be misleading; prefer local checks and report fidelity metrics alongside explanations."
      }

      metrics = data.table::data.table(
        surrogate = "lm",
        fidelity_r2 = r2,
        fidelity_rmse = rmse,
        n = nrow(df)
      )

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = metrics,
        artifacts = list(surrogate_model = fit)
      )
    }
  )
)
