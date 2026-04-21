# FILE: R/gate_04_faithfulness.R

#' @title Gate 4: Faithfulness (Surrogates, Local Accuracy, Local Interactions)
#'
#' @description
#' Screens whether the model's behavior is reasonably captured by:
#' \itemize{
#'   \item Simple global surrogates (linear model R-squared)
#'   \item Additive, case-level attributions (SHAP local accuracy checks)
#' }
#'
#' The gate intentionally keeps diagnostics lightweight; it is not a proof of
#' faithfulness. If strong interactions or heterogeneous effects are present,
#' prefer region-wise explanations and interaction-aware tools.
#'
#' @section Modes:
#' SHAP mode defaults to conditional (on-manifold) for within_support semantics
#' and marginal for marginal model query semantics.
#'
#' @name Gate4Faithfulness
#' @keywords internal
NULL

Gate4Faithfulness = R6::R6Class(
  "Gate4Faithfulness",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(id = "G4", name = "Faithfulness", pdr = "D")
    },

    run = function(ctx) {
      task = ctx$task
      model = ctx$final_model
      profile = ctx$profile %||% "standard"

      if (is.null(model)) {
        return(GateResult$new(
          gate_id = self$id,
          gate_name = self$name,
          pdr = self$pdr,
          status = "fail",
          summary = "Gate 4 requires a trained final model from Gate 1.",
          metrics = NULL
        ))
      }

      cfg = ctx$faithfulness %||% list()
      if (!is.list(cfg)) cfg <- list()
      .autoiml_assert_known_names(
        cfg,
        c(
          "r2_warn", "local_n", "shap_sample_size", "shap_background_n",
          "interaction_check", "interaction_delta_sd", "interaction_strength_warn",
          "shap_abs_error_warn"
        ),
        "ctx$faithfulness"
      )

      # ---- Outputs ---------------------------------------------------------
      status = "pass"
      messages = character()

      # Semantics from Gate 0A (if present)
      semantics = .autoiml_normalize_semantics((ctx$claim %||% list())$semantics %||% "within_support", default = "within_support")

      if (identical(semantics, "causal_recourse")) {
        return(GateResult$new(
          gate_id = self$id, gate_name = self$name, pdr = self$pdr,
          status = "skip",
          summary = "Causal/recourse semantics requested: Gate 4 is skipped (hard stop). Use a causal explanation workflow before evaluating additive attributions.",
          metrics = data.table::data.table(semantics = semantics),
          artifacts = list(),
          messages = c("Hard stop: causal/recourse requires explicit causal assumptions and identification; AutoIML does not implement this.")
        ))
      }

      # Gate 4 is only required when the claim includes local/regional explanation
      # or decision-support use. For purely global descriptive claims, skip.
      claim_scopes = ((ctx$claim %||% list())$claims %||% list())
      need_local = isTRUE(claim_scopes$local %||% FALSE)
      need_decision = isTRUE(claim_scopes$decision %||% FALSE)
      if (!isTRUE(need_local || need_decision)) {
        return(GateResult$new(
          gate_id = self$id, gate_name = self$name, pdr = self$pdr,
          status = "skip",
          summary = "No local/regional or decision-support claim requested; skipping faithfulness diagnostics.",
          metrics = data.table::data.table(semantics = semantics, skipped = TRUE),
          artifacts = list(),
          messages = "Gate 4 can be re-enabled by setting ctx$claim$claims$local=TRUE and/or ctx$claim$claims$decision=TRUE."
        ))
      }

      # SHAP mode defaults to conditional (on-manifold) for within_support semantics and marginal otherwise.
      shap_cfg = ctx$shap %||% list()
      if (!is.list(shap_cfg)) shap_cfg <- list()

      shap_mode = .autoiml_normalize_shap_mode(
        shap_cfg$mode %||% if (identical(semantics, "within_support")) "conditional" else "marginal",
        default = if (identical(semantics, "within_support")) "conditional" else "marginal"
      )

      cond_k = as.integer(shap_cfg$conditional_k %||% 5L)
      cond_k = max(1L, cond_k)
      cond_weighted = isTRUE(shap_cfg$conditional_weighted %||% TRUE)

      # ---- Data ------------------------------------------------------------
      X = data.table::as.data.table(task$data(cols = task$feature_names))
      n = nrow(X)

      # Select a numeric score to surrogate-fit / attribute.
      pred = ctx$pred %||% model$predict(task)

      yhat_mat = NULL
      yhat_vec = NULL
      score_name = "response"

      if (inherits(task, "TaskRegr")) {
        yhat_vec = as.numeric(pred$response)
        score_name = "response"
      } else if (inherits(task, "TaskClassif")) {
        prob = pred$prob
        if (is.null(prob)) {
          stop("Gate 4 requires probability predictions for classification. Set predict_type='prob'.", call. = FALSE)
        }
        yhat_mat = as.matrix(prob)
        if (is.null(colnames(yhat_mat))) colnames(yhat_mat) = task$class_names

        # Choose class to evaluate: claim decision_spec$positive_class > task$positive > first class.
        pos = ((ctx$claim %||% list())$decision_spec %||% list())$positive_class %||% task$positive %||% colnames(yhat_mat)[1L]
        pos = as.character(pos)[1L]
        if (is.na(pos) || !pos %in% colnames(yhat_mat)) pos = colnames(yhat_mat)[1L]

        yhat_vec = as.numeric(yhat_mat[, pos])
        score_name = paste0("prob_", pos)
      } else {
        stop("Unsupported task type in Gate 4.", call. = FALSE)
      }

      # ---- Global surrogate fidelity --------------------------------------
      # Simple linear surrogate (fast, interpretable). This is *diagnostic* only.
      complete_rows = stats::complete.cases(X)
      Xc = X[complete_rows]
      yhat_c = yhat_vec[complete_rows]

      surrogate_r2 = NA_real_
      surrogate_rmse = NA_real_

      if (nrow(Xc) >= 20L) {
        df = data.table::as.data.table(Xc)
        df$yhat = yhat_c

        # Use a plain LM. For factors, LM will expand via contrasts.
        fit = stats::lm(yhat ~ ., data = df)

        yhat_hat = stats::predict(fit, newdata = df)
        ss_res = sum((yhat_c - yhat_hat)^2, na.rm = TRUE)
        ss_tot = sum((yhat_c - mean(yhat_c, na.rm = TRUE))^2, na.rm = TRUE)

        surrogate_r2 = 1 - ss_res / ss_tot
        surrogate_rmse = sqrt(mean((yhat_c - yhat_hat)^2, na.rm = TRUE))
      }

      # Heuristics: if global surrogate is poor, warn that global linear narratives are risky.
      r2_warn = as.numeric(cfg$r2_warn %||% 0.70)
      if (is.finite(surrogate_r2) && surrogate_r2 < r2_warn) {
        status = "warn"
        messages = c(messages, sprintf("Low global surrogate R^2=%.2f (< %.2f): avoid oversimplified global narratives; interactions/nonlinearities likely matter.", surrogate_r2, r2_warn))
      }

      # ---- Local Shapley accuracy check -----------------------------------
      # For additive explanations, verify local accuracy: baseline + sum(phi) ~= f(x).
      local_n = as.integer(cfg$local_n %||% if (identical(profile, "fast")) 3L else 5L)
      local_n = max(1L, local_n)

      shap_sample_size = as.integer(cfg$shap_sample_size %||% (ctx$shap$sample_size %||% if (identical(profile, "fast")) 40L else 80L))
      shap_sample_size = max(10L, shap_sample_size)

      background_n = as.integer(cfg$shap_background_n %||% (ctx$shap$background_n %||% if (identical(profile, "fast")) 50L else 200L))
      background_n = max(20L, background_n)

      # Select rows for local checks
      rid = if (!is.null(ctx$row_ids)) ctx$row_ids else seq_len(n)
      rid = as.integer(rid)
      rid = rid[rid >= 1L & rid <= n]
      if (length(rid) < 1L) rid = seq_len(n)

      set.seed(.autoiml_gate_seed(ctx, self$id))
      check_rows = sample(rid, size = min(local_n, length(rid)))

      # Background sample for baseline expectations
      bg_rows = sample(rid, size = min(background_n, length(rid)))
      bg = task$data(rows = bg_rows, cols = task$feature_names)

      # Baseline expectation
      pred_bg = .autoiml_predict_matrix(task, model, bg)
      if (score_name == "response") {
        baseline = mean(as.numeric(pred_bg[, 1L]), na.rm = TRUE)
        cls_keep = NULL
      } else {
        # For classification, baseline on the selected class column.
        cls = sub("^prob_", "", score_name)
        cls_keep = cls
        baseline = mean(as.numeric(pred_bg[, cls]), na.rm = TRUE)
      }

      shap_check = data.table::data.table()
      interaction_check = data.table::data.table()

      cast_like_feature = function(values, template) {
        if (is.integer(template)) {
          return(as.integer(round(values)))
        }
        if (is.numeric(template)) {
          return(as.numeric(values))
        }
        values
      }

      for (r in check_rows) {
        x0 = task$data(rows = r, cols = task$feature_names)

        # Shapley values (interventional design). We compute for the chosen class only.
        cls_labels = if (is.null(cls_keep)) NULL else cls_keep
        phi = .autoiml_shapley_iml(
          task = task,
          model = model,
          x_interest = x0,
          background = bg,
          sample_size = shap_sample_size,
          seed = (ctx$seed %||% 1L) + r,
          class_labels = cls_labels,
          mode = shap_mode,
          conditional_k = cond_k,
          conditional_weighted = cond_weighted
        )

        if (nrow(phi) < 1L) next

        # Prediction at x0
        pred_x0 = .autoiml_predict_matrix(task, model, x0)
        fx0 = if (is.null(cls_keep)) {
          as.numeric(pred_x0[, 1L])
        } else {
          as.numeric(pred_x0[, cls_keep])
        }

        phi_sum = sum(phi$phi, na.rm = TRUE)
        recon = baseline + phi_sum
        err = abs(recon - fx0)

        shap_check = data.table::rbindlist(list(
          shap_check,
          data.table::data.table(
            row_id = r,
            score = score_name,
            fx = fx0,
            baseline = baseline,
            phi_sum = phi_sum,
            recon = recon,
            abs_error = err,
            sample_size = shap_sample_size
          )
        ), fill = TRUE)

        # ---- Local interaction screening (2x2 slice for top-2 numeric features) ----
        if (isTRUE(cfg$interaction_check %||% TRUE)) {
          # Choose top-2 features by |phi|
          top = phi[order(-abs(phi))]

          # Prefer numeric features for perturbation
          cand = intersect(top$feature, task$feature_names)
          cand = cand[cand %in% names(X)]
          if (length(cand) > 0L) {
            is_num = vapply(X[, ..cand], is.numeric, logical(1L))
            cand = cand[is_num]
          }
          num_feats = cand
          if (length(num_feats) >= 2L) {
            f1 = num_feats[1L]
            f2 = num_feats[2L]

            # Perturbations: +/- delta * sd (clamped to observed range)
            delta_sd = as.numeric(cfg$interaction_delta_sd %||% 0.25)
            s1 = stats::sd(X[[f1]], na.rm = TRUE)
            if (!is.finite(s1) || s1 <= 0) s1 = 0
            s2 = stats::sd(X[[f2]], na.rm = TRUE)
            if (!is.finite(s2) || s2 <= 0) s2 = 0

            x1 = as.numeric(x0[[f1]])
            x2 = as.numeric(x0[[f2]])
            lo1 = max(min(X[[f1]], na.rm = TRUE), x1 - delta_sd * s1)
            hi1 = min(max(X[[f1]], na.rm = TRUE), x1 + delta_sd * s1)
            lo2 = max(min(X[[f2]], na.rm = TRUE), x2 - delta_sd * s2)
            hi2 = min(max(X[[f2]], na.rm = TRUE), x2 + delta_sd * s2)

            # If no variation possible (sd=0 or clamped), skip.
            if (is.finite(lo1) && is.finite(hi1) && is.finite(lo2) && is.finite(hi2) && (lo1 != hi1) && (lo2 != hi2)) {
              grid = data.table::rbindlist(list(
                data.table::copy(data.table::as.data.table(x0)),
                data.table::copy(data.table::as.data.table(x0)),
                data.table::copy(data.table::as.data.table(x0)),
                data.table::copy(data.table::as.data.table(x0))
              ))

              # (lo1, lo2), (hi1, lo2), (lo1, hi2), (hi1, hi2)
              grid[[f1]] = cast_like_feature(c(lo1, hi1, lo1, hi1), X[[f1]])
              grid[[f2]] = cast_like_feature(c(lo2, lo2, hi2, hi2), X[[f2]])

              pred_grid = .autoiml_predict_matrix(task, model, grid)
              fg = if (is.null(cls_keep)) as.numeric(pred_grid[, 1L]) else as.numeric(pred_grid[, cls_keep])

              # Difference-in-differences interaction measure
              int = (fg[4L] - fg[3L]) - (fg[2L] - fg[1L])
              denom = abs(fg[4L] - fg[1L]) + 1e-8
              strength = abs(int) / denom

              interaction_check = data.table::rbindlist(list(
                interaction_check,
                data.table::data.table(
                  row_id = r,
                  score = score_name,
                  f1 = f1,
                  f2 = f2,
                  lo1 = lo1,
                  hi1 = hi1,
                  lo2 = lo2,
                  hi2 = hi2,
                  interaction = int,
                  strength = strength
                )
              ), fill = TRUE)
            }
          }
        }
      }

      shap_mae = if (nrow(shap_check) > 0L) mean(shap_check$abs_error, na.rm = TRUE) else NA_real_
      shap_max = if (nrow(shap_check) > 0L) max(shap_check$abs_error, na.rm = TRUE) else NA_real_

      if (nrow(shap_check) < 1L) {
        status = "fail"
        messages = c(messages, "No local Shapley diagnostics were produced; Gate 4 evidence is insufficient for local/decision claims.")
      }

      # Warn if local accuracy is poor (usually indicates insufficient MC sample size or unstable model predictions).
      shap_warn = as.numeric(cfg$shap_abs_error_warn %||% if (inherits(task, "TaskClassif")) 0.05 else 0.10)
      if (is.finite(shap_max) && shap_max > shap_warn) {
        status = "warn"
        messages = c(messages, sprintf("Local Shapley reconstruction error is high (max=%.3f > %.3f). Increase shap_sample_size/background_n or avoid additive narratives.", shap_max, shap_warn))
      }

      # Local interaction strength warning
      int_mean = if (nrow(interaction_check) > 0L) mean(interaction_check$strength, na.rm = TRUE) else NA_real_
      int_max = if (nrow(interaction_check) > 0L) max(interaction_check$strength, na.rm = TRUE) else NA_real_
      int_warn = as.numeric(cfg$interaction_strength_warn %||% 0.20)
      if (is.finite(int_max) && int_max > int_warn) {
        status = "warn"
        messages = c(messages, sprintf("Local interaction screening suggests non-additivity (max strength=%.2f > %.2f). Prefer region-wise explanations or interaction-aware tools.", int_max, int_warn))
      }

      # Semantics reminder: attribution mode and interpretation.
      if (identical(semantics, "within_support") && identical(shap_mode, "marginal")) {
        messages = c(messages, "Note: claim semantics are within_support/on-manifold, but SHAP was computed in marginal mode; interpret as marginal model query attributions and check off-support risk.")
      }
      if (identical(shap_mode, "conditional")) {
        messages = c(messages, "Conditional SHAP uses a kNN conditional sampler to approximate on-manifold attributions; it does not establish causality.")
      }

      metrics = data.table::data.table(
        semantics = semantics,
        shap_mode = shap_mode,
        score = score_name,
        surrogate_r2 = surrogate_r2,
        surrogate_rmse = surrogate_rmse,
        shap_mae = shap_mae,
        shap_max_abs_error = shap_max,
        local_interaction_mean = int_mean,
        local_interaction_max = int_max
      )

      perturbation_design = data.table::data.table(
        semantics = semantics,
        shap_mode = shap_mode,
        background_n = nrow(bg),
        local_n = local_n,
        shap_sample_size = shap_sample_size,
        conditional_k = cond_k,
        conditional_weighted = cond_weighted,
        interaction_check = isTRUE(cfg$interaction_check %||% TRUE),
        interaction_delta_sd = as.numeric(cfg$interaction_delta_sd %||% 0.25)
      )

      faithfulness_summary = data.table::data.table(
        semantics = semantics,
        shap_mode = shap_mode,
        score = score_name,
        n_local_checks = nrow(shap_check),
        n_interaction_checks = nrow(interaction_check),
        surrogate_r2 = surrogate_r2,
        surrogate_rmse = surrogate_rmse,
        shap_mae = shap_mae,
        shap_max_abs_error = shap_max,
        local_interaction_mean = int_mean,
        local_interaction_max = int_max
      )

      surrogate_spec = data.table::data.table(
        surrogate_family = "linear_model",
        n_complete_rows = nrow(Xc),
        n_features = length(task$feature_names),
        surrogate_r2 = surrogate_r2,
        surrogate_rmse = surrogate_rmse
      )

      summary = sprintf("Faithfulness screened via linear surrogate (R^2=%.2f) and local additive checks (Shapley max error=%.3f).", surrogate_r2, shap_max)

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = metrics,
        artifacts = list(
          shap_check = shap_check,
          faithfulness_summary = faithfulness_summary,
          local_interaction_check = interaction_check,
          perturbation_design = perturbation_design,
          surrogate_spec = surrogate_spec
        ),
        messages = c(
          sprintf("Shapley mode used: %s (conditional ~= on-manifold approximation; marginal ~= interventional what-if).", shap_mode),

          "Faithfulness checks are diagnostics, not guarantees: they can miss complex feature interactions and distribution shift.",
          messages
        )
      )
    }
  )
)
