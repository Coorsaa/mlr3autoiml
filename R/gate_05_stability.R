#' @title Gate 5: Stability and Robustness
#'
#' @description
#' Implements two complementary stability layers:
#' \itemize{
#'   \item Performance variability across resampling iterations (from Gate 1)
#'   \item Explanation stability via bootstrap permutation importance on the final model
#' }
#'
#' When features are correlated, the gate automatically groups them for joint
#' permutation to reduce attribution distortion.
#'
#' @name Gate5Stability
#' @keywords internal
NULL

Gate5Stability = R6::R6Class(
  "Gate5Stability",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G5",
        name = "Stability and robustness",
        pdr = "D"
      )
    },

    run = function(ctx) {
      task = ctx$task
      model = ctx$final_model

      if (is.null(model)) {
        return(GateResult$new(
          gate_id = self$id, gate_name = self$name, pdr = self$pdr,
          status = "fail",
          summary = "Gate 5 requires a trained final model from Gate 1.",
          metrics = NULL
        ))
      }

      stab = ctx$stability %??% list()
      .autoiml_assert_known_names(stab, c("B", "max_features", "grouping", "sanity_checks", "instability_rel_sd_warn"), "ctx$stability")

      B = as.integer(stab$B %??% 25L)
      max_features = as.integer(stab$max_features %??% 10L)
      grouping = stab$grouping %??% NULL
      sanity_checks = isTRUE(stab$sanity_checks %??% TRUE)
      instability_rel_sd_warn = as.numeric(stab$instability_rel_sd_warn %??% 0.75)

      set.seed(.autoiml_gate_seed(ctx, self$id))

      feat_all = task$feature_names
      ft = task$feature_types
      num_cols = intersect(ft[type %in% c("numeric", "integer"), id], feat_all)

      feat = num_cols
      if (length(feat) > max_features) {
        # crude but deterministic: highest variance numeric subset
        dat_num = task$data(cols = feat)
        v = vapply(dat_num, function(x) stats::var(as.numeric(x), na.rm = TRUE), numeric(1))
        feat = feat[order(v, decreasing = TRUE)]
        feat = feat[seq_len(max_features)]
      }

      if (length(feat) == 0L) {
        return(GateResult$new(
          gate_id = self$id, gate_name = self$name, pdr = self$pdr,
          status = "skip",
          summary = "No numeric features available for stability checks.",
          metrics = NULL
        ))
      }

      X_full = task$data(cols = feat_all)
      y_full = task$truth()

      # scorer (fixed once)
      scorer = private$primary_scorer(task)

      # baseline predictions precomputed ONCE (major speed-up)
      p_full = private$predict_numeric(model, X_full, task)

      # grouping support: list of groups (each is character vector of feature names)
      groups = NULL
      group_mode = FALSE
      if (!is.null(grouping)) {
        if (is.list(grouping)) {
          groups = grouping
          group_mode = TRUE
        } else if (is.character(grouping)) {
          # treat as grouping variable names -> single group
          groups = list(grouping)
          group_mode = TRUE
        }
      }

      # Auto-grouping under strong dependence (optional):
      # If the user did not provide cfg$grouping, cluster features by absolute correlation
      # and permute as groups + singletons. This reduces attribution distortion when
      # correlated features are present.
      auto_grouping = FALSE
      if (is.null(grouping)) {
        cor_threshold = ctx$structure$cor_threshold %??% 0.70

        ft = tryCatch(task$feature_types, error = function(e) NULL)
        num = if (!is.null(ft)) ft$id[ft$type %in% c("numeric", "integer")] else feat
        num = intersect(num, feat)

        if (length(num) >= 2L) {
          Xc = task$data(cols = num)
          cm = suppressWarnings(stats::cor(Xc, use = "pairwise.complete.obs"))
          if (is.matrix(cm)) {
            cm[!is.finite(cm)] = 0
            idx = which(abs(cm) >= cor_threshold & upper.tri(cm), arr.ind = TRUE)

            if (nrow(idx) > 0L) {
              nodes = num
              parent = seq_along(nodes)
              findp = function(i) {
                while (parent[i] != i) {
                  parent[i] <<- parent[parent[i]]
                  i = parent[i]
                }
                i
              }
              unionp = function(i, j) {
                ri = findp(i)
                rj = findp(j)
                if (ri != rj) parent[rj] <<- ri
              }

              for (k in seq_len(nrow(idx))) {
                unionp(idx[k, 1], idx[k, 2])
              }

              comp = split(nodes, vapply(seq_along(nodes), findp, integer(1)))
              comp = comp[vapply(comp, length, integer(1)) > 1L]

              if (length(comp) > 0L) {
                in_group = unique(unlist(comp))
                singles = setdiff(feat, in_group)
                groups = c(unname(comp), lapply(singles, c))
                group_mode = TRUE
                auto_grouping = TRUE
              }
            }
          }
        }
      }


      n_full = nrow(X_full)

      # Core function to compute one bootstrap iteration
      compute_bootstrap_iteration = function(b, seed_offset = 0L) {
        set.seed(seed_offset + b)
        idx = sample.int(n_full, size = n_full, replace = TRUE)

        X = X_full[idx, , drop = FALSE]
        y = y_full[idx]
        p0 = p_full[idx]

        base = scorer(y, p0)

        if (!group_mode) {
          p = length(feat)
          n = nrow(X)

          # replicate X once (n*p) and permute each feature within its block
          X_big = X[rep.int(seq_len(n), times = p), , drop = FALSE]

          for (k in seq_along(feat)) {
            f = feat[k]
            block = ((k - 1L) * n + 1L):(k * n)
            vals_perm = sample(X[[f]], size = n, replace = FALSE)
            if (inherits(X_big, "data.table")) {
              data.table::set(X_big, i = block, j = f, value = vals_perm)
            } else {
              X_big[block, f] = vals_perm
            }
          }

          p1_all = private$predict_numeric(model, X_big, task)
          p1_mat = matrix(p1_all, nrow = n, ncol = p)

          s1 = apply(p1_mat, 2, function(col) scorer(y, col))
          imp = vapply(s1, function(v) private$importance_delta(base, v, task), numeric(1))

          data.table::data.table(
            bootstrap = b,
            feature = feat,
            importance = as.numeric(imp)
          )
        } else {
          g = length(groups)
          n = nrow(X)

          X_big = X[rep.int(seq_len(n), times = g), , drop = FALSE]

          for (k in seq_len(g)) {
            block = ((k - 1L) * n + 1L):(k * n)

            # joint permutation index for the group (preserves within-group dependence)
            perm = sample.int(n, size = n, replace = FALSE)

            for (f in groups[[k]]) {
              if (!f %in% names(X_big)) next
              vals_perm = X[[f]][perm]
              if (inherits(X_big, "data.table")) {
                data.table::set(X_big, i = block, j = f, value = vals_perm)
              } else {
                X_big[block, f] = vals_perm
              }
            }
          }

          p1_all = private$predict_numeric(model, X_big, task)
          p1_mat = matrix(p1_all, nrow = n, ncol = g)

          s1 = apply(p1_mat, 2, function(col) scorer(y, col))
          imp = vapply(s1, function(v) private$importance_delta(base, v, task), numeric(1))

          group_names = vapply(groups, function(gr) paste(gr, collapse = "+"), character(1))
          data.table::data.table(
            bootstrap = b,
            feature = group_names,
            importance = as.numeric(imp)
          )
        }
      }

      # Check if future.apply is available for parallel execution
      # Note: Parallelization requires the package to be installed (not just loaded via
      # devtools), so that worker processes can load it.
      use_parallel = FALSE
      if (requireNamespace("future.apply", quietly = TRUE) &&
        requireNamespace("future", quietly = TRUE)) {
        plan_info = future::plan()
        is_sequential = inherits(plan_info, "sequential") ||
          identical(class(plan_info)[1L], "sequential")
        pkg_installed = "mlr3autoiml" %in% rownames(utils::installed.packages())
        if (!is_sequential && pkg_installed) {
          use_parallel = TRUE
        }
      }

      boot_seed = as.integer(ctx$seed %??% 1L)

      if (use_parallel) {
        # Parallel execution via future.apply
        imp_list = future.apply::future_lapply(
          seq_len(B),
          function(b) compute_bootstrap_iteration(b, boot_seed),
          future.seed = TRUE,
          future.packages = "mlr3autoiml"
        )
      } else {
        # Sequential fallback
        imp_list = lapply(seq_len(B), function(b) compute_bootstrap_iteration(b, boot_seed))
      }

      perm_imp = data.table::rbindlist(imp_list, fill = TRUE)
      perm_imp_summary = perm_imp[, .(
        importance_mean = mean(importance, na.rm = TRUE),
        importance_sd = stats::sd(importance, na.rm = TRUE)
      ), by = feature][order(-importance_mean)]

      # CI on mean importance (per feature)
      ci_dt = perm_imp[, {
        ci = .autoiml_ci_mean(importance, level = 0.95)
        list(
          mean = ci$mean,
          ci_low = ci$lower,
          ci_high = ci$upper,
          n = ci$n
        )
      }, by = feature][order(-mean)]

      # stability flag: high relative variability
      rel_sd = perm_imp_summary[, importance_sd / (abs(importance_mean) + 1e-8)]
      unstable = any(is.finite(rel_sd) & rel_sd > instability_rel_sd_warn)

      max_rel_sd = if (length(rel_sd) > 0L && any(is.finite(rel_sd))) max(rel_sd[is.finite(rel_sd)], na.rm = TRUE) else NA_real_
      stability_tier = if (!is.finite(max_rel_sd)) {
        "unknown"
      } else if (max_rel_sd <= 0.40) {
        "stable"
      } else if (max_rel_sd <= instability_rel_sd_warn) {
        "partially_stable"
      } else {
        "unstable"
      }

      sanity_dt = data.table::data.table(
        task_type = class(task)[1L],
        check = "label_randomization",
        baseline_score = NA_real_,
        randomized_score = NA_real_,
        sanity_pass = NA
      )

      if (isTRUE(sanity_checks)) {
        set.seed(.autoiml_gate_seed(ctx, paste0(self$id, "_sanity")))
        y_rand = sample(y_full)
        baseline_score = scorer(y_full, p_full)
        random_score = scorer(y_rand, p_full)

        sanity_pass = if (inherits(task, "TaskRegr")) {
          isTRUE(is.finite(random_score) && is.finite(baseline_score) && random_score >= baseline_score * 1.05)
        } else if (inherits(task, "TaskClassif") && length(task$class_names) == 2L) {
          isTRUE(is.finite(random_score) && random_score <= 0.60)
        } else {
          isTRUE(is.finite(random_score) && is.finite(baseline_score) && random_score <= baseline_score)
        }

        sanity_dt = data.table::data.table(
          task_type = class(task)[1L],
          check = "label_randomization",
          baseline_score = baseline_score,
          randomized_score = random_score,
          sanity_pass = sanity_pass
        )

        if (!isTRUE(sanity_pass)) {
          unstable = TRUE
        }
      }

      status = if (unstable) "warn" else "pass"
      summary = if (unstable) {
        "Stability concerns: explanation quantities and/or performance vary substantially under resampling/permutation; narrow claims or revise the feature pipeline."
      } else {
        "Permutation-based stability check suggests robust importance ordering under bootstrap perturbations."
      }

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = data.table::data.table(
          B = B,
          n_features = nrow(perm_imp_summary),
          any_unstable = unstable,
          max_rel_sd = max_rel_sd,
          stability_tier = stability_tier,
          sanity_checks = sanity_checks,
          sanity_pass = if (nrow(sanity_dt) > 0L) isTRUE(sanity_dt$sanity_pass[[1L]]) else NA
        ),
        artifacts = list(
          perm_importance = perm_imp_summary,
          perm_importance_raw = perm_imp,
          perf_ci = ci_dt,
          grouping_used = if (isTRUE(group_mode)) groups else NULL,
          grouping_auto = if (exists("auto_grouping")) auto_grouping else FALSE,
          sanity_check_result = sanity_dt
        ),
        messages = c(
          "Permutation stability is a heuristic: correlated features and strong model regularization can blur attributions."
        )
      )
    }
  ),

  private = list(

    primary_scorer = function(task) {
      if (inherits(task, "TaskRegr")) {
        # Direct computation: RMSE
        return(function(y, yhat) sqrt(mean((y - yhat)^2)))
      }
      if (inherits(task, "TaskClassif")) {
        if (length(task$class_names) == 2L) {
          pos = task$positive %??% task$class_names[2L]
          return(function(y, p) {
            y01 = factor(as.integer(y == pos), levels = c(0L, 1L))
            mlr3measures::auc(y01, p, positive = "1")
          })
        }
        # multiclass fallback: mean prob of true class (higher is better)
        return(function(y, p) {
          # Here p is numeric score already (one-vs-rest handled upstream if needed)
          mean(p, na.rm = TRUE)
        })
      }
      stop("Unsupported task type.", call. = FALSE)
    },

    importance_delta = function(base, perm, task) {
      # For AUC: lower is worse, so importance is base - perm
      # For RMSE: higher is worse, so importance is perm - base
      if (inherits(task, "TaskRegr")) {
        return(perm - base)
      }
      if (inherits(task, "TaskClassif") && length(task$class_names) == 2L) {
        return(base - perm)
      }
      return(base - perm)
    },

    predict_numeric = function(model, newdata, task) {
      pr = model$predict_newdata(newdata)
      .autoiml_pred_numeric(pr, task)
    }
  )
)
