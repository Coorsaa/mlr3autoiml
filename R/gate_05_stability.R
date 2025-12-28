#' Gate 5: Stability and robustness [D]
#'
#' Implements two complementary stability layers:
#' (a) performance variability across resampling iterations (from Gate 1),
#' (b) explanation stability via bootstrap permutation importance on the final model.
#'
#' @keywords internal
#' @import data.table
#' @noRd
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
          status = "skip",
          summary = "No trained final model found; run Gate 1 first.",
          metrics = NULL
        ))
      }

      stab = ctx$stability %||% list()
      .autoiml_assert_known_names(stab, c("B", "max_features", "grouping"), "ctx$stability")

      B = as.integer(stab$B %||% 25L)
      max_features = as.integer(stab$max_features %||% 10L)
      grouping = stab$grouping %||% NULL

      set.seed(ctx$seed %||% 1L)

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
        cor_threshold = ctx$structure$cor_threshold %||% 0.70

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


      imp_list = vector("list", B)

      n_full = nrow(X_full)

      for (b in seq_len(B)) {
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

          imp_dt = data.table::data.table(
            bootstrap = b,
            feature = feat,
            importance = as.numeric(imp)
          )
          imp_list[[b]] = imp_dt
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
          imp_dt = data.table::data.table(
            bootstrap = b,
            feature = group_names,
            importance = as.numeric(imp)
          )
          imp_list[[b]] = imp_dt
        }
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
      unstable = any(is.finite(rel_sd) & rel_sd > 0.75)

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
          any_unstable = unstable
        ),
        artifacts = list(
          perm_importance = perm_imp_summary,
          perm_importance_raw = perm_imp,
          perf_ci = ci_dt,
          grouping_used = if (isTRUE(group_mode)) groups else NULL,
          grouping_auto = if (exists("auto_grouping")) auto_grouping else FALSE
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
        return(function(y, yhat) .autoiml_rmse(y, yhat))
      }
      if (inherits(task, "TaskClassif")) {
        if (length(task$class_names) == 2L) {
          pos = task$positive %||% task$class_names[2L]
          return(function(y, p) {
            y01 = as.integer(y == pos)
            .autoiml_auc(y01, p)
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
