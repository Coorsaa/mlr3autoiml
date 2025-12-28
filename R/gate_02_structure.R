# FILE: R/gate_02_structure.R
#' Gate 2: What is being summarized? (dependence & interactions)
#'
#' Produces dependence-aware global structure artifacts, focusing on:
#' - Feature dependence screening (numeric correlation proxy)
#' - Global effect curves (PDP/ICE and ALE)
#' - Interaction screening (Friedman-Popescu H-statistic on a sampled grid)
#' - Optional regionalization of heterogeneous effects (lightweight GADGET-style)
#'
#' Notes:
#' - PDP/ICE and ALE are intervention-style, model-agnostic summaries; interpret
#'   cautiously under strong feature dependence and outside the data support.
#' - ALE is generally preferred under strong dependence (Apley & Zhu).
#' - PDP/ICE implementation uses an equidistant grid by default, mirroring 'iml'
#'   defaults; ALE uses a quantile grid (also consistent with 'iml').
#'
#' @keywords internal
#' @noRd
Gate2Structure = R6::R6Class(
  "Gate2Structure",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G2",
        name = "What is being summarized? (dependence & interactions)",
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

      cfg = ctx$structure %||% list()

      # --- computation budget knobs
      sample_n = as.integer(cfg$sample_n %||% 200L)
      max_features = as.integer(cfg$max_features %||% 5L)
      ice_keep_n = as.integer(cfg$ice_keep_n %||% 30L)

      # --- effect curves
      pd_grid_n = as.integer(cfg$grid_n %||% 10L)
      pd_grid_type = as.character(cfg$grid_type %||% "equidist")
      ice_center = as.character(cfg$ice_center %||% "anchor")
      ale_bins = as.integer(cfg$ale_bins %||% 10L)

      # --- dependence screen
      cor_threshold = as.numeric(cfg$cor_threshold %||% 0.70)

      # --- interaction screening
      hstat_max_features = as.integer(cfg$hstat_max_features %||% 4L)
      hstat_grid_n = as.integer(cfg$hstat_grid_n %||% 6L)
      hstat_threshold = as.numeric(cfg$hstat_threshold %||% 0.20)

      # --- regionalization (GADGET-style)
      regionalize = isTRUE(cfg$regionalize %||% FALSE)
      regional_method = as.character(cfg$regional_method %||% "auto")
      gadget_max_depth = as.integer(cfg$gadget_max_depth %||% 3L)
      gadget_min_bucket = as.integer(cfg$gadget_min_bucket %||% 30L)
      gadget_gamma = as.numeric(cfg$gadget_gamma %||% 0.00)
      gadget_top_k = as.integer(cfg$gadget_top_k %||% 2L)

      set.seed(ctx$seed %||% 1L)

      feat = task$feature_names
      ft = task$feature_types
      num_cols = intersect(ft[type %in% c("numeric", "integer"), id], feat)

      # (1) Dependence proxy: max absolute correlation among numeric features
      max_abs_cor = NA_real_
      cor_pairs = NULL
      if (length(num_cols) >= 2L) {
        dat_num = task$data(cols = num_cols)
        cm = tryCatch(stats::cor(dat_num, use = "pairwise.complete.obs"), error = function(e) NULL)
        if (!is.null(cm)) {
          cm[upper.tri(cm, diag = TRUE)] = NA_real_
          max_abs_cor = suppressWarnings(max(abs(cm), na.rm = TRUE))
          if (is.finite(max_abs_cor)) {
            idx = which(abs(cm) == max_abs_cor, arr.ind = TRUE)
            idx = idx[1L, , drop = FALSE]
            cor_pairs = data.table::data.table(
              var1 = rownames(cm)[idx[1, 1]],
              var2 = colnames(cm)[idx[1, 2]],
              abs_cor = max_abs_cor
            )
          }
        }
      }

      # Choose numeric features to analyze (highest variance)
      eval_feats = character(0)
      if (length(num_cols) > 0L) {
        dat_num_full = task$data(cols = num_cols)
        v = vapply(dat_num_full, function(x) stats::var(as.numeric(x), na.rm = TRUE), numeric(1))
        ord = order(v, decreasing = TRUE)
        eval_feats = num_cols[ord]
        if (length(eval_feats) > max_features) eval_feats <- eval_feats[seq_len(max_features)]
      }

      # Sample rows for IML computations
      sample_n = min(sample_n, task$nrow)
      rows = sample(task$row_ids, size = sample_n)
      X = task$data(rows = rows, cols = feat)

      # Choose classes to analyze
      class_labels = NULL
      if (inherits(task, "TaskClassif")) {
        if (length(task$class_names) == 2L) {
          class_labels = task$positive %||% task$class_names[2L]
        } else {
          tab = sort(table(task$truth()), decreasing = TRUE)
          class_labels = names(tab)[seq_len(min(3L, length(tab)))]
        }
      }

      # interaction screening helper (Friedman-Popescu H-statistic)
      compute_hstat_pair = function(Xs, f1, f2, class_label) {
        g1 = .autoiml_grid_1d_iml(Xs[[f1]], grid_n = hstat_grid_n, grid_type = "quantile")
        g2 = .autoiml_grid_1d_iml(Xs[[f2]], grid_n = hstat_grid_n, grid_type = "quantile")
        if (length(g1) < 2L || length(g2) < 2L) {
          return(NULL)
        }

        base = .autoiml_predict_score(model, Xs, task, class_of_interest = class_label)
        f0 = mean(base, na.rm = TRUE)

        pd1 = sapply(g1, function(v) {
          X2 = data.table::copy(Xs)
          X2 = .autoiml_set_feature_value(X2, task, f1, v)
          mean(.autoiml_predict_score(model, X2, task, class_of_interest = class_label), na.rm = TRUE)
        })

        pd2 = sapply(g2, function(v) {
          X2 = data.table::copy(Xs)
          X2 = .autoiml_set_feature_value(X2, task, f2, v)
          mean(.autoiml_predict_score(model, X2, task, class_of_interest = class_label), na.rm = TRUE)
        })

        pd12 = matrix(NA_real_, nrow = length(g1), ncol = length(g2))
        for (i in seq_along(g1)) {
          for (j in seq_along(g2)) {
            X2 = data.table::copy(Xs)
            X2 = .autoiml_set_feature_value(X2, task, f1, g1[[i]])
            X2 = .autoiml_set_feature_value(X2, task, f2, g2[[j]])
            pd12[i, j] = mean(.autoiml_predict_score(model, X2, task, class_of_interest = class_label), na.rm = TRUE)
          }
        }

        pd1c = pd1 - f0
        pd2c = pd2 - f0
        pd12c = pd12 - f0

        int = pd12c - outer(pd1c, rep(1, length(pd2c))) - outer(rep(1, length(pd1c)), pd2c)
        denom = sum(pd12c^2, na.rm = TRUE)
        num = sum(int^2, na.rm = TRUE)

        h2 = if (is.finite(denom) && denom > 0) num / denom else NA_real_
        h = sqrt(pmin(pmax(h2, 0), 1))

        data.table::data.table(
          class_label = as.character(class_label),
          feature1 = f1,
          feature2 = f2,
          hstat = as.numeric(h),
          grid_n = length(g1) * length(g2),
          sample_n = nrow(Xs)
        )
      }

      # --- main computations ------------------------------------------------
      pd_curves = NULL
      ice_curves = NULL
      ice_spread = NULL
      ale_curves = NULL
      hstats = NULL
      gadget = list()

      pred_mats = list()
      grids = list()

      if (length(eval_feats) > 0L) {
        # classification: loop over a small set of classes; regression: single pass
        cls_loop = if (is.null(class_labels)) list(NULL) else as.list(class_labels)

        for (cls in cls_loop) {
          for (f in eval_feats) {
            # PDP + ICE
            pdice = .autoiml_pdp_ice_1d(
              task = task,
              model = model,
              X = X,
              feature = f,
              grid_n = pd_grid_n,
              grid_type = pd_grid_type,
              class_label = cls,
              ice_keep_n = ice_keep_n,
              ice_center = ice_center
            )
            if (!is.null(pdice)) {
              key = paste0(if (is.null(cls)) "__regr__" else as.character(cls), "::", f)
              pred_mats[[key]] = pdice$pred_mat
              grids[[key]] = pdice$grid

              pd_curves = data.table::rbindlist(list(pd_curves, pdice$pd), fill = TRUE)

              # add row_id for ICE
              tmp_ice = pdice$ice
              tmp_ice[, row_id := rows[row_index]]
              tmp_ice[, row_index := NULL]
              ice_curves = data.table::rbindlist(list(ice_curves, tmp_ice), fill = TRUE)

              ice_spread = data.table::rbindlist(list(ice_spread, pdice$ice_spread), fill = TRUE)
            }

            # ALE
            ale_dt = .autoiml_ale_1d_iml(
              task = task,
              model = model,
              X = X,
              feature = f,
              bins = ale_bins,
              class_label = cls
            )
            if (!is.null(ale_dt) && nrow(ale_dt) > 0L) {
              ale_curves = data.table::rbindlist(list(ale_curves, ale_dt), fill = TRUE)
            }
          }
        }
        # Interaction screening (H-statistic) on a smaller feature subset
        if (!is.null(ice_spread) && nrow(ice_spread) > 0L) {
          # choose a primary class for screening (binary: positive; multiclass: most frequent)
          cls_main = if (is.null(class_labels)) NULL else as.character(class_labels[[1L]])
          tmp = ice_spread
          if (!is.null(cls_main)) {
            tmp = ice_spread[class_label == cls_main]
            if (nrow(tmp) == 0L) tmp <- ice_spread
          }
          top_feats = tmp[order(-ice_sd_mean)]$feature
          top_feats = unique(top_feats)
          top_feats = top_feats[seq_len(min(length(top_feats), hstat_max_features))]
        } else {
          top_feats = eval_feats[seq_len(min(length(eval_feats), hstat_max_features))]
          cls_main = if (is.null(class_labels)) NULL else as.character(class_labels[[1L]])
        }

        if (length(top_feats) >= 2L && !is.null(cls_main)) {
          pairs = utils::combn(top_feats, 2, simplify = FALSE)
          hlist = lapply(pairs, function(p) compute_hstat_pair(X, p[1], p[2], cls_main))
          hstats = data.table::rbindlist(hlist, fill = TRUE)
        }

        # Optional: GADGET-style regionalization if strong interaction signal
        if (regionalize && !is.null(hstats) && nrow(hstats) > 0L) {
          hs = hstats[is.finite(hstat)][order(-hstat)]
          if (nrow(hs) > 0L && isTRUE(hs$hstat[1L] >= hstat_threshold)) {
            cls_main = as.character(hs$class_label[1L])
            f1 = hs$feature1[1L]
            f2 = hs$feature2[1L]

            # Determine which effect curves should be regionalized
            rec_method = if (isTRUE(is.finite(max_abs_cor) && max_abs_cor >= cor_threshold)) "ale" else "pdp"
            regional_method_used = if (regional_method == "auto") rec_method else regional_method

            focus = unique(c(f1, f2))
            focus = focus[seq_len(min(length(focus), gadget_top_k))]

            for (ff in focus) {
              key = paste0(cls_main, "::", ff)
              M = pred_mats[[key]]
              g = grids[[key]]
              if (is.null(M) || is.null(g)) next

              # GADGET uses centered ICE matrices to target *heterogeneous effects*
              M_center = M # already centered according to ctx$structure$ice_center

              gad = .autoiml_gadget_regionalize_feature(
                X = X,
                row_ids = rows,
                feature = ff,
                grid = as.numeric(g),
                pred_mat = M_center,
                split_candidates = setdiff(names(X), ff),
                max_depth = gadget_max_depth,
                min_bucket = gadget_min_bucket,
                gamma = gadget_gamma
              )

              if (!is.null(gad)) {
                # annotate for downstream filtering / plotting
                if (!is.null(gad$regions) && data.table::is.data.table(gad$regions)) {
                  gad$regions[, class_label := cls_main]
                  gad$regions[, method := regional_method_used]
                }
                if (!is.null(gad$assignments) && data.table::is.data.table(gad$assignments)) {
                  gad$assignments[, class_label := cls_main]
                  gad$assignments[, method := regional_method_used]
                }

                gad$class_label = cls_main
                gad$method = regional_method_used
                gadget[[ff]] = gad
              }
            }
          }
        }
      }

      # --- gate decision ----------------------------------------------------
      dependence_flag = isTRUE(is.finite(max_abs_cor) && max_abs_cor >= cor_threshold)
      max_ice_sd = if (!is.null(ice_spread) && nrow(ice_spread) > 0L) max(ice_spread$ice_sd_mean, na.rm = TRUE) else NA_real_
      max_hstat = if (!is.null(hstats) && nrow(hstats) > 0L) max(hstats$hstat, na.rm = TRUE) else NA_real_

      interaction_flag = isTRUE(is.finite(max_hstat) && max_hstat >= hstat_threshold)

      recommended_effect_method = if (dependence_flag) "ale" else "pdp"

      status = "pass"
      summary = "Dependence/heterogeneity assessed; global effect curves computed (PDP/ICE + ALE)."
      if (dependence_flag || interaction_flag) {
        status = "warn"
        summary = sprintf(
          "Meaningful feature dependence and/or heterogeneity detected; prefer %s (with ICE spreads) over PDP and consider interaction-aware regionalization.",
          toupper(recommended_effect_method)
        )
      }

      recommendation = list(
        recommended_effect_method = recommended_effect_method,
        dependence_flag = dependence_flag,
        interaction_flag = interaction_flag,
        cor_threshold = cor_threshold,
        hstat_threshold = hstat_threshold,
        regionalize = regionalize,
        regional_method_used = if (regional_method == "auto") recommended_effect_method else regional_method
      )

      metrics = data.table::data.table(
        max_abs_cor = max_abs_cor,
        dependence_flag = dependence_flag,
        recommended_effect_method = recommended_effect_method,
        max_ice_sd_mean = max_ice_sd,
        max_hstat = max_hstat,
        interaction_flag = interaction_flag,
        analyzed_numeric_features = length(eval_feats),
        sample_n = sample_n
      )

      # Summarize all regionalization outputs (optional convenience table)
      gadget_regions = NULL
      if (!is.null(gadget) && length(gadget) > 0L) {
        grlist = lapply(names(gadget), function(ff) {
          one = gadget[[ff]]
          if (is.null(one) || is.null(one$regions) || nrow(one$regions) == 0L) {
            return(NULL)
          }
          dt = data.table::copy(one$regions)
          dt[, feature := ff]
          if (!"class_label" %in% names(dt) && !is.null(one$class_label)) {
            dt[, class_label := as.character(one$class_label)]
          }
          if (!"method" %in% names(dt) && !is.null(one$method)) {
            dt[, method := as.character(one$method)]
          }
          dt
        })
        gadget_regions = data.table::rbindlist(grlist, fill = TRUE)
      }

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = metrics,
        artifacts = list(
          recommendation = recommendation,
          max_cor_pair = cor_pairs,
          ice_spread = ice_spread,
          ice_curves = ice_curves,
          pd_curves = pd_curves,
          ale_curves = ale_curves,
          hstats = hstats,
          gadget = gadget,
          gadget_regions = gadget_regions
        ),
        messages = c(
          "PDP/ICE and ALE are model-agnostic but rely on intervention-style predictions; interpret with care under strong feature dependence and extrapolation.",
          "ICE spread is computed on centered ICE curves (cICE) to reflect heterogeneity of *effects* rather than baseline risk differences."
        )
      )
    }
  )
)
