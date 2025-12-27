#' Gate 2: What is being summarized? (dependence & interactions)
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
      .autoiml_assert_known_names(
        cfg,
        allowed = c(
          "sample_n", "max_features", "ice_keep_n",
          "grid_n", "grid_type",
          "ice_center",
          "ale_bins", "trim",
          "cor_threshold",
          "hstat_max_features", "hstat_grid_n", "hstat_threshold",
          "regionalize", "regional_method",
          "gadget_max_depth", "gadget_min_bucket", "gadget_gamma",
          "gadget_n_thresholds", "gadget_top_k"
        ),
        where = "ctx$structure"
      )

      sample_n = as.integer(cfg$sample_n %||% 200L)
      max_features = as.integer(cfg$max_features %||% 5L)
      ice_keep_n = as.integer(cfg$ice_keep_n %||% 30L)
      grid_n = as.integer(cfg$grid_n %||% 10L)
      grid_type = as.character(cfg$grid_type %||% "equidist")
      ice_center = as.character(cfg$ice_center %||% "mean")
      ale_bins = as.integer(cfg$ale_bins %||% 10L)
      trim = cfg$trim %||% c(0.05, 0.95)
      cor_threshold = as.numeric(cfg$cor_threshold %||% 0.7)

      hstat_max_features = as.integer(cfg$hstat_max_features %||% 4L)
      hstat_grid_n = as.integer(cfg$hstat_grid_n %||% 6L)
      hstat_threshold = as.numeric(cfg$hstat_threshold %||% 0.20)

      regionalize = isTRUE(cfg$regionalize %||% FALSE)
      regional_method = as.character(cfg$regional_method %||% "auto")
      gadget_max_depth = as.integer(cfg$gadget_max_depth %||% 3L)
      gadget_min_bucket = as.integer(cfg$gadget_min_bucket %||% 30L)
      gadget_gamma = as.numeric(cfg$gadget_gamma %||% 0)
      gadget_n_thresholds = as.integer(cfg$gadget_n_thresholds %||% 5L)
      gadget_top_k = as.integer(cfg$gadget_top_k %||% 1L)

      set.seed(ctx$seed %||% 1L)

      feat = task$feature_names
      ft = task$feature_types
      num_cols = intersect(ft[type %in% c("numeric", "integer"), id], feat)

      # (1) dependence proxy: max absolute correlation among numeric features
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

      # choose numeric features by variance (full data)
      eval_feats = character(0)
      if (length(num_cols) > 0L) {
        dat_num_full = task$data(cols = num_cols)
        v = vapply(dat_num_full, function(x) stats::var(as.numeric(x), na.rm = TRUE), numeric(1))
        ord = order(v, decreasing = TRUE)
        eval_feats = num_cols[ord]
        if (length(eval_feats) > max_features) eval_feats = eval_feats[seq_len(max_features)]
      }

      sample_n = min(sample_n, task$nrow)
      rows = sample(task$row_ids, size = sample_n)
      X = task$data(rows = rows, cols = feat)

      # class labels for classif
      class_labels = NULL
      if (inherits(task, "TaskClassif")) {
        if (length(task$class_names) == 2L) {
          class_labels = task$class_names
        } else {
          tab = sort(table(task$truth()), decreasing = TRUE)
          class_labels = names(tab)[seq_len(min(3L, length(tab)))]
        }
      }

      # helpers --------------------------------------------------------------
      compute_hstat_pair = function(Xs, f1, f2, class_label) {
        g1 = .autoiml_grid_1d_iml(Xs[[f1]], grid_n = hstat_grid_n, grid_type = "quantile", trim = trim)
        g2 = .autoiml_grid_1d_iml(Xs[[f2]], grid_n = hstat_grid_n, grid_type = "quantile", trim = trim)
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
            X2 = .autoiml_set_feature_value(X2, task, f1, g1[i])
            X2 = .autoiml_set_feature_value(X2, task, f2, g2[j])
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

      # main computations ----------------------------------------------------
      pd_curves = NULL
      ice_curves = NULL
      ice_spread = NULL
      ale_curves = NULL
      hstats = NULL
      gadget_results = list()
      gadget_regions = list()

      # store centered ICE matrices for gadget
      pred_mats_centered = list()
      grids = list()

      if (length(eval_feats) > 0L) {
        for (f in eval_feats) {
          pdice = .autoiml_pdp_ice_1d(
            task = task,
            model = model,
            X = X,
            feature = f,
            grid_n = grid_n,
            grid_type = grid_type,
            class_labels = class_labels,
            ice_keep_n = ice_keep_n,
            ice_center = ice_center,
            seed = ctx$seed %||% 1L
          )

          if (!is.null(pdice$pd) && nrow(pdice$pd) > 0L) {
            pd_curves = data.table::rbindlist(list(pd_curves, pdice$pd), fill = TRUE)
          }
          if (!is.null(pdice$ice) && nrow(pdice$ice) > 0L) {
            ice_dt = data.table::copy(pdice$ice)
            ice_dt[, row_id := rows[row_index]]
            ice_dt[, row_index := NULL]
            ice_curves = data.table::rbindlist(list(ice_curves, ice_dt), fill = TRUE)
          }
          if (!is.null(pdice$ice_spread) && nrow(pdice$ice_spread) > 0L) {
            ice_spread = data.table::rbindlist(list(ice_spread, pdice$ice_spread), fill = TRUE)
          }

          if (!is.null(pdice$pred_mats_centered) && length(pdice$pred_mats_centered) > 0L) {
            for (lab in names(pdice$pred_mats_centered)) {
              key_lab = if (identical(lab, "response")) "__regr__" else as.character(lab)
              key = paste0(key_lab, "::", f)
              pred_mats_centered[[key]] = pdice$pred_mats_centered[[lab]]
              grids[[key]] = pdice$grid
            }
          }

          ale_dt = .autoiml_ale_1d_iml(
            task = task,
            model = model,
            X = X,
            feature = f,
            bins = ale_bins,
            trim = trim,
            class_labels = class_labels,
            seed = ctx$seed %||% 1L
          )
          if (!is.null(ale_dt) && nrow(ale_dt) > 0L) {
            ale_curves = data.table::rbindlist(list(ale_curves, ale_dt), fill = TRUE)
          }
        }

        # Interaction screening via H-statistic on top ICE-spread features
        if (!is.null(ice_spread) && nrow(ice_spread) > 0L && !is.null(class_labels)) {
          cls_main = class_labels[1L]
          tmp = ice_spread[class_label == cls_main]
          if (nrow(tmp) == 0L) tmp = ice_spread
          top_feats = unique(tmp[order(-ice_sd_mean)]$feature)
          top_feats = top_feats[seq_len(min(length(top_feats), hstat_max_features))]
        } else {
          top_feats = eval_feats[seq_len(min(length(eval_feats), hstat_max_features))]
          cls_main = if (!is.null(class_labels)) class_labels[1L] else NULL
        }

        if (!is.null(cls_main) && length(top_feats) >= 2L) {
          pairs = utils::combn(top_feats, 2, simplify = FALSE)
          hlist = lapply(pairs, function(p) compute_hstat_pair(X, p[1], p[2], cls_main))
          hstats = data.table::rbindlist(hlist, fill = TRUE)
        }

        # Optional: GADGET regionalization (centered ICE) for strongest interaction
        if (regionalize && !is.null(hstats) && nrow(hstats) > 0L) {
          hs = hstats[is.finite(hstat)][order(-hstat)]
          if (nrow(hs) > 0L) {
            hs = hs[seq_len(min(nrow(hs), gadget_top_k))]
            for (rr in seq_len(nrow(hs))) {
              top = hs[rr]
              if (!isTRUE(top$hstat >= hstat_threshold)) next

              f_focus = top$feature1
              cls_main = top$class_label

              key = paste0(cls_main, "::", f_focus)
              M = pred_mats_centered[[key]]
              g = grids[[key]]

              if (is.null(M) || is.null(g)) next

              gad = .autoiml_gadget_regionalize_feature(
                X = data.table::as.data.table(X),
                row_ids = rows,
                feature = f_focus,
                grid = g,
                pred_mat = M,
                split_candidates = setdiff(names(X), f_focus),
                max_depth = gadget_max_depth,
                min_bucket = gadget_min_bucket,
                gamma = gadget_gamma,
                n_thresholds = gadget_n_thresholds,
                seed = ctx$seed %||% 1L
              )

              gadget_results[[paste0(cls_main, "::", f_focus)]] = gad
              gadget_regions[[paste0(cls_main, "::", f_focus)]] = gad$regions
            }
          }
        }
      }

      dependence_flag = isTRUE(is.finite(max_abs_cor) && max_abs_cor >= cor_threshold)
      max_ice_sd = if (!is.null(ice_spread) && nrow(ice_spread) > 0L) max(ice_spread$ice_sd_mean, na.rm = TRUE) else NA_real_
      max_hstat = if (!is.null(hstats) && nrow(hstats) > 0L) max(hstats$hstat, na.rm = TRUE) else NA_real_
      interaction_flag = isTRUE(is.finite(max_hstat) && max_hstat >= hstat_threshold)

      recommended_method = if (dependence_flag) "ale" else "pd"

      status = "pass"
      summary = "Dependence/heterogeneity assessed; use dependence-aware global summaries when needed."
      if (dependence_flag || interaction_flag) {
        status = "warn"
        summary = "Meaningful feature dependence and/or heterogeneity detected; prefer ALE (with ICE spreads) over PDP and consider interaction-aware regionalization."
      }

      metrics = data.table::data.table(
        max_abs_cor = max_abs_cor,
        dependence_flag = dependence_flag,
        recommended_effect_method = recommended_method,
        max_ice_sd_mean = max_ice_sd,
        max_hstat = max_hstat,
        interaction_flag = interaction_flag,
        analyzed_numeric_features = length(eval_feats),
        sample_n = sample_n
      )

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = metrics,
        artifacts = list(
          recommendation = list(
            recommended_effect_method = recommended_method,
            dependence_flag = dependence_flag,
            interaction_flag = interaction_flag,
            hstat_threshold = hstat_threshold,
            regionalize = regionalize,
            regional_method_used = if (identical(regional_method, "auto")) recommended_method else regional_method
          ),
          max_cor_pair = cor_pairs,
          ice_spread = ice_spread,
          ice_curves = ice_curves,
          pd_curves = pd_curves,
          ale_curves = ale_curves,
          hstats = hstats,
          gadget = gadget_results,
          gadget_regions = gadget_regions
        ),
        messages = c(
          "Note: PDP/ALE/ICE and H-statistic rely on intervention-style predictions; interpret with care under strong feature dependence and extrapolation."
        )
      )
    }
  )
)
