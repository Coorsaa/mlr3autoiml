# FILE: R/plot_gate_02_structure.R
#
# Plot helpers for Gate 2 (Dependence & structure).
#
# Design goal: make ALE and PDP+ICE plots look close to {iml}'s defaults:
# - ALE: single line + rug
# - PDP+ICE: many ICE lines + one PDP line + rug + legend "Curve"
#
# IMPORTANT: this file must not redefine the Gate2 R6 class.

NULL

.autoiml_plot_ctx = function(x) {
  if (inherits(x, "AutoIML")) {
    return(x$ctx)
  }
  if (inherits(x, "AutoIMLResult")) {
    if (!is.null(x$extras) && is.list(x$extras) && !is.null(x$extras$ctx)) {
      return(x$extras$ctx)
    }
    return(NULL)
  }
  NULL
}

.autoiml_theme_iml = function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right",
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "plain"),
      axis.title = ggplot2::element_text(face = "plain")
    )
}

.autoiml_rug_dt = function(result, feature, max_n = 2000L) {
  ctx = .autoiml_plot_ctx(result)
  if (is.null(ctx) || !is.environment(ctx) || is.null(ctx$task)) {
    return(NULL)
  }

  task = ctx$task
  if (!inherits(task, "Task")) {
    return(NULL)
  }

  if (!feature %in% task$feature_names) {
    return(NULL)
  }

  v = tryCatch(task$data(cols = feature)[[feature]], error = function(e) NULL)
  if (is.null(v)) {
    return(NULL)
  }

  v = suppressWarnings(as.numeric(v))
  v = v[is.finite(v)]
  if (length(v) == 0L) {
    return(NULL)
  }

  max_n = as.integer(max_n)
  if (length(v) > max_n) {
    set.seed(1L)
    v = sample(v, max_n)
  }

  data.table::data.table(x = v)
}

.autoiml_plot_g2_ice_spread = function(result, class_label = NULL, top_n = 12L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  gr = .autoiml_get_gate_result(result, "G2")
  if (is.null(gr)) {
    return(NULL)
  }

  dt = gr$artifacts$ice_spread
  if (is.null(dt) || nrow(dt) == 0L) {
    return(NULL)
  }

  dt = data.table::as.data.table(dt)

  if (!is.null(class_label) && "class_label" %in% names(dt)) {
    cl_sel = as.character(class_label)[1L]
    dt = dt[class_label == cl_sel]
  }

  if (nrow(dt) == 0L) {
    return(NULL)
  }

  top_n = as.integer(top_n)
  dt = dt[order(-ice_sd_mean)][seq_len(min(top_n, .N))]
  dt[, feature := factor(feature, levels = rev(feature))]

  ggplot2::ggplot(dt, ggplot2::aes(x = ice_sd_mean, y = feature)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Gate 2: ICE heterogeneity",
      x = "Mean SD of centered ICE (higher = more heterogeneous effect)",
      y = NULL
    ) +
    .autoiml_theme_iml()
}

.autoiml_plot_g2_effect = function(result,
  feature = NULL,
  method = c("auto", "pdp", "ale"),
  class_label = NULL,
  show_ice = TRUE,
  ice_max_curves = 200L,
  rug = TRUE,
  rug_alpha = 0.25,
  base_size = 11) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  method = match.arg(method)

  gr = .autoiml_get_gate_result(result, "G2")
  if (is.null(gr)) {
    return(NULL)
  }

  pd = gr$artifacts$pd_curves
  ale = gr$artifacts$ale_curves
  ice = gr$artifacts$ice_curves

  feats = unique(c(
    if (!is.null(pd) && nrow(pd) > 0L) unique(pd$feature) else character(),
    if (!is.null(ale) && nrow(ale) > 0L) unique(ale$feature) else character()
  ))
  if (length(feats) == 0L) {
    return(NULL)
  }

  if (is.null(feature)) {
    feature = feats[1L]
  }
  feature = as.character(feature)

  # Multi-feature -> patchwork grid if available
  if (length(feature) > 1L) {
    plots = lapply(feature, function(f) {
      .autoiml_plot_g2_effect(
        result = result,
        feature = f,
        method = method,
        class_label = class_label,
        show_ice = show_ice,
        ice_max_curves = ice_max_curves,
        rug = rug,
        rug_alpha = rug_alpha,
        base_size = base_size
      )
    })
    plots = Filter(Negate(is.null), plots)
    if (length(plots) == 0L) {
      return(NULL)
    }
    if (requireNamespace("patchwork", quietly = TRUE)) {
      ncol = min(3L, length(plots))
      return(patchwork::wrap_plots(plots, ncol = ncol))
    }
    return(plots)
  }

  feat_sel = feature[1L]
  if (!feat_sel %in% feats) {
    stop("Unknown feature: ", feat_sel, call. = FALSE)
  }

  rec = gr$artifacts$recommendation %||% list()
  method_eff = if (method == "auto") (rec$recommended_effect_method %||% "pdp") else method

  rug_dt = if (isTRUE(rug)) .autoiml_rug_dt(result, feature = feat_sel) else NULL

  # =========================
  # PDP + ICE (iml-like)
  # =========================
  if (method_eff == "pdp") {
    if (is.null(pd) || nrow(pd) == 0L) {
      return(NULL)
    }

    pd_dt = data.table::as.data.table(pd)[feature == feat_sel]

    if (!is.null(class_label) && "class_label" %in% names(pd_dt)) {
      cl_sel = as.character(class_label)[1L]
      pd_dt = pd_dt[class_label == cl_sel]
    }

    if (nrow(pd_dt) == 0L) {
      return(NULL)
    }

    # ICE data (stored subset); group by row_id
    ice_dt = NULL
    if (isTRUE(show_ice) && !is.null(ice) && nrow(ice) > 0L) {
      ice_dt = data.table::as.data.table(ice)[feature == feat_sel]

      if (!is.null(class_label) && "class_label" %in% names(ice_dt)) {
        cl_sel = as.character(class_label)[1L]
        ice_dt = ice_dt[class_label == cl_sel]
      }

      if (!"row_id" %in% names(ice_dt)) {
        # fallback if older artifact format is present
        if ("row_index" %in% names(ice_dt)) {
          data.table::setnames(ice_dt, "row_index", "row_id")
        }
      }

      if (nrow(ice_dt) > 0L && "row_id" %in% names(ice_dt)) {
        ice_max_curves = as.integer(ice_max_curves)
        ids = unique(ice_dt$row_id)
        if (length(ids) > ice_max_curves) {
          set.seed(1L)
          ids = sample(ids, ice_max_curves)
        }
        ice_dt = ice_dt[row_id %in% ids]
      } else {
        ice_dt = NULL
      }
    }

    ttl = if (!is.null(class_label)) {
      sprintf("Positive: %s \u2013 PDP + ICE", as.character(class_label)[1L])
    } else {
      "PDP + ICE"
    }

    p = ggplot2::ggplot()

    if (!is.null(ice_dt) && nrow(ice_dt) > 0L) {
      p = p +
        ggplot2::geom_line(
          data = ice_dt,
          mapping = ggplot2::aes(x = x, y = yhat, group = row_id, color = "ICE"),
          alpha = 0.18,
          linewidth = 0.25
        )
    }

    p = p +
      ggplot2::geom_line(
        data = pd_dt,
        mapping = ggplot2::aes(x = x, y = pd, color = "PDP"),
        linewidth = 0.9
      )

    if (!is.null(rug_dt)) {
      p = p +
        ggplot2::geom_rug(
          data = rug_dt,
          mapping = ggplot2::aes(x = x),
          inherit.aes = FALSE,
          sides = "b",
          alpha = rug_alpha
        )
    }

    p = p +
      ggplot2::scale_color_manual(
        name = "Curve",
        values = c("ICE" = "grey60", "PDP" = "#E69F00")
      ) +
      ggplot2::labs(
        title = ttl,
        x = feat_sel,
        y = "Predicted y"
      ) +
      .autoiml_theme_iml(base_size = base_size)

    if ("class_label" %in% names(pd_dt) && is.null(class_label)) {
      p = p + ggplot2::facet_wrap(~class_label, scales = "free_y")
    }

    return(p)
  }

  # =========================
  # ALE (iml-like)
  # =========================
  if (is.null(ale) || nrow(ale) == 0L) {
    return(NULL)
  }

  ale_dt = data.table::as.data.table(ale)[feature == feat_sel]

  if (!is.null(class_label) && "class_label" %in% names(ale_dt)) {
    cl_sel = as.character(class_label)[1L]
    ale_dt = ale_dt[class_label == cl_sel]
  }

  if (nrow(ale_dt) == 0L) {
    return(NULL)
  }

  ttl = if (!is.null(class_label)) {
    sprintf("ALE \u2013 %s", as.character(class_label)[1L])
  } else {
    "ALE"
  }

  p = ggplot2::ggplot(ale_dt, ggplot2::aes(x = x, y = ale)) +
    ggplot2::geom_line(color = "grey20", linewidth = 0.8)

  if (!is.null(rug_dt)) {
    p = p +
      ggplot2::geom_rug(
        data = rug_dt,
        mapping = ggplot2::aes(x = x),
        inherit.aes = FALSE,
        sides = "b",
        alpha = rug_alpha
      )
  }

  p = p +
    ggplot2::labs(
      title = ttl,
      x = feat_sel,
      y = "ALE of y"
    ) +
    .autoiml_theme_iml(base_size = base_size)

  if ("class_label" %in% names(ale_dt) && is.null(class_label)) {
    p = p + ggplot2::facet_wrap(~class_label, scales = "free_y")
  }

  p
}

.autoiml_plot_g2_hstats = function(result, class_label = NULL, top_n = 15L, as_barplot = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  gr = .autoiml_get_gate_result(result, "G2")
  if (is.null(gr)) {
    return(NULL)
  }

  dt = gr$artifacts$hstats
  if (is.null(dt) || nrow(dt) == 0L) {
    return(NULL)
  }

  dt = data.table::as.data.table(dt)

  if (!is.null(class_label) && "class_label" %in% names(dt)) {
    cl_sel = as.character(class_label)[1L]
    dt = dt[class_label == cl_sel]
  }
  if (nrow(dt) == 0L) {
    return(NULL)
  }

  if (!"feature1" %in% names(dt) && "f1" %in% names(dt)) data.table::setnames(dt, "f1", "feature1")
  if (!"feature2" %in% names(dt) && "f2" %in% names(dt)) data.table::setnames(dt, "f2", "feature2")

  dt[, pair := paste0(feature1, " \u00D7 ", feature2)]
  top_n = as.integer(top_n)
  dt = dt[is.finite(hstat)][order(-hstat)][seq_len(min(top_n, .N))]
  dt[, pair := factor(pair, levels = rev(pair))]

  if (isTRUE(as_barplot)) {
    return(
      ggplot2::ggplot(dt, ggplot2::aes(x = hstat, y = pair)) +
        ggplot2::geom_col() +
        ggplot2::labs(
          title = "Gate 2: Interaction strength (Friedman H-statistic)",
          x = "H-statistic",
          y = NULL
        ) +
        .autoiml_theme_iml()
    )
  }

  ggplot2::ggplot(dt, ggplot2::aes(x = hstat, y = pair)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Gate 2: Interaction strength (Friedman H-statistic)",
      x = "H-statistic",
      y = NULL
    ) +
    .autoiml_theme_iml()
}

.autoiml_plot_g2_gadget = function(result, feature = NULL, class_label = NULL, ncol = 2L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  gr = .autoiml_get_gate_result(result, "G2")
  if (is.null(gr)) {
    return(NULL)
  }

  gad = gr$artifacts$gadget
  if (is.null(gad) || length(gad) == 0L) {
    stop("No GADGET-style regionalization results found in Gate 2 artifacts.", call. = FALSE)
  }

  if (is.null(feature)) feature = names(gad)[1L]
  feature = as.character(feature)[1L]
  if (!feature %in% names(gad)) stop("Unknown feature for regionalization: ", feature, call. = FALSE)

  one = gad[[feature]]
  if (is.null(one$curves) || nrow(one$curves) == 0L) {
    return(NULL)
  }

  dt = data.table::as.data.table(one$curves)

  if (!is.null(class_label) && "class_label" %in% names(dt)) {
    cl_sel = as.character(class_label)[1L]
    dt = dt[class_label == cl_sel]
  }
  if (nrow(dt) == 0L) {
    return(NULL)
  }

  ggplot2::ggplot(dt, ggplot2::aes(x = x, y = y, group = region_id)) +
    ggplot2::geom_line(color = "grey20", linewidth = 0.7) +
    ggplot2::facet_wrap(~path, ncol = as.integer(ncol)) +
    ggplot2::labs(
      title = sprintf("Gate 2: Regionalized effect curves (GADGET-style) for %s", feature),
      x = feature,
      y = "Centered effect (regional)"
    ) +
    .autoiml_theme_iml()
}
