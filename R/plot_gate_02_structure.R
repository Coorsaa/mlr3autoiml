# FILE: R/plot_gate_02_structure.R

#' @title Gate 2 Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helpers for Gate 2 (Dependence & Structure).
#'
#' Design goal: make ALE and PDP+ICE plots look close to the 'iml' package's defaults:
#' \itemize{
#'   \item ALE: single line + rug
#'   \item PDP+ICE: many ICE lines + one PDP line + rug + legend "Curve"
#' }
#'
#' These functions visualize feature effects, interactions, and heterogeneity
#' diagnostics produced by Gate 2.
#'
#' @name plot_gate_02_structure
#' @keywords internal
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

.autoiml_should_facet_class_label = function(dt, class_label = NULL) {
  if (!is.null(class_label) || !"class_label" %in% names(dt)) {
    return(FALSE)
  }

  vals = unique(dt[["class_label"]])
  vals = vals[!is.na(vals)]
  length(vals) > 1L
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
  feat_levels = unique(dt$feature)
  dt[, feature := factor(feature, levels = rev(feat_levels))]
  pal = .autoiml_plot_palette()

  ggplot2::ggplot(dt, ggplot2::aes(x = ice_sd_mean, y = feature)) +
    ggplot2::geom_col(fill = pal$metric[["primary"]]) +
    ggplot2::labs(
      title = "G2: ICE heterogeneity",
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

  rec = gr$artifacts$recommendation %??% list()
  method_eff = if (method == "auto") (rec$recommended_effect_method %??% "pdp") else method

  pal = .autoiml_plot_palette()
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
      sprintf("Positive: %s \u002D PDP + ICE", as.character(class_label)[1L])
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
        values = c("ICE" = pal$reference[["neutral"]], "PDP" = pal$metric[["primary"]])
      ) +
      ggplot2::labs(
        title = ttl,
        x = feat_sel,
        y = "Predicted y"
      ) +
      .autoiml_theme_iml(base_size = base_size)

    if (.autoiml_should_facet_class_label(pd_dt, class_label = class_label)) {
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
    sprintf("ALE \u002D %s", as.character(class_label)[1L])
  } else {
    "ALE"
  }

  p = ggplot2::ggplot(ale_dt, ggplot2::aes(x = x, y = ale)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    ggplot2::geom_line(color = pal$metric[["primary"]], linewidth = 0.8)

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
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.18, 0.1))) +
    ggplot2::labs(
      title = ttl,
      x = feat_sel,
      y = "ALE of y"
    ) +
    .autoiml_theme_iml(base_size = base_size)

  if (.autoiml_should_facet_class_label(ale_dt, class_label = class_label)) {
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

  pal = .autoiml_plot_palette()

  if (isTRUE(as_barplot)) {
    return(
      ggplot2::ggplot(dt, ggplot2::aes(x = hstat, y = pair)) +
        ggplot2::geom_col(fill = pal$metric[["tertiary"]]) +
        ggplot2::labs(
          title = "G2: Interaction strength (Friedman H-statistic)",
          x = "H-statistic",
          y = NULL
        ) +
        .autoiml_theme_iml()
    )
  }

  ggplot2::ggplot(dt, ggplot2::aes(x = hstat, y = pair)) +
    ggplot2::geom_point(color = pal$metric[["primary"]], size = 2) +
    ggplot2::labs(
      title = "G2: Interaction strength (Friedman H-statistic)",
      x = "H-statistic",
      y = NULL
    ) +
    .autoiml_theme_iml()
}

.autoiml_gadget_region_colors = function(paths) {
  paths = unique(as.character(paths))
  paths = paths[nzchar(paths)]
  pal = .autoiml_plot_palette()
  base = c(
    pal$metric[["primary"]],
    pal$metric[["secondary"]],
    pal$metric[["tertiary"]],
    pal$metric[["quaternary"]],
    pal$status[["pass"]],
    pal$status[["warn"]]
  )
  if (length(paths) == 0L) {
    return(character())
  }
  stats::setNames(rep(base, length.out = length(paths)), paths)
}

.autoiml_plot_g2_gadget = function(result,
  feature = NULL,
  class_label = NULL,
  mode = c("facet", "overlay"),
  show_local = TRUE,
  local_max_curves = 120L,
  rug = TRUE,
  rug_alpha = 0.25,
  ncol = 2L,
  base_size = 11) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  mode = match.arg(mode)

  gr = .autoiml_get_gate_result(result, "G2")
  if (is.null(gr)) {
    return(NULL)
  }

  gm = gr$artifacts$gadget_multi
  gad = gr$artifacts$gadget
  use_multi = !is.null(gm) && !is.null(gm$curves) && nrow(gm$curves) > 0L

  if (use_multi) {
    curves = data.table::as.data.table(gm$curves)
    local_dt = if (!is.null(gm$local_curves)) data.table::as.data.table(gm$local_curves) else NULL
    global_dt = if (!is.null(gm$global_curves)) data.table::as.data.table(gm$global_curves) else NULL
    assignments = if (!is.null(gm$assignments)) data.table::as.data.table(gm$assignments) else NULL
    feature_available = unique(curves$feature)
  } else {
    if (is.null(gad) || length(gad) == 0L) {
      stop("No GADGET-style regionalization results found in Gate 2 artifacts.", call. = FALSE)
    }
    if (is.null(feature)) feature = names(gad)[1L]
    feature_sel0 = as.character(feature)[1L]
    if (!feature_sel0 %in% names(gad)) stop("Unknown feature for regionalization: ", feature_sel0, call. = FALSE)
    one = gad[[feature_sel0]]
    if (is.null(one$curves) || nrow(one$curves) == 0L) {
      return(NULL)
    }
    curves = data.table::as.data.table(one$curves)
    if (!"feature" %in% names(curves)) curves[, feature := feature_sel0]
    if (!"y_region" %in% names(curves) && "y" %in% names(curves)) curves[, y_region := y]
    if (!"y_global" %in% names(curves)) curves[, y_global := NA_real_]
    local_dt = if (!is.null(one$local_curves)) data.table::as.data.table(one$local_curves) else NULL
    if (!is.null(local_dt) && !"feature" %in% names(local_dt)) local_dt[, feature := feature_sel0]
    global_dt = if (!is.null(one$global_curves)) data.table::as.data.table(one$global_curves) else NULL
    if (!is.null(global_dt) && !"feature" %in% names(global_dt)) global_dt[, feature := feature_sel0]
    assignments = if (!is.null(one$assignments)) data.table::as.data.table(one$assignments) else NULL
    feature_available = unique(curves$feature)
  }

  if (length(feature_available) == 0L) {
    return(NULL)
  }
  if (is.null(feature)) {
    feature = feature_available[1L]
  }
  feature_sel = as.character(feature)[1L]
  if (!feature_sel %in% feature_available) {
    stop("Unknown feature for regionalization: ", feature_sel, call. = FALSE)
  }

  curves = curves[feature == feature_sel]
  if (!is.null(class_label) && "class_label" %in% names(curves)) {
    cl_sel = as.character(class_label)[1L]
    curves = curves[class_label == cl_sel]
  }
  if (nrow(curves) == 0L) {
    return(NULL)
  }

  if (!is.null(local_dt) && nrow(local_dt) > 0L) {
    local_dt = local_dt[feature == feature_sel]
    if (!is.null(class_label) && "class_label" %in% names(local_dt)) {
      local_dt = local_dt[class_label == as.character(class_label)[1L]]
    }
    if (nrow(local_dt) > 0L && "row_id" %in% names(local_dt)) {
      ids = unique(local_dt$row_id)
      local_max_curves = as.integer(local_max_curves)
      if (length(ids) > local_max_curves) {
        set.seed(1L)
        ids = sample(ids, local_max_curves)
      }
      local_dt = local_dt[row_id %in% ids]
    }
  }

  if (!is.null(global_dt) && nrow(global_dt) > 0L) {
    global_dt = global_dt[feature == feature_sel]
    if (!is.null(class_label) && "class_label" %in% names(global_dt)) {
      global_dt = global_dt[class_label == as.character(class_label)[1L]]
    }
  }
  if (is.null(global_dt) || nrow(global_dt) == 0L) {
    global_dt = unique(curves[is.finite(y_global), .(feature, x, y_global)])
  }

  rug_dt = NULL
  if (isTRUE(rug)) {
    ctx = .autoiml_plot_ctx(result)
    if (!is.null(ctx) && is.environment(ctx) && !is.null(ctx$task) &&
      !is.null(assignments) && nrow(assignments) > 0L &&
      "row_id" %in% names(assignments) && feature_sel %in% ctx$task$feature_names) {
      if (!is.null(class_label) && "class_label" %in% names(assignments)) {
        assignments = assignments[class_label == as.character(class_label)[1L]]
      }
      vals = tryCatch(ctx$task$data(rows = assignments$row_id, cols = feature_sel)[[feature_sel]], error = function(e) NULL)
      if (!is.null(vals)) {
        rug_dt = data.table::data.table(
          row_id = assignments$row_id,
          region_id = assignments$region_id,
          path = assignments$path,
          x = suppressWarnings(as.numeric(vals))
        )
        rug_dt = rug_dt[is.finite(x)]
      }
    }
    if ((is.null(rug_dt) || nrow(rug_dt) == 0L)) {
      rug_dt = .autoiml_rug_dt(result, feature = feature_sel)
      if (!is.null(rug_dt) && nrow(rug_dt) > 0L) rug_dt[, path := curves$path[1L]]
    }
  }

  pal = .autoiml_plot_palette()
  region_values = .autoiml_gadget_region_colors(unique(c(curves$path, if (!is.null(rug_dt)) rug_dt$path else character())))
  class_txt = if (!is.null(class_label)) sprintf(" - %s", as.character(class_label)[1L]) else ""

  if (mode == "facet") {
    p = ggplot2::ggplot()

    if (isTRUE(show_local) && !is.null(local_dt) && nrow(local_dt) > 0L) {
      p = p + ggplot2::geom_line(
        data = local_dt,
        mapping = ggplot2::aes(x = x, y = local_effect, group = interaction(row_id, path)),
        color = pal$reference[["neutral"]],
        alpha = 0.14,
        linewidth = 0.22
      )
    }

    if ("y_global" %in% names(curves) && any(is.finite(curves$y_global))) {
      p = p + ggplot2::geom_line(
        data = curves[is.finite(y_global)],
        mapping = ggplot2::aes(x = x, y = y_global, group = path),
        color = pal$reference[["neutral"]],
        linetype = "dashed",
        linewidth = 0.55
      )
    }

    p = p + ggplot2::geom_line(
      data = curves,
      mapping = ggplot2::aes(x = x, y = y_region, color = path, group = region_id),
      linewidth = 0.9
    )

    if (!is.null(rug_dt) && nrow(rug_dt) > 0L && "path" %in% names(rug_dt)) {
      p = p + ggplot2::geom_rug(
        data = rug_dt,
        mapping = ggplot2::aes(x = x, color = path),
        inherit.aes = FALSE,
        sides = "b",
        alpha = rug_alpha
      )
    }

    return(p +
      ggplot2::scale_color_manual(values = region_values, name = "Region") +
      ggplot2::facet_wrap(stats::as.formula("~path"), ncol = as.integer(ncol), scales = "free_y") +
      ggplot2::labs(
        title = sprintf("G2: GADGET regional effects%s - %s", class_txt, feature_sel),
        subtitle = "Thin lines: centered local effects; dashed line: global effect; thick line: regional effect",
        x = feature_sel,
        y = "Centered local effect"
      ) +
      .autoiml_theme_iml(base_size = base_size))
  }

  p = ggplot2::ggplot()

  if (isTRUE(show_local) && !is.null(local_dt) && nrow(local_dt) > 0L) {
    p = p + ggplot2::geom_line(
      data = local_dt,
      mapping = ggplot2::aes(x = x, y = local_effect, group = row_id),
      color = pal$reference[["neutral"]],
      alpha = 0.10,
      linewidth = 0.20
    )
  }

  if (!is.null(global_dt) && nrow(global_dt) > 0L && any(is.finite(global_dt$y_global))) {
    p = p + ggplot2::geom_line(
      data = global_dt[is.finite(y_global)],
      mapping = ggplot2::aes(x = x, y = y_global),
      color = pal$reference[["neutral"]],
      linetype = "dashed",
      linewidth = 0.75
    )
  }

  p = p + ggplot2::geom_line(
    data = curves,
    mapping = ggplot2::aes(x = x, y = y_region, color = path, group = region_id),
    linewidth = 0.95
  )

  if (!is.null(rug_dt) && nrow(rug_dt) > 0L && "path" %in% names(rug_dt)) {
    p = p + ggplot2::geom_rug(
      data = rug_dt,
      mapping = ggplot2::aes(x = x, color = path),
      inherit.aes = FALSE,
      sides = "b",
      alpha = rug_alpha
    )
  }

  p +
    ggplot2::scale_color_manual(values = region_values, name = "Region") +
    ggplot2::labs(
      title = sprintf("G2: GADGET regional effects%s - %s", class_txt, feature_sel),
      subtitle = "Thin lines: centered local effects; dashed line: global effect; colored lines: regional effects",
      x = feature_sel,
      y = "Centered local effect"
    ) +
    .autoiml_theme_iml(base_size = base_size)
}

.autoiml_gadget_tree_leaf_grob = function(curves,
  region_color,
  leaf_label,
  local_curves = NULL,
  rug_dt = NULL,
  base_size = 11) {
  curves = data.table::as.data.table(curves)
  if (nrow(curves) == 0L) {
    return(grid::nullGrob())
  }

  pal = .autoiml_plot_palette()
  curves = data.table::copy(curves)
  curves[, feature := factor(feature, levels = unique(feature))]

  curve_group_n = curves[, .N, by = feature]
  line_features = as.character(curve_group_n[N >= 2L, feature])
  curves_line = curves[as.character(feature) %in% line_features]
  curves_single = curves[!(as.character(feature) %in% line_features)]

  local_curves = if (!is.null(local_curves) && nrow(local_curves) > 0L) {
    dt = data.table::as.data.table(local_curves)
    dt = dt[feature %in% as.character(unique(curves$feature))]
    if (nrow(dt) > 0L) {
      dt[, feature := factor(feature, levels = levels(curves$feature))]
      dt_group_n = dt[, .N, by = .(row_id, feature)]
      dt = merge(dt, dt_group_n, by = c("row_id", "feature"), all.x = TRUE, sort = FALSE)
      dt
    } else {
      NULL
    }
  } else {
    NULL
  }

  rug_dt = if (!is.null(rug_dt) && nrow(rug_dt) > 0L) {
    dt = data.table::as.data.table(rug_dt)
    dt = dt[feature %in% as.character(unique(curves$feature))]
    if (nrow(dt) > 0L) {
      dt[, feature := factor(feature, levels = levels(curves$feature))]
      dt
    } else {
      NULL
    }
  } else {
    NULL
  }

  p = ggplot2::ggplot(curves, ggplot2::aes(x = x)) +
    {
      if (!is.null(local_curves) && nrow(local_curves) > 0L) {
        ggplot2::geom_line(
          data = local_curves[N >= 2L],
          mapping = ggplot2::aes(y = local_effect, group = interaction(row_id, feature)),
          linewidth = 0.22,
          alpha = 0.16,
          color = pal$reference[["neutral"]]
        )
      }
    } +
    {
      if (!is.null(local_curves) && nrow(local_curves[N < 2L]) > 0L) {
        ggplot2::geom_point(
          data = local_curves[N < 2L],
          mapping = ggplot2::aes(y = local_effect),
          size = 0.30,
          alpha = 0.24,
          color = pal$reference[["neutral"]]
        )
      }
    } +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.20,
      color = pal$reference[["neutral"]],
      alpha = 0.55
    ) +
    ggplot2::geom_line(
      data = curves_line[is.finite(y_global)],
      mapping = ggplot2::aes(y = y_global, group = feature),
      color = pal$reference[["neutral"]],
      linetype = "dashed",
      linewidth = 0.30
    ) +
    ggplot2::geom_line(
      data = curves_line,
      mapping = ggplot2::aes(y = y_region, group = feature),
      color = region_color,
      linewidth = 0.65
    ) +
    {
      if (nrow(curves_single) > 0L) {
        ggplot2::geom_point(
          data = curves_single,
          mapping = ggplot2::aes(y = y_region),
          size = 0.55,
          color = region_color,
          alpha = 0.85
        )
      }
    } +
    {
      if (!is.null(rug_dt) && nrow(rug_dt) > 0L) {
        ggplot2::geom_rug(
          data = rug_dt,
          mapping = ggplot2::aes(x = x),
          inherit.aes = FALSE,
          sides = "b",
          alpha = 0.22,
          linewidth = 0.18,
          color = pal$reference[["neutral"]]
        )
      }
    } +
    ggplot2::facet_wrap(stats::as.formula("~feature"), nrow = 1L, scales = "free") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.04))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.14, 0.12))) +
    ggplot2::labs(title = leaf_label, x = "x", y = "effect") +
    .autoiml_theme_iml(base_size = max(4.1, base_size * 0.38)) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = max(2.8, base_size * 0.25), color = "#3A3A3A"),
      axis.title = ggplot2::element_text(size = max(3.0, base_size * 0.26), color = "#3A3A3A"),
      axis.ticks = ggplot2::element_line(linewidth = 0.15, color = "#9A9A9A"),
      axis.ticks.length = grid::unit(0.6, "pt"),
      legend.position = "none",
      strip.text = ggplot2::element_text(size = max(3.4, base_size * 0.31), face = "bold", color = "#1F1F1F"),
      strip.background = ggplot2::element_rect(fill = pal$surface[["panel"]], color = NA),
      plot.title = ggplot2::element_text(
        size = max(3.8, base_size * 0.34),
        face = "bold",
        hjust = 0.5,
        margin = ggplot2::margin(b = 1)
      ),
      plot.background = ggplot2::element_rect(fill = "white", color = region_color, linewidth = 0.75),
      panel.spacing.x = grid::unit(0.25, "lines"),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )

  ggplot2::ggplotGrob(p)
}

.autoiml_plot_g2_gadget_tree = function(result, base_size = 11) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }
  gr = .autoiml_get_gate_result(result, "G2")
  if (is.null(gr)) {
    return(NULL)
  }
  gm = gr$artifacts$gadget_multi
  splits = gr$artifacts$gadget_splits
  regions = gr$artifacts$gadget_regions
  if (is.null(splits) || nrow(splits) == 0L) {
    return(NULL)
  }

  splits = data.table::as.data.table(splits)
  regions = data.table::as.data.table(regions %??% data.table::data.table())
  pal = .autoiml_plot_palette()

  child_path = function(parent_path, rule) {
    parent_path = as.character(parent_path %??% "(root)")
    rule = as.character(rule %??% "")
    if (!nzchar(rule)) return(parent_path)
    if (identical(parent_path, "(root)")) rule else paste(parent_path, rule, sep = " & ")
  }

  make_split_value = function(dt) {
    if ("gain_ratio" %in% names(dt) && any(is.finite(dt$gain_ratio))) {
      return(sprintf("gain %.1f%%", 100 * dt$gain_ratio))
    }
    if ("gain" %in% names(dt) && any(is.finite(dt$gain))) {
      return(sprintf("gain %.0f", dt$gain))
    }
    rep(NA_character_, nrow(dt))
  }

  short_rule = function(split_feature, split_type, threshold, level, side) {
    split_type = as.character(split_type %??% "numeric")
    split_feature = as.character(split_feature %??% "split")
    side = match.arg(side, c("left", "right"))

    if (identical(split_type, "numeric")) {
      thr = if (is.finite(threshold)) format(signif(threshold, 4L), trim = TRUE) else "?"
      op = if (identical(side, "left")) "<=" else ">"
      return(sprintf("%s %s %s", split_feature, op, thr))
    }

    lvl = as.character(level %??% "level")
    op = if (identical(side, "left")) "==" else "!="
    sprintf("%s %s %s", split_feature, op, lvl)
  }

  splits[, `:=`(
    node_kind = "split",
    path_chr = as.character(path),
    split_value = make_split_value(.SD),
    child_left = vapply(seq_len(.N), function(i) child_path(path[i], rule_left[i]), character(1L)),
    child_right = vapply(seq_len(.N), function(i) child_path(path[i], rule_right[i]), character(1L)),
    branch_left = vapply(seq_len(.N), function(i) short_rule(split_feature[i], split_type[i], threshold[i], level[i], "left"), character(1L)),
    branch_right = vapply(seq_len(.N), function(i) short_rule(split_feature[i], split_type[i], threshold[i], level[i], "right"), character(1L))
  )]

  split_lookup = split(seq_len(nrow(splits)), splits$path_chr)
  node_map = new.env(parent = emptyenv())
  edge_rows = list()
  order_counter = 0L

  add_or_update_node = function(path, depth, node_kind, split_idx = NA_integer_) {
    key = as.character(path)
    if (exists(key, envir = node_map, inherits = FALSE)) {
      node = get(key, envir = node_map, inherits = FALSE)
      node$depth = min(node$depth, depth)
      if (!identical(node_kind, "unknown")) node$node_kind = node_kind
      if (is.finite(split_idx)) node$split_idx = split_idx
    } else {
      node = list(
        path = key,
        depth = depth,
        node_kind = node_kind,
        split_idx = if (is.finite(split_idx)) split_idx else NA_integer_,
        x = NA_real_,
        y = NA_real_,
        order = NA_real_
      )
    }
    assign(key, node, envir = node_map)
    invisible(node)
  }

  walk_tree = function(path = "(root)", depth = 0L) {
    idx = split_lookup[[path]][1L] %??% NA_integer_
    if (is.finite(idx)) {
      add_or_update_node(path, depth, "split", split_idx = idx)
      left_path = splits$child_left[[idx]]
      right_path = splits$child_right[[idx]]

      edge_rows[[length(edge_rows) + 1L]] <<- data.table::data.table(
        parent_path = path,
        child_path = left_path,
        branch_side = "left",
        branch_label = splits$branch_left[[idx]],
        child_order = 1L
      )
      edge_rows[[length(edge_rows) + 1L]] <<- data.table::data.table(
        parent_path = path,
        child_path = right_path,
        branch_side = "right",
        branch_label = splits$branch_right[[idx]],
        child_order = 2L
      )

      walk_tree(left_path, depth + 1L)
      walk_tree(right_path, depth + 1L)

      node = get(path, envir = node_map, inherits = FALSE)
      left_node = get(left_path, envir = node_map, inherits = FALSE)
      right_node = get(right_path, envir = node_map, inherits = FALSE)
      node$x = mean(c(left_node$x, right_node$x))
      node$order = mean(c(left_node$order, right_node$order))
      assign(path, node, envir = node_map)
      return(invisible(NULL))
    }

    add_or_update_node(path, depth, "leaf")
    order_counter <<- order_counter + 1L
    node = get(path, envir = node_map, inherits = FALSE)
    node$x = order_counter
    node$order = order_counter
    assign(path, node, envir = node_map)
    invisible(NULL)
  }

  walk_tree()

  node_keys = ls(node_map, all.names = TRUE)
  nodes = data.table::rbindlist(lapply(node_keys, function(key) {
    node = get(key, envir = node_map, inherits = FALSE)
    data.table::data.table(
      path = node$path,
      depth = node$depth,
      node_kind = node$node_kind,
      split_idx = node$split_idx,
      x = node$x,
      order = node$order
    )
  }), fill = TRUE)

  max_depth = max(nodes$depth, na.rm = TRUE)
  nodes[, y := max_depth - depth]
  x_spacing = 1.35
  y_spacing = 1.12
  nodes[, `:=`(
    x = x * x_spacing,
    y = y * y_spacing
  )]

  nodes = merge(
    nodes,
    splits[, .(
      path_chr,
      split_feature,
      split_type,
      threshold,
      level,
      n_parent,
      gain,
      gain_ratio,
      split_value,
      target_features,
      semantics_label,
      class_label
    )],
    by.x = "path",
    by.y = "path_chr",
    all.x = TRUE,
    sort = FALSE
  )

  if (nrow(regions) > 0L) {
    nodes = merge(
      nodes,
      regions[, .(
        path,
        region_id,
        n_leaf = n,
        total_loss,
        bounds
      )],
      by = "path",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    nodes[, `:=`(region_id = NA_integer_, n_leaf = NA_real_, total_loss = NA_real_, bounds = NA_character_)]
  }

  nodes[, label_main := ifelse(
    node_kind == "split",
    as.character(split_feature),
    sprintf("Region %s", ifelse(is.na(region_id), "?", region_id))
  )]
  nodes[, label_stats := ifelse(
    node_kind == "split",
    sprintf("n = %s%s",
      ifelse(is.na(n_parent), "?", format(n_parent, trim = TRUE)),
      ifelse(is.na(split_value), "", paste0(" | ", split_value))
    ),
    sprintf("n = %s%s",
      ifelse(is.na(n_leaf), "?", format(n_leaf, trim = TRUE)),
      ifelse(is.na(total_loss), "", paste0(" | loss ", format(round(total_loss), big.mark = ",", trim = TRUE)))
    )
  )]
  nodes[, label_text := paste(label_main, label_stats, sep = "\n")]

  edge_dt = if (length(edge_rows) > 0L) data.table::rbindlist(edge_rows, fill = TRUE) else data.table::data.table()
  if (nrow(edge_dt) == 0L) {
    return(NULL)
  }

  edge_dt = merge(edge_dt, nodes[, .(parent_path = path, x_parent = x, y_parent = y, parent_depth = depth)], by = "parent_path", all.x = TRUE, sort = FALSE)
  edge_dt = merge(edge_dt, nodes[, .(child_path = path, x_child = x, y_child = y, child_kind = node_kind)], by = "child_path", all.x = TRUE, sort = FALSE)
  leaf_height = 0.90
  split_height = 0.26
  edge_dt[, parent_half_height := split_height / 2]
  edge_dt[, child_half_height := ifelse(child_kind == "leaf", leaf_height / 2, split_height / 2)]
  edge_dt[, line_dy := pmax(abs(y_child - y_parent), 1e-6)]
  edge_dt[, parent_frac := pmin(0.42, parent_half_height / line_dy)]
  edge_dt[, child_frac := pmin(0.42, child_half_height / line_dy)]
  edge_dt[, `:=`(
    x_from = x_parent + (x_child - x_parent) * parent_frac,
    y_from = y_parent + (y_child - y_parent) * parent_frac,
    x_to = x_child - (x_child - x_parent) * child_frac,
    y_to = y_child - (y_child - y_parent) * child_frac
  )]
  edge_dt[, `:=`(
    x_label = x_from + 0.54 * (x_to - x_from),
    y_label = y_from + 0.54 * (y_to - y_from) + ifelse(child_order == 1L, 0.11, -0.11)
  )]
  label_depth_limit = max_depth - 2L
  branch_label_dt = edge_dt[parent_depth <= label_depth_limit]

  region_paths = nodes[node_kind == "leaf" & !is.na(region_id), path]
  region_values = .autoiml_gadget_region_colors(region_paths)
  nodes[, fill_key := "internal"]

  fill_values = c(internal = pal$surface[["panel"]])

  internal_nodes = nodes[node_kind == "split"]
  leaf_nodes = nodes[node_kind == "leaf" & !is.na(region_id)]
  leaf_x_step = if (nrow(leaf_nodes) >= 2L) {
    min(diff(sort(unique(leaf_nodes$x))), na.rm = TRUE)
  } else {
    x_spacing
  }
  if (!is.finite(leaf_x_step) || leaf_x_step <= 0) {
    leaf_x_step = x_spacing
  }
  leaf_half_width = pmax(0.38, pmin(0.56, 0.46 * leaf_x_step))

  internal_nodes[, label_main := as.character(split_feature)]
  internal_nodes[, label_stats := sprintf(
    "n = %s%s",
    ifelse(is.na(n_parent), "?", format(n_parent, trim = TRUE)),
    ifelse(is.na(split_value), "", paste0(" | ", split_value))
  )]
  internal_nodes[, label_text := paste(label_main, label_stats, sep = "\n")]

  leaf_curve_dt = NULL
  if (!is.null(gm) && !is.null(gm$curves) && nrow(gm$curves) > 0L) {
    leaf_curve_dt = data.table::as.data.table(gm$curves)
  }

  leaf_local_dt = NULL
  if (!is.null(gm) && !is.null(gm$local_curves) && nrow(gm$local_curves) > 0L) {
    leaf_local_dt = data.table::as.data.table(gm$local_curves)
  }

  leaf_assignments = NULL
  if (!is.null(gm) && !is.null(gm$assignments) && nrow(gm$assignments) > 0L) {
    leaf_assignments = data.table::as.data.table(gm$assignments)
  }

  ctx = .autoiml_plot_ctx(result)

  p = ggplot2::ggplot()

  p = p +
    ggplot2::geom_segment(
      data = edge_dt,
      mapping = ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
      linewidth = 0.7,
      color = pal$reference[["neutral"]]
    )

  if (nrow(branch_label_dt) > 0L) {
    p = p +
      ggplot2::geom_label(
        data = branch_label_dt,
        mapping = ggplot2::aes(x = x_label, y = y_label, label = branch_label),
        size = 3.0,
        label.size = 0,
        label.padding = grid::unit(0.11, "lines"),
        fill = "white",
        color = pal$reference[["neutral"]],
        alpha = 0.95
      )
  }

  if (nrow(internal_nodes) > 0L) {
    p = p +
      ggplot2::geom_label(
        data = internal_nodes,
        mapping = ggplot2::aes(x = x, y = y, label = label_text, fill = fill_key),
        label.size = 0.25,
        label.padding = grid::unit(0.23, "lines"),
        label.r = grid::unit(0.13, "lines"),
        size = 3.3,
        color = "#1F1F1F",
        lineheight = 0.95
      )
  }

  if (nrow(leaf_nodes) > 0L && !is.null(leaf_curve_dt) && nrow(leaf_curve_dt) > 0L) {
    for (i in seq_len(nrow(leaf_nodes))) {
      leaf_path = leaf_nodes$path[[i]]
      region_color = region_values[[leaf_path]] %??% pal$metric[["primary"]]
      leaf_curves = leaf_curve_dt[path == leaf_path]
      if (nrow(leaf_curves) == 0L) {
        next
      }
      leaf_locals = if (!is.null(leaf_local_dt) && nrow(leaf_local_dt) > 0L) {
        leaf_local_dt[path == leaf_path]
      } else {
        NULL
      }

      leaf_rug = NULL
      if (!is.null(leaf_assignments) && !is.null(ctx) && is.environment(ctx) && !is.null(ctx$task)) {
        leaf_row_ids = leaf_assignments[path == leaf_path, row_id]
        if (length(leaf_row_ids) > 0L) {
          feat_vals = lapply(as.character(unique(leaf_curves$feature)), function(ff) {
            vals = tryCatch(ctx$task$data(rows = leaf_row_ids, cols = ff)[[ff]], error = function(e) NULL)
            if (is.null(vals)) {
              return(NULL)
            }
            data.table::data.table(
              feature = ff,
              x = suppressWarnings(as.numeric(vals))
            )
          })
          leaf_rug = data.table::rbindlist(feat_vals, fill = TRUE)
          if (!is.null(leaf_rug) && nrow(leaf_rug) > 0L) {
            leaf_rug = leaf_rug[is.finite(x)]
          }
        }
      }

      leaf_label = sprintf(
        "R%s  n=%s",
        ifelse(is.na(leaf_nodes$region_id[[i]]), "?", as.character(leaf_nodes$region_id[[i]])),
        ifelse(is.na(leaf_nodes$n_leaf[[i]]), "?", format(leaf_nodes$n_leaf[[i]], trim = TRUE))
      )
      p = p + ggplot2::annotation_custom(
        grob = .autoiml_gadget_tree_leaf_grob(
          curves = leaf_curves,
          region_color = region_color,
          leaf_label = leaf_label,
          local_curves = leaf_locals,
          rug_dt = leaf_rug,
          base_size = base_size
        ),
        xmin = leaf_nodes$x[[i]] - leaf_half_width,
        xmax = leaf_nodes$x[[i]] + leaf_half_width,
        ymin = leaf_nodes$y[[i]] - leaf_height / 2,
        ymax = leaf_nodes$y[[i]] + leaf_height / 2
      )
    }
  } else if (nrow(leaf_nodes) > 0L) {
    p = p +
      ggplot2::geom_label(
        data = leaf_nodes,
        mapping = ggplot2::aes(x = x, y = y, label = label_text, fill = fill_key),
        label.size = 0.25,
        label.padding = grid::unit(0.23, "lines"),
        label.r = grid::unit(0.13, "lines"),
        size = 3.3,
        color = "#1F1F1F",
        lineheight = 0.95
      )
  }

  p +
    ggplot2::scale_fill_manual(values = fill_values, guide = "none") +
    ggplot2::coord_cartesian(
      xlim = range(nodes$x, na.rm = TRUE) + c(-(leaf_half_width + 0.2), leaf_half_width + 0.2),
      ylim = c(min(nodes$y, na.rm = TRUE) - 0.50, max(nodes$y, na.rm = TRUE) + 0.45),
      clip = "off"
    ) +
    ggplot2::labs(
      title = "G2: GADGET split tree",
      subtitle = "Internal nodes show split and gain; branch labels are shown for higher-level splits; terminal nodes contain detailed GADGET regional curves",
      x = NULL,
      y = NULL
    ) +
    .autoiml_theme_iml(base_size = base_size) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 30, 10, 10)
    )
}

.autoiml_plot_g2_pint = function(result, class_label = NULL, top_n = 15L, base_size = 11) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }
  gr = .autoiml_get_gate_result(result, "G2")
  if (is.null(gr)) {
    return(NULL)
  }
  dt = gr$artifacts$pint
  if (is.null(dt) || nrow(dt) == 0L) {
    return(NULL)
  }
  dt = data.table::as.data.table(dt)
  if (!is.null(class_label) && "class_label" %in% names(dt)) {
    dt = dt[class_label == as.character(class_label)[1L]]
  }
  dt = dt[is.finite(observed_risk)]
  if (nrow(dt) == 0L) {
    return(NULL)
  }
  top_n = as.integer(top_n)
  dt = dt[order(-observed_risk)]
  dt = dt[seq_len(min(top_n, nrow(dt)))]
  dt[, feature_plot := factor(feature, levels = rev(feature))]
  dt[, flag := ifelse(pint_interaction %in% TRUE, "flagged", "not flagged")]

  pal = .autoiml_plot_palette()
  p = ggplot2::ggplot(dt, ggplot2::aes(y = feature_plot))

  if ("null_quantile" %in% names(dt) && any(is.finite(dt$null_quantile))) {
    p = p + ggplot2::geom_segment(
      data = dt[is.finite(null_quantile)],
      mapping = ggplot2::aes(x = 0, xend = null_quantile, yend = feature_plot),
      color = pal$reference[["neutral"]],
      linetype = "dashed",
      linewidth = 0.45
    ) +
      ggplot2::geom_point(
        data = dt[is.finite(null_quantile)],
        mapping = ggplot2::aes(x = null_quantile),
        color = pal$reference[["neutral"]],
        shape = 4,
        size = 2.2
      )
  }

  p +
    ggplot2::geom_point(ggplot2::aes(x = observed_risk, color = flag), size = 2.4) +
    ggplot2::scale_color_manual(
      values = c("flagged" = pal$metric[["secondary"]], "not flagged" = pal$metric[["primary"]]),
      drop = FALSE,
      name = "PINT"
    ) +
    ggplot2::labs(
      title = "G2: GADGET-PINT interaction screen",
      subtitle = "Points are observed centered-ICE risk; crosses show the permutation null quantile when available",
      x = "Centered-ICE interaction risk",
      y = NULL
    ) +
    .autoiml_theme_iml(base_size = base_size)
}

.autoiml_plot_g2_ale_2d = function(result,
  feature1 = NULL,
  feature2 = NULL,
  class_label = NULL,
  base_size = 11
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  gr = .autoiml_get_gate_result(result, "G2")
  if (is.null(gr)) {
    return(NULL)
  }

  ale2d = gr$artifacts$ale2d
  if (is.null(ale2d) || length(ale2d) == 0L) {
    return(NULL)
  }

  # Resolve pair
  if (!is.null(feature1) && !is.null(feature2)) {
    key = paste0(feature1, "::", feature2)
    key_rev = paste0(feature2, "::", feature1)
    dt = ale2d[[key]] %??% ale2d[[key_rev]]
    if (is.null(dt)) {
      return(NULL)
    }
  } else {
    dt = ale2d[[1L]]
    feature1 = dt$feature1[1L]
    feature2 = dt$feature2[1L]
  }

  dt = data.table::as.data.table(dt)

  if (!is.null(class_label) && "class_label" %in% names(dt)) {
    dt = dt[class_label == as.character(class_label)[1L]]
  }
  if (nrow(dt) < 1L) {
    return(NULL)
  }

  pal = .autoiml_plot_palette()
  ale2d_vals = dt$ale2d[is.finite(dt$ale2d)]
  abs_max = if (length(ale2d_vals) > 0L) max(abs(ale2d_vals)) else 0.01
  if (!is.finite(abs_max) || abs_max == 0) abs_max = 0.01
  contour_ok = length(unique(signif(ale2d_vals, 8L))) >= 2L

  cl_lbl = if (!is.null(class_label)) sprintf(" \u002D %s", as.character(class_label)[1L]) else ""
  x1_left = x1_right = x2_bottom = x2_top = x1 = x2 = ale2d = NULL

  p = ggplot2::ggplot(dt) +
    ggplot2::geom_rect(ggplot2::aes(
      xmin = x1_left,
      xmax = x1_right,
      ymin = x2_bottom,
      ymax = x2_top,
      fill = ale2d
    ), linewidth = 0)

  if (isTRUE(contour_ok)) {
    p = p +
      ggplot2::geom_contour(ggplot2::aes(x = x1, y = x2, z = ale2d),
        colour = "white", alpha = 0.4, linewidth = 0.3)
  }

  p +
    ggplot2::scale_fill_gradient2(
      low      = pal$gradient[["low"]],
      high     = pal$gradient[["high"]],
      mid      = pal$gradient[["mid"]],
      midpoint = 0,
      limits   = c(-abs_max, abs_max),
      name     = "ALE 2D"
    ) +
    ggplot2::labs(
      title = sprintf("G2: ALE interaction surface%s \u002D %s \u00D7 %s",
        cl_lbl, feature1, feature2),
      x = feature1,
      y = feature2
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    .autoiml_theme_iml(base_size = base_size)
}
