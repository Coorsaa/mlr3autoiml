# FILE: R/plot_gate_02_structure.R
#
# Gate 2 plotting helpers (ggplot2 + patchwork).
#
# These functions are called by AutoIML$plot(type = ...) and expect an
# AutoIMLResult object as first argument.
#
# @keywords internal
NULL

#' @keywords internal
.autoiml_plot_g2_ice_spread = function(result, class_label = NULL, top_n = 10L, as_barplot = TRUE) {
  .autoiml_require_pkg("ggplot2")
  checkmate::assert_integerish(top_n, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_flag(as_barplot)

  g2 = .autoiml_get_gate_result(result, "G2")
  dt = g2$artifacts$ice_spread
  if (is.null(dt) || nrow(dt) == 0L) {
    stop("Gate 2 artifacts do not contain 'ice_spread'. Run Gate 2 first.", call. = FALSE)
  }
  dt = data.table::as.data.table(dt)
  if (!"class_label" %in% names(dt)) dt[, class_label := NA_character_]

  # Choose default class label (first non-NA)
  if (is.null(class_label)) {
    cl = unique(dt$class_label)
    cl = cl[!is.na(cl)]
    class_label = if (length(cl) == 0L) NA_character_ else cl[[1L]]
  }
  if (!is.na(class_label)) {
    cl = as.character(class_label)
    dt = dt[class_label == cl]
  }
  if (nrow(dt) == 0L) {
    stop("No ICE spread results available for the requested class_label.", call. = FALSE)
  }

  dt = dt[order(-ice_sd_mean)]
  dt = dt[seq_len(min(as.integer(top_n), nrow(dt)))]
  dt[, feature := factor(feature, levels = rev(feature))]

  p = ggplot2::ggplot(dt, ggplot2::aes(x = feature, y = ice_sd_mean))
  if (as_barplot) {
    p = p + ggplot2::geom_col()
  } else {
    p = p + ggplot2::geom_point(size = 2)
  }

  p +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Gate 2: ICE spread (heterogeneity proxy)",
      subtitle = if (!is.na(class_label)) paste0("Class: ", class_label) else NULL,
      x = NULL,
      y = "Mean SD across centered ICE grid"
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
.autoiml_plot_g2_hstats = function(result, class_label = NULL, top_n = 10L, as_barplot = TRUE) {
  .autoiml_require_pkg("ggplot2")
  checkmate::assert_integerish(top_n, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_flag(as_barplot)

  g2 = .autoiml_get_gate_result(result, "G2")
  dt = g2$artifacts$hstats
  if (is.null(dt) || nrow(dt) == 0L) {
    stop("Gate 2 artifacts do not contain 'hstats'. Run Gate 2 first.", call. = FALSE)
  }
  dt = data.table::as.data.table(dt)
  if (!"class_label" %in% names(dt)) dt[, class_label := NA_character_]

  if (is.null(class_label)) {
    cl = unique(dt$class_label)
    cl = cl[!is.na(cl)]
    class_label = if (length(cl) == 0L) NA_character_ else cl[[1L]]
  }
  if (!is.na(class_label)) {
    cl = as.character(class_label)
    dt = dt[class_label == cl]
  }
  if (nrow(dt) == 0L) {
    stop("No H-statistic results available for the requested class_label.", call. = FALSE)
  }

  dt[, interaction := paste0(feature1, " × ", feature2)]
  dt = dt[order(-hstat)]
  dt = dt[seq_len(min(as.integer(top_n), nrow(dt)))]
  dt[, interaction := factor(interaction, levels = rev(interaction))]

  p = ggplot2::ggplot(dt, ggplot2::aes(x = interaction, y = hstat))
  if (as_barplot) {
    p = p + ggplot2::geom_col()
  } else {
    p = p + ggplot2::geom_point(size = 2)
  }

  p +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Gate 2: Interaction screening (Friedman–Popescu H)",
      subtitle = if (!is.na(class_label)) paste0("Class: ", class_label) else NULL,
      x = NULL,
      y = "H-statistic (0–1)"
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
.autoiml_plot_g2_effect = function(result, feature, class_label = NULL,
  method = c("auto", "pdp", "ale"),
  show_ice = TRUE,
  ice_max_curves = 50L,
  ice_alpha = 0.15,
  ice_center_for_ale = c("mean", "anchor")
) {
  .autoiml_require_pkg("ggplot2")
  .autoiml_require_pkg("patchwork")

  method = match.arg(method)
  ice_center_for_ale = match.arg(ice_center_for_ale)

  checkmate::assert_string(feature, min.chars = 1)
  checkmate::assert_flag(show_ice)
  checkmate::assert_integerish(ice_max_curves, lower = 5, len = 1, any.missing = FALSE)
  checkmate::assert_number(ice_alpha, lower = 0, upper = 1)

  g2 = .autoiml_get_gate_result(result, "G2")
  art = g2$artifacts

  rec_method = art$recommendation$recommended_effect_method %||% "pdp"
  if (method == "auto") method = rec_method

  f = as.character(feature)

  pd = data.table::as.data.table(art$pd_curves)
  ice = data.table::as.data.table(art$ice_curves)
  ale = data.table::as.data.table(art$ale_curves)

  if (!"class_label" %in% names(pd)) pd[, class_label := NA_character_]
  if (!"class_label" %in% names(ice)) ice[, class_label := NA_character_]
  if (!"class_label" %in% names(ale)) ale[, class_label := NA_character_]

  # Pick default class if needed
  if (is.null(class_label)) {
    cl = unique(pd[feature == f]$class_label)
    cl = cl[!is.na(cl)]
    if (length(cl) == 0L) {
      cl = unique(ale[feature == f]$class_label)
      cl = cl[!is.na(cl)]
    }
    class_label = if (length(cl) == 0L) NA_character_ else cl[[1L]]
  }

  # Helper: filter by class_label (supports NA for regression)
  filter_class = function(dt, cl) {
    if (is.na(cl)) {
      dt[is.na(class_label)]
    } else {
      dt[class_label == as.character(cl)]
    }
  }

  if (method == "pdp") {
    pd_f = filter_class(pd[feature == f], class_label)
    ice_f = filter_class(ice[feature == f], class_label)

    if (nrow(pd_f) == 0L) {
      stop("No PDP results found for feature '", f, "'.", call. = FALSE)
    }

    if (show_ice && nrow(ice_f) > 0L && "row_id" %in% names(ice_f)) {
      ids = unique(ice_f$row_id)
      if (length(ids) > ice_max_curves) {
        set.seed(1L)
        ids = sample(ids, ice_max_curves)
      }
      ice_f = ice_f[row_id %in% ids]
    }

    p = ggplot2::ggplot()
    if (show_ice && nrow(ice_f) > 0L) {
      p = p + ggplot2::geom_line(
        data = ice_f,
        ggplot2::aes(x = x, y = yhat, group = row_id),
        alpha = ice_alpha,
        linewidth = 0.3
      )
    }
    p = p + ggplot2::geom_line(
      data = pd_f,
      ggplot2::aes(x = x, y = pd),
      linewidth = 1
    ) +
      ggplot2::labs(
        title = paste0("Gate 2: PDP", if (show_ice) " + ICE" else ""),
        subtitle = paste0("Feature: ", f, if (!is.na(class_label)) paste0(" | Class: ", class_label) else ""),
        x = f,
        y = "Predicted outcome"
      ) +
      ggplot2::theme_minimal()

    return(p)
  }

  # method == "ale"
  ale_f = filter_class(ale[feature == f], class_label)
  if (nrow(ale_f) == 0L) {
    stop("No ALE results found for feature '", f, "'.", call. = FALSE)
  }

  p_ale = ggplot2::ggplot(ale_f, ggplot2::aes(x = x, y = ale)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      title = "Gate 2: ALE (main effect)",
      subtitle = paste0("Feature: ", f, if (!is.na(class_label)) paste0(" | Class: ", class_label) else ""),
      x = f,
      y = "Accumulated local effect"
    ) +
    ggplot2::theme_minimal()

  if (!show_ice) {
    return(p_ale)
  }

  ice_f = filter_class(ice[feature == f], class_label)
  if (nrow(ice_f) == 0L) {
    return(p_ale)
  }

  if ("row_id" %in% names(ice_f)) {
    ids = unique(ice_f$row_id)
    if (length(ids) > ice_max_curves) {
      set.seed(1L)
      ids = sample(ids, ice_max_curves)
    }
    ice_f = ice_f[row_id %in% ids]
  }

  if (ice_center_for_ale == "mean") {
    ice_f[, y_center := yhat - mean(yhat, na.rm = TRUE), by = row_id]
  } else {
    data.table::setorder(ice_f, row_id, x)
    ice_f[, y_center := yhat - yhat[1L], by = row_id]
  }
  mean_cice = ice_f[, .(y_center = mean(y_center, na.rm = TRUE)), by = x]

  p_ice = ggplot2::ggplot() +
    ggplot2::geom_line(
      data = ice_f,
      ggplot2::aes(x = x, y = y_center, group = row_id),
      alpha = ice_alpha,
      linewidth = 0.3
    ) +
    ggplot2::geom_line(
      data = mean_cice,
      ggplot2::aes(x = x, y = y_center),
      linewidth = 1
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      title = "Centered ICE (cICE) around ALE",
      subtitle = paste0("Centering: ", ice_center_for_ale),
      x = f,
      y = "Centered prediction"
    ) +
    ggplot2::theme_minimal()

  p_ale / p_ice
}

#' @keywords internal
.autoiml_plot_g2_gadget = function(result, feature, class_label = NULL, max_regions = 6L) {
  .autoiml_require_pkg("ggplot2")
  .autoiml_require_pkg("patchwork")

  checkmate::assert_string(feature, min.chars = 1)
  checkmate::assert_integerish(max_regions, lower = 1, len = 1, any.missing = FALSE)

  g2 = .autoiml_get_gate_result(result, "G2")
  art = g2$artifacts

  gad = art$gadget[[as.character(feature)]]
  if (is.null(gad)) {
    stop(
      "No GADGET-style regionalization results found for feature '", feature, "'. ",
      "Enable ctx$structure$regionalize and ensure an interaction triggers regionalization.",
      call. = FALSE
    )
  }

  regions = gad$regions
  curves = gad$curves
  if (is.null(regions) || nrow(regions) == 0L || is.null(curves) || nrow(curves) == 0L) {
    stop("GADGET regionalization object is missing required 'regions'/'curves' tables.", call. = FALSE)
  }

  regions = data.table::as.data.table(regions)
  curves = data.table::as.data.table(curves)

  if (!"class_label" %in% names(regions)) regions[, class_label := NA_character_]
  if (!is.null(class_label) && "class_label" %in% names(regions)) {
    cl = as.character(class_label)
    regions = regions[class_label == cl]
    if (nrow(regions) == 0L) {
      stop("No GADGET regions available for requested class_label.", call. = FALSE)
    }
  }

  regions = regions[order(-n)]
  keep = regions[seq_len(min(as.integer(max_regions), nrow(regions)))]
  keep_ids = keep$region_id
  curves = curves[region_id %in% keep_ids]

  p_curves = ggplot2::ggplot(curves, ggplot2::aes(x = x, y = y, group = region_id)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~path, scales = "free_y") +
    ggplot2::labs(
      title = "Gate 2: Regionalized effects (GADGET-style)",
      subtitle = paste0("Feature: ", feature),
      x = feature,
      y = "Region mean curve"
    ) +
    ggplot2::theme_minimal()

  p_sizes = ggplot2::ggplot(keep, ggplot2::aes(x = stats::reorder(path, n), y = n)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Region sizes (top regions)",
      x = NULL,
      y = "n"
    ) +
    ggplot2::theme_minimal()

  p_curves / p_sizes
}
