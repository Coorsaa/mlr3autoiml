.csdg_plot_data_types = c(
  "performance", "calibration", "pfi", "importance_comparison", "model_comparison", "rank_heatmap", "dependence",
  "measurement_trajectory", "setting_generalization", "setting_penalty", "subgroup", "subgroup_contrast",
  "local_fidelity", "gate_status", "explanation_sensitivity",
  "ale", "ice", "interaction", "ale_2d", "multiplicity", "decision_curve"
)

.csdg_plot_data_table = function(x, components = character(), label = "Plot data") {
  if (is.data.frame(x)) return(.as_dt(x))
  for (component in components) {
    value = x[[component]] %||% NULL
    if (is.data.frame(value)) return(.as_dt(value))
  }
  .csdg_stop("%s must be a data frame or contain one of: %s.", label, paste(components, collapse = ", "))
}

.csdg_plot_data_columns = function(x, required, label) {
  missing = setdiff(required, names(x))
  if (length(missing)) {
    .csdg_stop("%s is missing required columns: %s.", label, paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

.csdg_plot_data_column = function(x, candidates, label) {
  found = candidates[candidates %in% names(x)]
  if (!length(found)) {
    .csdg_stop("%s is unavailable; expected one of: %s.", label, paste(candidates, collapse = ", "))
  }
  found[[1L]]
}

.csdg_plot_data_labels = function(values, labels = NULL) {
  values = as.character(values)
  result = if (is.null(labels)) rep(NA_character_, length(values)) else unname(labels[values])
  missing = is.na(result) | !nzchar(result)
  fallback = gsub("_", " ", values[missing])
  fallback = gsub("\\b([A-Za-z]+)([0-9]+)\\b", "\\1 \\2", fallback, perl = TRUE)
  result[missing] = toTitleCase(fallback)
  result
}

.csdg_plot_data_options = function(dots, allowed, type) {
  .assert_named_dots(dots)
  unknown = setdiff(names(dots), allowed)
  if (length(unknown)) {
    .csdg_stop(
      "Unknown option%s for plot type `%s`: %s.",
      if (length(unknown) == 1L) "" else "s",
      type,
      paste(unknown, collapse = ", ")
    )
  }
  dots
}

.csdg_plot_data_option = function(options, name, default = NULL) {
  options[[name]] %||% default
}

.csdg_plot_data_style = function(options) {
  .csdg_plot_data_option(options, "style", "color")
}

.csdg_plot_data_palette = function(options) {
  .autoiml_plot_palette(.csdg_plot_data_style(options))
}

.csdg_plot_data_is_monochrome = function(options) {
  identical(.csdg_plot_data_style(options), "monochrome")
}

.csdg_plot_data_point_shape = function(options) {
  if (.csdg_plot_data_is_monochrome(options)) 21L else 16L
}

.csdg_plot_data_point_fill = function(options, color) {
  if (.csdg_plot_data_is_monochrome(options)) "white" else color
}

.csdg_plot_data_validate_labels = function(labels, name) {
  if (is.null(labels)) return(invisible(TRUE))
  assert_character(labels, any.missing = FALSE, names = "unique", .var.name = name)
  invisible(TRUE)
}

.csdg_plot_data_direct_label = function(values, digits = 3L) {
  formatC(values, digits = digits, format = "fg", flag = "#")
}

.csdg_plot_data_horizontal_labels = function(
    estimate,
    low,
    high,
    labels,
    limits,
    direct_label_size,
    base_size,
    boundary_padding = 0.012) {
  low = ifelse(is.finite(low), pmin(estimate, low), estimate)
  high = ifelse(is.finite(high), pmax(estimate, high), estimate)
  limits = range(limits, finite = TRUE)
  if (diff(limits) <= sqrt(.Machine$double.eps)) {
    half_span = max(abs(limits[[1L]]) * 0.05, 0.5)
    limits = limits + c(-half_span, half_span)
  }
  span = diff(limits)
  size_scale = direct_label_size / (2.9 * base_size / 11)
  label_reserve = span * pmin(
    0.42,
    pmax(0.08, 0.014 * nchar(labels, type = "width") * size_scale)
  )
  gap = 0.018 * span
  left_anchor = low - gap
  right_anchor = high + gap
  left_space = left_anchor - limits[[1L]]
  right_space = limits[[2L]] - right_anchor
  left_fits = left_space >= label_reserve
  right_fits = right_space >= label_reserve
  place_right = (right_fits & !left_fits) |
    (right_fits & left_fits & right_space >= left_space) |
    (!right_fits & !left_fits & right_space >= left_space)
  label_x = ifelse(place_right, right_anchor, left_anchor)
  label_hjust = ifelse(place_right, 0, 1)
  label_edge = label_x + ifelse(place_right, label_reserve, -label_reserve)
  edge_padding = boundary_padding * span
  plot_limits = c(
    min(limits[[1L]], label_edge) - edge_padding,
    max(limits[[2L]], label_edge) + edge_padding
  )

  list(
    x = label_x,
    hjust = label_hjust,
    limits = plot_limits
  )
}

.csdg_plot_data_typography = function(options) {
  element = function(name) {
    value = .csdg_plot_data_option(options, name)
    if (is.null(value)) return(NULL)
    assert_number(value, lower = 1, finite = TRUE, .var.name = name)
    element_text(size = value)
  }
  elements = list(
    axis.text = element("axis_text_size"),
    axis.title = element("axis_title_size"),
    legend.text = element("legend_text_size"),
    legend.title = element("legend_title_size"),
    strip.text = element("strip_text_size")
  )
  do.call(theme, elements[!vapply(elements, is.null, logical(1L))])
}

.csdg_plot_data_performance = function(x, title, subtitle, base_size, options) {
  summary = .csdg_plot_data_table(x, c("summary"), "Performance plot data")
  learner_candidates = intersect(c("learner_id", "learner_name"), names(summary))
  learner_column = if (length(learner_candidates)) learner_candidates[[1L]] else NULL
  measure_column = .csdg_plot_data_column(summary, c("measure_id", "measure"), "Measure column")
  estimate_column = .csdg_plot_data_column(summary, c("mean", "mean_score", "estimate"), "Mean estimate")
  low_column = .csdg_plot_data_column(summary, c("q10", "q10_score"), "Lower descriptive endpoint")
  high_column = .csdg_plot_data_column(summary, c("q90", "q90_score"), "Upper descriptive endpoint")
  learner_labels = .csdg_plot_data_option(options, "learner_labels")
  .csdg_plot_data_validate_labels(learner_labels, "learner_labels")
  direct_label_size = .csdg_plot_data_option(options, "direct_label_size", 2.55 * base_size / 11)
  include_zero = .csdg_plot_data_option(options, "include_zero", FALSE)
  assert_number(direct_label_size, lower = 1, finite = TRUE)
  assert_flag(include_zero)
  summary[, `:=`(
    learner_label = if (is.null(learner_column)) {
      "Focal model"
    } else if (is.null(learner_labels)) {
      .csdg_plot_model_labels(get(learner_column))
    } else {
      .csdg_plot_data_labels(get(learner_column), learner_labels)
    },
    estimate__ = as.numeric(get(estimate_column)),
    low__ = as.numeric(get(low_column)),
    high__ = as.numeric(get(high_column))
  )]
  summary[, ("measure_label") := .csdg_plot_measure_labels(get(measure_column))]
  if (!nrow(summary) || any(!is.finite(summary$estimate__)) || any(!is.finite(summary$low__)) ||
      any(!is.finite(summary$high__)) || any(summary$low__ > summary$estimate__) ||
      any(summary$high__ < summary$estimate__)) {
    .csdg_stop("Performance estimates and descriptive endpoints must be finite and internally ordered.")
  }
  order = summary[, .(order_value = mean(estimate__)), by = learner_label][order(order_value), learner_label]
  summary[, `:=`(
    learner_label = factor(learner_label, levels = order),
    value_label = .csdg_plot_data_direct_label(estimate__, 3L),
    plot_row__ = 1
  )]
  summary[, c("label_x", "label_hjust", "scale_min__", "scale_max__") := {
    endpoints = c(estimate__, low__, high__)
    if (include_zero) endpoints = c(0, endpoints)
    placement = .csdg_plot_data_horizontal_labels(
      estimate = estimate__,
      low = low__,
      high = high__,
      labels = value_label,
      limits = range(endpoints),
      direct_label_size = direct_label_size,
      base_size = base_size
    )
    list(
      placement$x,
      placement$hjust,
      rep(placement$limits[[1L]], .N),
      rep(placement$limits[[2L]], .N)
    )
  }, by = "measure_label"]
  palette = .csdg_plot_data_palette(options)

  ggplot(summary, aes(x = estimate__, y = learner_label)) +
    geom_errorbar(
      aes(xmin = low__, xmax = high__),
      orientation = "y",
      color = palette$metric[["primary"]],
      width = 0.16,
      linewidth = 0.65
    ) +
    geom_point(
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 1.8,
      stroke = 0.55
    ) +
    geom_text(
      aes(x = label_x, label = value_label, hjust = label_hjust),
      size = direct_label_size,
      color = "grey20"
    ) +
    geom_blank(aes(x = scale_min__)) +
    geom_blank(aes(x = scale_max__)) +
    facet_wrap(stats::as.formula("~ measure_label"), scales = "free_x") +
    scale_x_continuous(expand = expansion(mult = 0)) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Held-out metric value"),
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
}

.csdg_calibration_plot_data = function(x, confidence_level) {
  curve = .csdg_plot_data_table(x, c("curve", "calibration"), "Calibration plot data")
  prediction_column = .csdg_plot_data_column(
    curve,
    c("predicted_value", "predicted", "prediction", "mean_prediction"),
    "Calibration prediction"
  )
  observed_column = .csdg_plot_data_column(
    curve,
    c("observed_value", "observed", "estimate", "truth", "event_rate"),
    "Calibration outcome"
  )
  curve[, `:=`(
    predicted_value = as.numeric(get(prediction_column)),
    observed_value = as.numeric(get(observed_column))
  )]
  raw_regression = identical(prediction_column, "prediction") && identical(observed_column, "truth") &&
    !"n" %in% names(curve)
  curve = curve[is.finite(predicted_value) & is.finite(observed_value)]
  if (!nrow(curve)) .csdg_stop("Calibration plot data has no finite bins.")
  if ("n" %in% names(curve)) curve[, n := as.numeric(n)]
  binary = all(curve$predicted_value >= 0 & curve$predicted_value <= 1) &&
    all(curve$observed_value >= 0 & curve$observed_value <= 1) &&
    "n" %in% names(curve) && all(is.finite(curve$n) & curve$n > 0 & curve$n == floor(curve$n))
  if (!"ci_low" %in% names(curve) && "lower" %in% names(curve)) {
    curve[, ci_low := lower]
  }
  if (!"ci_high" %in% names(curve) && "upper" %in% names(curve)) {
    curve[, ci_high := upper]
  }
  interval_presence = c("ci_low", "ci_high") %in% names(curve)
  if (xor(interval_presence[[1L]], interval_presence[[2L]])) {
    .csdg_stop("Calibration intervals must supply both `ci_low` and `ci_high`.")
  }
  interval_columns = all(interval_presence)
  if (interval_columns) {
    curve[, `:=`(
      ci_low = suppressWarnings(as.numeric(as.character(ci_low))),
      ci_high = suppressWarnings(as.numeric(as.character(ci_high)))
    )]
  }
  intervals_all_missing = interval_columns && all(is.na(curve$ci_low)) && all(is.na(curve$ci_high))
  has_intervals = interval_columns && !intervals_all_missing
  if (has_intervals) {
    valid = all(is.finite(curve$ci_low) & is.finite(curve$ci_high)) && all(curve$ci_low <= curve$ci_high)
    if (binary || "n" %in% names(curve)) {
      valid = valid && all(curve$ci_low <= curve$observed_value & curve$ci_high >= curve$observed_value)
    }
    if (!valid) .csdg_stop("Calibration intervals must be finite, ordered, and valid for their estimand.")
  } else if (binary) {
    z = qnorm(1 - (1 - confidence_level) / 2)
    denominator = 1 + z^2 / curve$n
    curve[, `:=`(
      ci_low = pmax(0, (observed_value + z^2 / (2 * n)) / denominator -
        z * sqrt(observed_value * (1 - observed_value) / n + z^2 / (4 * n^2)) / denominator),
      ci_high = pmin(1, (observed_value + z^2 / (2 * n)) / denominator +
        z * sqrt(observed_value * (1 - observed_value) / n + z^2 / (4 * n^2)) / denominator)
    )]
  } else if (!interval_columns) {
    curve[, `:=`(ci_low = NA_real_, ci_high = NA_real_)]
  }
  attr(curve, "raw_regression") = raw_regression
  curve[]
}

.csdg_plot_data_calibration = function(x, title, subtitle, base_size, options) {
  confidence_level = .csdg_plot_data_option(options, "confidence_level", 0.95)
  assert_number(confidence_level, lower = 0, upper = 1, finite = TRUE)
  if (confidence_level <= 0 || confidence_level >= 1) {
    .csdg_stop("`confidence_level` must lie strictly between zero and one.")
  }
  curve = .csdg_calibration_plot_data(x, confidence_level)
  palette = .csdg_plot_data_palette(options)
  if (isTRUE(attr(curve, "raw_regression"))) {
    return(
      ggplot(curve, aes(x = predicted_value, y = observed_value)) +
        geom_point(
          alpha = 0.20,
          color = palette$metric[["primary"]],
          fill = .csdg_plot_data_point_fill(options, palette$metric[["primary"]]),
          shape = .csdg_plot_data_point_shape(options)
        ) +
        geom_smooth(method = "lm", se = FALSE, color = palette$metric[["secondary"]]) +
        labs(
          x = .csdg_plot_data_option(options, "x_label", "Held-out prediction"),
          y = .csdg_plot_data_option(options, "y_label", "Observed outcome"),
          title = title,
          subtitle = subtitle
        ) +
        .csdg_plot_theme(base_size)
    )
  }
  interval_endpoints = c(curve$ci_low, curve$ci_high)
  interval_endpoints = interval_endpoints[is.finite(interval_endpoints)]
  limits = range(c(curve$predicted_value, curve$observed_value, interval_endpoints), finite = TRUE)
  if (all(limits >= 0 & limits <= 1)) {
    limits = c(0, min(1, max(0.10, limits[[2L]] * 1.12)))
  } else if (diff(limits) <= sqrt(.Machine$double.eps)) {
    limits = limits + c(-0.5, 0.5)
  } else {
    limits = limits + c(-0.04, 0.04) * diff(limits)
  }
  has_intervals = all(is.finite(curve$ci_low)) && all(is.finite(curve$ci_high))
  smooth_curve = !"n" %in% names(curve)
  plot = ggplot(curve, aes(x = predicted_value, y = observed_value)) +
    geom_abline(slope = 1, intercept = 0, color = "grey55", linetype = 2, linewidth = 0.5)
  if (has_intervals && smooth_curve) {
    plot = plot + geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high),
      fill = palette$metric[["primary"]],
      alpha = if (.csdg_plot_data_is_monochrome(options)) 0.16 else 0.14,
      color = NA
    )
  } else if (has_intervals) {
    plot = plot + geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      color = palette$metric[["primary"]],
      width = 0.008,
      linewidth = 0.55
    )
  }
  if (smooth_curve) {
    plot = plot + geom_line(color = palette$metric[["primary"]], linewidth = 0.75)
  } else {
    plot = plot + geom_point(
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 1.2,
      stroke = 0.45
    )
  }
  plot +
    coord_equal(xlim = limits, ylim = limits, expand = FALSE) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Mean held-out prediction"),
      y = .csdg_plot_data_option(options, "y_label", "Mean held-out outcome"),
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(legend.position = "none")
}

.csdg_plot_data_pfi = function(x, title, subtitle, base_size, options) {
  summary = .csdg_plot_data_table(x, c("summary"), "PFI plot data")
  feature_column = .csdg_plot_data_column(summary, c("feature_group", "feature"), "PFI feature")
  estimate_column = .csdg_plot_data_column(
    summary,
    c("mean_importance", "mean_across_pv", "mean"),
    "PFI mean"
  )
  low_column = .csdg_plot_data_column(
    summary,
    c("q10_importance", "minimum_across_pv", "q10"),
    "PFI lower endpoint"
  )
  high_column = .csdg_plot_data_column(
    summary,
    c("q90_importance", "maximum_across_pv", "q90"),
    "PFI upper endpoint"
  )
  feature_labels = .csdg_plot_data_option(options, "feature_labels")
  .csdg_plot_data_validate_labels(feature_labels, "feature_labels")
  top_n = .csdg_plot_data_option(options, "top_n", 15L)
  direct_label_size = .csdg_plot_data_option(options, "direct_label_size", 2.9 * base_size / 11)
  assert_int(top_n, lower = 1L)
  assert_number(direct_label_size, lower = 1, finite = TRUE)
  summary[, `:=`(
    feature_label = .csdg_plot_data_labels(get(feature_column), feature_labels),
    estimate__ = as.numeric(get(estimate_column)),
    low__ = as.numeric(get(low_column)),
    high__ = as.numeric(get(high_column))
  )]
  summary = summary[is.finite(estimate__) & is.finite(low__) & is.finite(high__)]
  if (!nrow(summary) || any(summary$low__ > summary$estimate__) || any(summary$high__ < summary$estimate__)) {
    .csdg_stop("PFI estimates and descriptive endpoints must be finite and internally ordered.")
  }
  setorder(summary, -estimate__, feature_label)
  summary = head(summary, top_n)
  summary[, `:=`(
    feature_label = factor(feature_label, levels = rev(feature_label)),
    value_label = .csdg_plot_data_direct_label(estimate__, 3L)
  )]
  label_placement = .csdg_plot_data_horizontal_labels(
    estimate = summary$estimate__,
    low = summary$low__,
    high = summary$high__,
    labels = summary$value_label,
    limits = range(c(0, summary$estimate__, summary$low__, summary$high__)),
    direct_label_size = direct_label_size,
    base_size = base_size
  )
  summary[, `:=`(
    label_x = label_placement$x,
    label_hjust = label_placement$hjust
  )]
  palette = .csdg_plot_data_palette(options)

  ggplot(summary, aes(x = estimate__, y = feature_label)) +
    geom_vline(xintercept = 0, color = "grey65", linetype = 2, linewidth = 0.6) +
    geom_errorbar(
      aes(xmin = low__, xmax = high__),
      orientation = "y",
      color = palette$metric[["primary"]],
      width = 0.28,
      linewidth = 1
    ) +
    geom_point(
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 2.2,
      stroke = 0.55
    ) +
    geom_text(
      aes(x = label_x, label = value_label, hjust = label_hjust),
      size = direct_label_size,
      color = "grey20"
    ) +
    scale_x_continuous(limits = label_placement$limits, expand = expansion(mult = 0)) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Permutation importance"),
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
}

.csdg_plot_data_importance_comparison = function(x, title, subtitle, base_size, options) {
  values = .csdg_plot_data_table(x, c("values", "ranks"), "Importance-comparison plot data")
  learner_column = .csdg_plot_data_column(values, c("learner_name", "learner_id"), "Learner column")
  feature_column = .csdg_plot_data_column(values, c("feature_group", "feature"), "Feature column")
  estimate_column = .csdg_plot_data_column(
    values,
    c("mean_importance", "mean_across_pv", "mean"),
    "Mean importance"
  )
  low_column = .csdg_plot_data_column(
    values,
    c("minimum_importance", "minimum_across_pv", "q10_importance", "q10"),
    "Lower descriptive endpoint"
  )
  high_column = .csdg_plot_data_column(
    values,
    c("maximum_importance", "maximum_across_pv", "q90_importance", "q90"),
    "Upper descriptive endpoint"
  )
  top_n = .csdg_plot_data_option(options, "top_n", 10L)
  learner_order = .csdg_plot_data_option(options, "learner_order")
  feature_labels = .csdg_plot_data_option(options, "feature_labels")
  assert_int(top_n, lower = 1L)
  .csdg_plot_data_validate_labels(feature_labels, "feature_labels")
  if (!is.null(learner_order)) {
    assert_character(learner_order, any.missing = FALSE, unique = TRUE, min.len = 1L)
  }

  values[, `:=`(
    learner_name__ = as.character(get(learner_column)),
    feature_name__ = as.character(get(feature_column)),
    estimate__ = as.numeric(get(estimate_column)),
    low__ = as.numeric(get(low_column)),
    high__ = as.numeric(get(high_column))
  )]
  if (!nrow(values) || any(!is.finite(values$estimate__)) || any(!is.finite(values$low__)) ||
      any(!is.finite(values$high__)) || any(values$low__ > values$estimate__) ||
      any(values$high__ < values$estimate__)) {
    .csdg_stop("Importance estimates and descriptive endpoints must be finite and internally ordered.")
  }
  if (is.null(learner_order)) learner_order = unique(values$learner_name__)
  learner_order = c(
    intersect(learner_order, unique(values$learner_name__)),
    setdiff(unique(values$learner_name__), learner_order)
  )
  if (!length(learner_order)) .csdg_stop("No learners are available for the importance comparison.")

  values[, rank__ := if ("rank" %in% names(values)) {
    as.numeric(rank)
  } else {
    frank(-estimate__, ties.method = "average")
  }, by = learner_name__]
  selected = unique(values[rank__ <= top_n, feature_name__])
  values = values[feature_name__ %in% selected]
  if (!nrow(values)) .csdg_stop("No feature appears within the requested top ranks.")

  focal_order = values[learner_name__ == learner_order[[1L]]][order(estimate__, feature_name__), feature_name__]
  remaining = setdiff(
    values[, .(mean_estimate = mean(estimate__)), by = feature_name__][order(mean_estimate), feature_name__],
    focal_order
  )
  feature_order = c(remaining, focal_order)
  values[, `:=`(
    feature_label = factor(
      .csdg_plot_data_labels(feature_name__, feature_labels),
      levels = .csdg_plot_data_labels(feature_order, feature_labels)
    ),
    learner_label = factor(
      .csdg_plot_model_labels(learner_name__),
      levels = .csdg_plot_model_labels(learner_order)
    )
  )]
  palette = .csdg_plot_data_palette(options)

  ggplot(values, aes(x = estimate__, y = feature_label)) +
    geom_vline(xintercept = 0, color = "grey70", linetype = 2, linewidth = 0.55) +
    geom_errorbar(
      aes(xmin = low__, xmax = high__),
      orientation = "y",
      width = 0.22,
      color = palette$metric[["primary"]],
      linewidth = 0.65
    ) +
    geom_point(
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 2.1,
      stroke = 0.55
    ) +
    facet_grid(stats::as.formula(". ~ learner_label")) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.08))) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Permutation importance"),
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.spacing.x = grid::unit(0.8, "lines")
    )
}

.csdg_plot_data_model_comparison = function(x, title, subtitle, base_size, options) {
  candidates = if (inherits(x, "CSDGRashomon")) .as_dt(x$candidates) else {
    .csdg_plot_data_table(x, c("candidates"), "Model-comparison plot data")
  }
  .csdg_plot_data_columns(
    candidates,
    c("learner_name", "mean_score", "accepted", "acceptance_limit"),
    "Model-comparison plot data"
  )
  direct_label_size = .csdg_plot_data_option(options, "direct_label_size", 2.9 * base_size / 11)
  limit_label_size = .csdg_plot_data_option(options, "limit_label_size", 2.8 * base_size / 11)
  assert_number(direct_label_size, lower = 1, finite = TRUE)
  assert_number(limit_label_size, lower = 1, finite = TRUE)
  candidates[, `:=`(
    learner_label = .csdg_plot_model_labels(learner_name),
    accepted_label = factor(
      ifelse(accepted, "Within tolerance", "Outside tolerance"),
      levels = c("Within tolerance", "Outside tolerance")
    )
  )]
  direction = unique(candidates$direction %||% "minimize")
  direction = direction[!is.na(direction)][[1L]]
  learner_order = if (identical(direction, "maximize")) {
    candidates[order(mean_score), learner_label]
  } else {
    candidates[order(-mean_score), learner_label]
  }
  candidates[, learner_label := factor(learner_label, levels = learner_order)]
  limit = unique(candidates$acceptance_limit[is.finite(candidates$acceptance_limit)])
  if (length(limit) != 1L || any(!is.finite(candidates$mean_score)) || anyNA(candidates$accepted)) {
    .csdg_stop("Model-comparison data must contain finite scores, one acceptance limit, and complete decisions.")
  }
  measure = if ("primary_measure" %in% names(candidates)) candidates$primary_measure[[1L]] else "score"
  reference_learner = .csdg_plot_data_option(options, "reference_learner")
  if (!is.null(reference_learner)) assert_string(reference_learner, min.chars = 1L)
  reference_score = NA_real_
  difference_multiplier = 1
  candidates[, plot_score__ := mean_score]
  if (!is.null(reference_learner)) {
    reference_rows = candidates[learner_name == reference_learner]
    if (nrow(reference_rows) != 1L) {
      .csdg_stop("`reference_learner` must identify exactly one model-comparison candidate.")
    }
    reference_score = reference_rows$mean_score[[1L]]
    difference_multiplier = if (identical(direction, "maximize")) -1 else 1
    candidates[, plot_score__ := difference_multiplier * (mean_score - reference_score)]
    limit = difference_multiplier * (limit - reference_score)
  }
  candidates[, value_label := if (is.null(reference_learner)) {
    .csdg_plot_data_direct_label(plot_score__, 4L)
  } else {
    sprintf("%+.2f", plot_score__)
  }]
  candidates[, `:=`(low__ = NA_real_, high__ = NA_real_)]
  fold_scores = .csdg_plot_data_option(options, "fold_scores")
  if (!is.null(fold_scores)) {
    assert_data_frame(fold_scores, min.rows = 1L, .var.name = "fold_scores")
    fold_scores = .as_dt(fold_scores)
    .csdg_plot_data_columns(fold_scores, c("learner_id", "estimate"), "fold_scores")
    if ("measure_id" %in% names(fold_scores)) fold_scores = fold_scores[measure_id == measure]
    if (!nrow(fold_scores)) .csdg_stop("`fold_scores` has no rows for the model-comparison measure.")
    if (is.null(reference_learner)) {
      ranges = fold_scores[, .(
        low__ = quantile(estimate, 0.10, na.rm = TRUE, names = FALSE),
        high__ = quantile(estimate, 0.90, na.rm = TRUE, names = FALSE)
      ), by = .(learner_name = learner_id)]
    } else {
      fold_keys = intersect(c("outcome_id", "iteration", "repetition", "fold"), names(fold_scores))
      if (!length(fold_keys)) .csdg_stop("Reference-model fold differences require common resampling keys.")
      reference_folds = fold_scores[learner_id == reference_learner, c(fold_keys, "estimate"), with = FALSE]
      setnames(reference_folds, "estimate", "reference_estimate__")
      if (!nrow(reference_folds) || anyDuplicated(reference_folds, by = fold_keys)) {
        .csdg_stop("Reference-model fold scores must be unique on the common resampling keys.")
      }
      differences = merge(fold_scores, reference_folds, by = fold_keys, all.x = TRUE, sort = FALSE)
      if (anyNA(differences$reference_estimate__)) {
        .csdg_stop("Every model-comparison fold score must have a matched reference-model fold.")
      }
      differences[, difference__ := difference_multiplier * (estimate - reference_estimate__)]
      ranges = differences[, .(
        low__ = quantile(difference__, 0.10, na.rm = TRUE, names = FALSE),
        high__ = quantile(difference__, 0.90, na.rm = TRUE, names = FALSE)
      ), by = .(learner_name = learner_id)]
    }
    candidates = merge(candidates, ranges, by = "learner_name", all.x = TRUE, suffixes = c("", ".new"))
    candidates[is.finite(low__.new), low__ := low__.new]
    candidates[is.finite(high__.new), high__ := high__.new]
    candidates[, learner_label := factor(.csdg_plot_model_labels(learner_name), levels = learner_order)]
  }
  palette = .csdg_plot_data_palette(options)
  x_label = .csdg_plot_data_option(options, "x_label")
  if (is.null(x_label)) {
    x_label = if (is.null(reference_learner)) {
      paste("Mean held-out", .csdg_plot_measure_labels(measure))
    } else {
      paste0(
        "Held-out ",
        .csdg_plot_measure_labels(measure),
        " difference from ",
        .csdg_plot_model_labels(reference_learner)
      )
    }
  }
  x_limits = .csdg_plot_data_option(options, "x_limits")
  finite_endpoints = c(limit, candidates$plot_score__, candidates$low__, candidates$high__)
  finite_endpoints = finite_endpoints[is.finite(finite_endpoints)]
  if (is.null(x_limits)) {
    x_limits = range(c(if (!is.null(reference_learner)) 0, finite_endpoints))
  } else {
    assert_numeric(x_limits, finite = TRUE, len = 2L, sorted = TRUE, unique = TRUE)
    tolerance = sqrt(.Machine$double.eps) * max(1, abs(x_limits), abs(finite_endpoints))
    if (x_limits[[1L]] > min(finite_endpoints) + tolerance ||
        x_limits[[2L]] < max(finite_endpoints) - tolerance) {
      .csdg_stop("`x_limits` must contain the acceptance limit, estimates, and descriptive endpoints.")
    }
  }
  label_placement = .csdg_plot_data_horizontal_labels(
    estimate = candidates$plot_score__,
    low = candidates$low__,
    high = candidates$high__,
    labels = candidates$value_label,
    limits = x_limits,
    direct_label_size = direct_label_size,
    base_size = base_size
  )
  candidates[, `:=`(
    label_x__ = label_placement$x,
    label_hjust__ = label_placement$hjust
  )]
  limit_label_hjust = if (limit <= mean(label_placement$limits)) -0.05 else 1.05
  plot = ggplot(candidates, aes(x = plot_score__, y = learner_label)) +
    geom_vline(
      xintercept = if (is.null(reference_learner)) numeric() else 0,
      color = "grey65",
      linewidth = 0.6
    ) +
    geom_vline(xintercept = limit, color = "grey45", linetype = 2, linewidth = 0.8) +
    geom_errorbar(
      aes(xmin = low__, xmax = high__),
      orientation = "y",
      color = palette$metric[["primary"]],
      width = 0.14,
      linewidth = 1,
      na.rm = TRUE
    ) +
    geom_point(aes(shape = accepted_label), color = palette$metric[["secondary"]], size = 2.8) +
    geom_text(
      aes(x = label_x__, label = value_label, hjust = label_hjust__),
      color = "grey20",
      size = direct_label_size
    ) +
    annotate(
      "text",
      x = limit,
      y = Inf,
      label = if (is.null(reference_learner)) {
        paste0("Acceptance limit = ", .csdg_plot_data_direct_label(limit, 4L))
      } else {
        paste0("Tolerance = +", .csdg_plot_data_direct_label(limit, 2L))
      },
      hjust = limit_label_hjust,
      vjust = 1.25,
      color = "grey25",
      size = limit_label_size
    ) +
    scale_shape_manual(values = c("Within tolerance" = 16L, "Outside tolerance" = 4L)) +
    scale_x_continuous(limits = label_placement$limits, expand = expansion(mult = 0)) +
    scale_y_discrete(expand = expansion(add = c(0.35, 0.65))) +
    labs(
      x = x_label,
      y = NULL,
      shape = NULL,
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(panel.grid.major.y = element_blank())
  plot
}

.csdg_plot_data_rank_heatmap = function(x, title, subtitle, base_size, options) {
  ranks = .csdg_plot_data_table(x, c("ranks"), "Rank-heatmap plot data")
  learner_column = .csdg_plot_data_column(ranks, c("learner_name", "learner_id"), "Learner column")
  feature_column = .csdg_plot_data_column(ranks, c("feature_group", "feature"), "Feature column")
  .csdg_plot_data_columns(ranks, "rank", "Rank-heatmap plot data")
  top_n = .csdg_plot_data_option(options, "top_n", 10L)
  feature_labels = .csdg_plot_data_option(options, "feature_labels")
  learner_order = .csdg_plot_data_option(options, "learner_order")
  cell_label_size = .csdg_plot_data_option(options, "cell_label_size", 3.3 * base_size / 11)
  assert_int(top_n, lower = 1L)
  assert_number(cell_label_size, lower = 1, finite = TRUE)
  .csdg_plot_data_validate_labels(feature_labels, "feature_labels")
  if (!is.null(learner_order)) {
    assert_character(learner_order, any.missing = FALSE, unique = TRUE, min.len = 1L)
  }
  ranks[, `:=`(
    learner_name__ = as.character(get(learner_column)),
    feature_name__ = as.character(get(feature_column)),
    rank__ = as.numeric(rank)
  )]
  if (is.null(learner_order)) learner_order = unique(ranks$learner_name__)
  learner_order = c(
    intersect(learner_order, unique(ranks$learner_name__)),
    setdiff(unique(ranks$learner_name__), learner_order)
  )
  if (any(!is.finite(ranks$rank__)) || any(ranks$rank__ < 1)) {
    .csdg_stop("Rank-heatmap ranks must be finite and at least one.")
  }
  selected = unique(ranks[rank__ <= top_n, feature_name__])
  ranks = ranks[feature_name__ %in% selected]
  if (!nrow(ranks)) .csdg_stop("No feature appears within the requested top ranks.")
  feature_order = ranks[, .(mean_rank = mean(rank__)), by = feature_name__][order(-mean_rank), feature_name__]
  ranks[, `:=`(
    feature_label = factor(
      .csdg_plot_data_labels(feature_name__, feature_labels),
      levels = .csdg_plot_data_labels(feature_order, feature_labels)
    ),
    learner_label = factor(
      .csdg_plot_model_labels(learner_name__),
      levels = .csdg_plot_model_labels(learner_order)
    )
  )]
  rank_range = range(ranks$rank__)
  midpoint = mean(rank_range)
  span = max(diff(rank_range), 1)
  palette = .csdg_plot_data_palette(options)
  monochrome = .csdg_plot_data_is_monochrome(options)
  ranks[, label_color := if (monochrome) {
    ifelse((rank__ - rank_range[[1L]]) / span >= 0.58, "white", "grey15")
  } else {
    ifelse(abs(rank__ - midpoint) >= 0.34 * span, "white", "grey15")
  }]
  fill_scale = if (monochrome) {
    scale_fill_gradient(
      low = palette$gradient[["low"]],
      high = palette$gradient[["high"]],
      limits = rank_range
    )
  } else {
    scale_fill_gradient2(
      low = palette$gradient[["low"]],
      mid = "white",
      high = palette$gradient[["high"]],
      midpoint = midpoint
    )
  }

  tile_border = if (monochrome) "grey65" else "white"
  ggplot(ranks, aes(x = learner_label, y = feature_label, fill = rank__)) +
    geom_tile(color = tile_border, linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.0f", rank__), color = label_color), size = cell_label_size) +
    scale_color_identity() +
    fill_scale +
    labs(x = NULL, y = NULL, fill = "PFI rank\n(lower is better)", title = title, subtitle = subtitle) +
    .csdg_plot_theme(base_size) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 20, hjust = 1))
}

.csdg_plot_data_dependence = function(x, title, subtitle, base_size, options) {
  pairwise = .csdg_plot_data_table(x, c("pairwise"), "Dependence plot data")
  .csdg_plot_data_columns(
    pairwise,
    c("feature_1", "feature_2", "association"),
    "Dependence plot data"
  )
  top_n = .csdg_plot_data_option(options, "top_n", 15L)
  feature_labels = .csdg_plot_data_option(options, "feature_labels")
  assert_int(top_n, lower = 2L)
  .csdg_plot_data_validate_labels(feature_labels, "feature_labels")

  pairwise[, association_strength := abs(as.numeric(association))]
  feature_scores = rbindlist(list(
    pairwise[, .(feature = feature_1, association_strength)],
    pairwise[, .(feature = feature_2, association_strength)]
  ))[, .(
    maximum_association = if (any(is.finite(association_strength))) {
      max(association_strength, na.rm = TRUE)
    } else {
      NA_real_
    }
  ), by = feature]
  setorder(feature_scores, -maximum_association, feature)
  shown = head(feature_scores[is.finite(maximum_association), feature], top_n)
  if (length(shown) < 2L) {
    .csdg_stop("Dependence plot data must contain at least two features with finite associations.")
  }

  pairwise = pairwise[
    feature_1 %in% shown & feature_2 %in% shown & is.finite(association_strength)
  ]
  plot_data = rbindlist(list(
    pairwise[, .(feature_1, feature_2, association_strength)],
    pairwise[, .(
      feature_1 = feature_2,
      feature_2 = feature_1,
      association_strength
    )]
  ))
  plot_data[, `:=`(
    feature_1_index = match(feature_1, shown),
    feature_2_index = match(feature_2, shown)
  )]
  plot_data = plot_data[feature_2_index > feature_1_index]
  labels = setNames(.csdg_plot_data_labels(shown, feature_labels), shown)
  plot_data[, `:=`(
    feature_1 = factor(feature_1, levels = shown),
    feature_2 = factor(feature_2, levels = rev(shown))
  )]
  palette = .csdg_plot_data_palette(options)
  tile_border = if (.csdg_plot_data_is_monochrome(options)) "grey65" else "white"

  ggplot(plot_data, aes(x = feature_1, y = feature_2, fill = association_strength)) +
    geom_tile(color = tile_border, linewidth = 0.3) +
    scale_x_discrete(labels = labels, drop = FALSE) +
    scale_y_discrete(labels = labels, drop = FALSE) +
    scale_fill_gradient(
      low = palette$gradient[["low"]],
      high = palette$gradient[["high"]],
      limits = c(0, 1),
      na.value = "grey90"
    ) +
    coord_fixed() +
    labs(
      x = NULL,
      y = NULL,
      fill = "Absolute\nassociation",
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

.csdg_plot_data_measurement_trajectory = function(x, title, subtitle, base_size, options) {
  performance = .csdg_plot_data_table(x, c("summary"), "Measurement-trajectory plot data")
  .csdg_plot_data_columns(
    performance,
    c("outcome_id", "learner_id", "measure_id", "estimate"),
    "Measurement-trajectory plot data"
  )
  learner_labels = .csdg_plot_data_option(options, "learner_labels")
  include_zero = .csdg_plot_data_option(options, "include_zero", FALSE)
  .csdg_plot_data_validate_labels(learner_labels, "learner_labels")
  assert_flag(include_zero)
  outcome_order = .csdg_plot_data_option(options, "outcome_order")
  if (!is.null(outcome_order)) {
    assert_character(outcome_order, any.missing = FALSE, unique = TRUE, min.len = 2L)
  } else {
    outcome_number = suppressWarnings(as.integer(gsub("[^0-9]", "", performance$outcome_id)))
    outcome_order = if (all(!is.na(outcome_number))) {
      unique(as.character(performance$outcome_id[order(outcome_number)]))
    } else {
      unique(as.character(performance$outcome_id))
    }
  }
  performance[, `:=`(
    outcome_label = factor(as.character(outcome_id), levels = outcome_order),
    learner_label = if (is.null(learner_labels)) {
      .csdg_plot_model_labels(learner_id)
    } else {
      .csdg_plot_data_labels(learner_id, learner_labels)
    },
    estimate__ = as.numeric(estimate)
  )]
  performance[, ("measure_label") := .csdg_plot_measure_labels(measure_id)]
  performance = performance[!is.na(outcome_label) & is.finite(estimate__)]
  if (!nrow(performance)) {
    .csdg_stop("Measurement-trajectory plot data has no finite estimates in the requested outcome order.")
  }
  learner_levels = unique(performance$learner_label)
  model_colors = if (.csdg_plot_data_is_monochrome(options)) {
    rep("#111111", length(learner_levels))
  } else {
    rep(
      c("#4C72B0", "#C44E52", "#6E90C9", "#9A6675", "#2F5D9B", "#B77A7D"),
      length.out = length(learner_levels)
    )
  }
  colors = setNames(model_colors, learner_levels)
  shapes = .autoiml_model_shapes(learner_levels)
  linetypes = .autoiml_model_linetypes(learner_levels)

  plot = ggplot(performance, aes(
    x = outcome_label,
    y = estimate__,
    color = learner_label,
    shape = learner_label,
    linetype = learner_label,
    group = learner_label
  )) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.6) +
    facet_wrap(stats::as.formula("~ measure_label"), scales = "free_y") +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    scale_linetype_manual(values = linetypes) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Outcome realization"),
      y = .csdg_plot_data_option(options, "y_label", "Held-out estimate"),
      color = "Model",
      shape = "Model",
      linetype = "Model",
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "bottom",
      legend.justification = "center"
    ) +
    guides(color = guide_legend(nrow = 3L, byrow = TRUE, title.position = "top", title.hjust = 0.5))
  if (include_zero) plot = plot + expand_limits(y = 0)
  plot
}

.csdg_plot_data_setting = function(x, title, subtitle, base_size, options) {
  scores = .csdg_plot_data_table(x, c("scores"), "Setting-generalization plot data")
  measure = .csdg_plot_data_option(options, "measure")
  if (is.null(measure)) {
    candidates = setdiff(names(scores), c("held_out_group", "n_train", "n_assessment"))
    if (!length(candidates)) .csdg_stop("Setting-generalization data has no measure column.")
    measure = candidates[[1L]]
  }
  assert_string(measure, min.chars = 1L)
  .csdg_plot_data_columns(scores, c("held_out_group", measure), "Setting-generalization plot data")
  scores[, value__ := as.numeric(get(measure))]
  scores = scores[is.finite(value__)]
  if (!nrow(scores)) .csdg_stop("Setting-generalization data has no finite scores.")
  scores[, held_out_label := factor(held_out_group, levels = held_out_group[order(value__)])]
  palette = .csdg_plot_data_palette(options)

  ggplot(scores, aes(x = value__, y = held_out_label)) +
    geom_point(
      size = 2.4,
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      stroke = 0.55
    ) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", .csdg_plot_measure_labels(measure)),
      y = "Held-out setting",
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(panel.grid.major.y = element_blank())
}

.csdg_plot_data_setting_penalty = function(x, title, subtitle, base_size, options) {
  summary = .csdg_plot_data_table(x, c("summary"), "Setting-penalty plot data")
  .csdg_plot_data_columns(
    summary,
    c("held_out_group", "mean_penalty", "minimum_penalty", "maximum_penalty"),
    "Setting-penalty plot data"
  )
  summary[, `:=`(
    estimate__ = as.numeric(mean_penalty),
    low__ = as.numeric(minimum_penalty),
    high__ = as.numeric(maximum_penalty)
  )]
  summary = summary[
    !is.na(held_out_group) & is.finite(estimate__) & is.finite(low__) & is.finite(high__)
  ]
  if (!nrow(summary) || any(summary$low__ > summary$estimate__) || any(summary$high__ < summary$estimate__)) {
    .csdg_stop("Setting penalties must be finite and contained by their descriptive endpoints.")
  }
  summary[, held_out_label := factor(held_out_group, levels = held_out_group[order(estimate__)])]
  limits = range(c(0, summary$low__, summary$high__), finite = TRUE)
  span = max(diff(limits), sqrt(.Machine$double.eps))
  palette = .csdg_plot_data_palette(options)

  ggplot(summary, aes(x = estimate__, y = held_out_label)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2, linewidth = 0.7) +
    geom_errorbar(
      aes(xmin = low__, xmax = high__),
      orientation = "y",
      color = palette$metric[["primary"]],
      width = 0.18,
      linewidth = 0.9
    ) +
    geom_point(
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 2.4,
      stroke = 0.55
    ) +
    scale_x_continuous(
      limits = limits + c(-0.04, 0.04) * span,
      expand = expansion(mult = 0)
    ) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Held-out-setting penalty"),
      y = .csdg_plot_data_option(options, "y_label", "Held-out setting"),
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
}

.csdg_plot_data_subgroup_source = function(x, metric) {
  if (!is.list(x) || is.data.frame(x)) {
    .csdg_stop("Subgroup plot data must be a result from `csdg_subgroup_metrics()`.")
  }
  metrics = .csdg_plot_data_table(x, c("metrics"), "Subgroup metrics")
  .csdg_plot_data_columns(metrics, c("audit_variable", "subgroup", "n", metric), "Subgroup metrics")
  metrics[, `:=`(
    subgroup = as.character(subgroup),
    value__ = as.numeric(get(metric)),
    ci_low__ = NA_real_,
    ci_high__ = NA_real_
  )]
  uncertainty = x$uncertainty %||% data.table()
  if (nrow(uncertainty)) {
    metric_name = metric
    uncertainty = .as_dt(uncertainty)[get("metric") == metric_name, .(
      audit_variable,
      subgroup = as.character(subgroup),
      ci_low__ = as.numeric(ci_low),
      ci_high__ = as.numeric(ci_high)
    )]
    metrics = merge(metrics, uncertainty, by = c("audit_variable", "subgroup"), all.x = TRUE,
      suffixes = c("", ".new"))
    metrics[is.finite(ci_low__.new), ci_low__ := ci_low__.new]
    metrics[is.finite(ci_high__.new), ci_high__ := ci_high__.new]
  }
  metrics[]
}

.csdg_plot_data_subgroup = function(x, title, subtitle, base_size, options) {
  metric = .csdg_plot_data_option(options, "metric")
  assert_string(metric, min.chars = 1L)
  metrics = .csdg_plot_data_subgroup_source(x, metric)
  audit_labels = .csdg_plot_data_option(options, "audit_labels")
  subgroup_labels = .csdg_plot_data_option(options, "subgroup_labels")
  .csdg_plot_data_validate_labels(audit_labels, "audit_labels")
  .csdg_plot_data_validate_labels(subgroup_labels, "subgroup_labels")
  metrics[, audit_label := .csdg_plot_data_labels(sub("^audit_", "", audit_variable))]
  if (!is.null(audit_labels)) {
    mapped = unname(audit_labels[metrics$audit_variable])
    metrics[!is.na(mapped), audit_label := mapped[!is.na(mapped)]]
  }
  metrics[, subgroup_label := subgroup]
  if (!is.null(subgroup_labels)) {
    keys = paste(metrics$audit_variable, metrics$subgroup, sep = "::")
    mapped = unname(subgroup_labels[keys])
    metrics[!is.na(mapped), subgroup_label := mapped[!is.na(mapped)]]
  }
  metrics = metrics[is.finite(value__)]
  if (!nrow(metrics)) .csdg_stop("Subgroup data has no finite values for metric `%s`.", metric)
  subgroup_order = .csdg_plot_data_option(options, "subgroup_order")
  if (!is.null(subgroup_order)) {
    assert_character(subgroup_order, any.missing = FALSE, unique = TRUE, min.len = 1L)
  }
  subgroup_order = if (is.null(subgroup_order)) {
    unique(metrics$subgroup_label[order(metrics$value__)])
  } else {
    c(
      intersect(subgroup_order, metrics$subgroup_label),
      setdiff(unique(metrics$subgroup_label), subgroup_order)
    )
  }
  metrics[, subgroup_label := factor(subgroup_label, levels = subgroup_order)]
  direct_label_size = .csdg_plot_data_option(options, "direct_label_size", 2.55 * base_size / 11)
  assert_number(direct_label_size, lower = 1, finite = TRUE)
  metrics[, value_label := sprintf(
    "%.3f (n = %s)",
    value__,
    format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  )]
  x_limits = .csdg_plot_data_option(options, "x_limits")
  if (!is.null(x_limits)) {
    assert_numeric(x_limits, finite = TRUE, len = 2L, sorted = TRUE, unique = TRUE)
  }
  endpoints = c(metrics$value__, metrics$ci_low__, metrics$ci_high__)
  endpoints = endpoints[is.finite(endpoints)]
  axis_limits = x_limits %||% range(endpoints)
  label_placement = .csdg_plot_data_horizontal_labels(
    estimate = metrics$value__,
    low = metrics$ci_low__,
    high = metrics$ci_high__,
    labels = metrics$value_label,
    limits = axis_limits,
    direct_label_size = direct_label_size,
    base_size = base_size,
    boundary_padding = if (is.null(x_limits)) 0.012 else 0
  )
  metrics[, `:=`(
    label_hjust = label_placement$hjust,
    label_x = label_placement$x
  )]
  palette = .csdg_plot_data_palette(options)
  plot = ggplot(metrics, aes(x = value__, y = subgroup_label)) +
    geom_errorbar(
      aes(xmin = ci_low__, xmax = ci_high__),
      orientation = "y",
      color = palette$metric[["primary"]],
      width = 0.09,
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    geom_point(
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 1.9,
      stroke = 0.5
    ) +
    geom_text(
      aes(x = label_x, label = value_label, hjust = label_hjust),
      size = direct_label_size
    ) +
    facet_wrap(~ audit_label, scales = "free_y") +
    labs(
      x = .csdg_plot_data_option(options, "x_label", .csdg_plot_measure_labels(metric)),
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(legend.position = "none")
  plot + scale_x_continuous(limits = label_placement$limits, expand = expansion(mult = 0))
}

.csdg_plot_data_subgroup_contrast = function(x, title, subtitle, base_size, options) {
  metric = .csdg_plot_data_option(options, "metric")
  assert_string(metric, min.chars = 1L)
  metric_name = metric
  contrasts = .csdg_plot_data_table(x, c("contrasts"), "Subgroup-contrast plot data")
  .csdg_plot_data_columns(
    contrasts,
    c("audit_variable", "subgroup_1", "subgroup_2", "metric", "estimate_difference", "ci_low", "ci_high"),
    "Subgroup-contrast plot data"
  )
  contrasts = contrasts[get("metric") == metric_name]
  if (!nrow(contrasts)) .csdg_stop("No subgroup contrasts are available for metric `%s`.", metric)
  audit_labels = .csdg_plot_data_option(options, "audit_labels")
  subgroup_labels = .csdg_plot_data_option(options, "subgroup_labels")
  .csdg_plot_data_validate_labels(audit_labels, "audit_labels")
  .csdg_plot_data_validate_labels(subgroup_labels, "subgroup_labels")
  audit_label = .csdg_plot_data_labels(sub("^audit_", "", contrasts$audit_variable))
  if (!is.null(audit_labels)) {
    mapped = unname(audit_labels[contrasts$audit_variable])
    audit_label[!is.na(mapped)] = mapped[!is.na(mapped)]
  }
  subgroup_label = function(variable, subgroup) {
    keys = paste(variable, subgroup, sep = "::")
    mapped = if (is.null(subgroup_labels)) rep(NA_character_, length(keys)) else unname(subgroup_labels[keys])
    mapped[is.na(mapped)] = as.character(subgroup[is.na(mapped)])
    mapped
  }
  contrasts[, `:=`(
    axis_label = paste0(
      audit_label,
      ": ",
      subgroup_label(audit_variable, subgroup_1),
      " - ",
      subgroup_label(audit_variable, subgroup_2)
    ),
    value_label = .csdg_plot_data_direct_label(estimate_difference, 3L)
  )]
  contrasts = contrasts[
    is.finite(estimate_difference) & is.finite(ci_low) & is.finite(ci_high) &
      ci_low <= estimate_difference & ci_high >= estimate_difference
  ]
  if (!nrow(contrasts)) .csdg_stop("Subgroup contrasts have no valid finite intervals.")
  contrasts[, axis_label := factor(axis_label, levels = rev(axis_label[order(estimate_difference)]))]
  endpoint_range = range(c(0, contrasts$estimate_difference, contrasts$ci_low, contrasts$ci_high))
  span = max(diff(endpoint_range), sqrt(.Machine$double.eps))
  gap = 0.025 * span
  contrasts[, `:=`(
    label_hjust = ifelse(estimate_difference >= 0, 0, 1),
    label_x = ifelse(
      estimate_difference >= 0,
      pmax(estimate_difference, ci_high) + gap,
      pmin(estimate_difference, ci_low) - gap
    )
  )]
  label_reserve = 0.18 * span
  negative_labels = contrasts[label_hjust == 1, label_x]
  positive_labels = contrasts[label_hjust == 0, label_x]
  x_limits = c(
    if (length(negative_labels)) min(endpoint_range[[1L]], negative_labels - label_reserve) else endpoint_range[[1L]],
    if (length(positive_labels)) max(endpoint_range[[2L]], positive_labels + label_reserve) else endpoint_range[[2L]]
  )
  palette = .csdg_plot_data_palette(options)

  ggplot(contrasts, aes(x = estimate_difference, y = axis_label)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2, linewidth = 0.7) +
    geom_errorbar(
      aes(xmin = ci_low, xmax = ci_high),
      orientation = "y",
      color = palette$metric[["primary"]],
      width = 0.14,
      linewidth = 1
    ) +
    geom_point(
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 2.8,
      stroke = 0.6
    ) +
    geom_text(
      aes(x = label_x, label = value_label, hjust = label_hjust),
      size = .csdg_plot_data_option(options, "direct_label_size", 2.9 * base_size / 11),
      color = "grey20"
    ) +
    scale_x_continuous(limits = x_limits, expand = expansion(mult = 0)) +
    labs(
      x = .csdg_plot_data_option(
        options,
        "x_label",
        paste(.csdg_plot_measure_labels(metric), "difference (first - second)")
      ),
      y = NULL,
      title = title,
      subtitle = subtitle,
      caption = .csdg_plot_data_option(options, "caption")
    ) +
    .csdg_plot_theme(base_size) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(hjust = 0, color = "grey35"),
      plot.caption.position = "plot"
    )
}

.csdg_plot_data_local_fidelity = function(x, title, subtitle, base_size, options) {
  source = .csdg_plot_data_table(x, c("cases", "summary"), "Local-fidelity plot data")
  case_column = .csdg_plot_data_column(source, c("case_label", "communication_case"), "Case label")
  value_column = .csdg_plot_data_column(source, c("median_weighted_r2", "weighted_r2"), "Fidelity value")
  threshold_column = .csdg_plot_data_column(
    source,
    c("fidelity_threshold", "minimum_weighted_r2", "historical_weighted_r2_reference"),
    "Fidelity threshold"
  )
  source[, `:=`(
    case_label__ = as.character(get(case_column)),
    value__ = as.numeric(get(value_column)),
    threshold__ = as.numeric(get(threshold_column))
  )]
  thresholds = unique(source$threshold__[is.finite(source$threshold__)])
  if (!nrow(source) || any(!is.finite(source$value__)) || length(thresholds) != 1L ||
      anyNA(source$case_label__) || anyDuplicated(source$case_label__)) {
    .csdg_stop("Local-fidelity plot data must contain unique cases, finite values, and one finite threshold.")
  }
  source[, `:=`(
    case_label__ = factor(case_label__, levels = rev(case_label__)),
    value_label = sprintf("%.4f", value__)
  )]
  finite_values = c(0, 1, source$value__, thresholds)
  span = max(diff(range(finite_values)), sqrt(.Machine$double.eps))
  midpoint = mean(range(finite_values))
  source[, `:=`(
    label_x = value__ + ifelse(value__ > midpoint, -0.025, 0.025) * span,
    label_hjust = ifelse(value__ > midpoint, 1, 0)
  )]
  x_limits = range(c(finite_values, source$label_x)) + c(-0.03, 0.03) * span
  palette = .csdg_plot_data_palette(options)

  ggplot(source, aes(x = value__, y = case_label__)) +
    geom_vline(xintercept = 0, color = "grey75", linewidth = 0.5) +
    geom_vline(xintercept = thresholds[[1L]], color = "grey45", linetype = 2, linewidth = 0.8) +
    geom_point(
      size = 3,
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      stroke = 0.6
    ) +
    geom_text(
      aes(x = label_x, label = value_label, hjust = label_hjust),
      size = .csdg_plot_data_option(options, "direct_label_size", 3.2 * base_size / 11)
    ) +
    scale_x_continuous(limits = x_limits) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Cross-fitted weighted local R-squared"),
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size)
}

.csdg_plot_data_gate_status = function(x, title, subtitle, base_size, options) {
  source = data.table::copy(.csdg_plot_data_table(x, c("report_card", "gates"), "Gate-status plot data"))
  .csdg_plot_data_columns(source, c("gate_id", "status"), "Gate-status plot data")
  # Current statuses and, for stored report cards of versions up to 0.1.5, the legacy statuses (mapped to the
  # labels of the article without a warning).
  status_labels = c(
    supported = "Supported",
    contradicted = "Contradicted",
    open = "Open",
    not_required = "Not required",
    context = "Context",
    error = "Error",
    met = "Supported",
    unresolved = "Open",
    not_met = "Contradicted",
    not_applicable = "Not required",
    pass = "Pass",
    warn = "Warn",
    fail = "Fail",
    skip = "Skip"
  )
  status_keys = c(
    supported = "pass",
    contradicted = "fail",
    open = "warn",
    not_required = "skip",
    context = "skip",
    error = "error",
    met = "pass",
    unresolved = "warn",
    not_met = "fail",
    not_applicable = "skip",
    pass = "pass",
    warn = "warn",
    fail = "fail",
    skip = "skip"
  )
  source[, status := as.character(status)]
  # A gate reported as context has no property status; it is labeled "Context" whatever its computed result.
  if ("evidence_role" %in% names(source)) {
    source[evidence_role %in% "context" & status %in% c(.csdg_property_statuses, "error"), status := "context"]
  }
  source[, `:=`(
    gate_id = as.character(gate_id),
    status_label = unname(status_labels[as.character(status)]),
    status_key = unname(status_keys[as.character(status)])
  )]
  if (!nrow(source) || anyNA(source$gate_id) || any(!nzchar(source$gate_id)) ||
      anyNA(source$status_label) || anyNA(source$status_key)) {
    .csdg_stop("Gate-status data must contain non-empty gate IDs and supported status values.")
  }

  group_column = intersect(c("study", "group", "case"), names(source))
  group_column = if (length(group_column)) group_column[[1L]] else NULL
  source[, group_id__ := if (is.null(group_column)) "Gate status" else as.character(get(group_column))]
  if (anyNA(source$group_id__) || any(!nzchar(source$group_id__))) {
    .csdg_stop("Gate-status groups must be non-empty when supplied.")
  }
  if (anyDuplicated(source[, .(group_id__, gate_id)])) {
    .csdg_stop("Gate-status data must contain one row per group and gate ID.")
  }

  gate_order = .csdg_plot_data_option(options, "gate_order")
  if (is.null(gate_order)) {
    gate_order = unique(source$gate_id)
  } else {
    assert_character(gate_order, any.missing = FALSE, unique = TRUE, min.len = 1L, .var.name = "gate_order")
    gate_order = c(intersect(gate_order, source$gate_id), setdiff(unique(source$gate_id), gate_order))
  }
  group_order = .csdg_plot_data_option(options, "group_order")
  if (is.null(group_order)) {
    group_order = unique(source$group_id__)
  } else {
    assert_character(group_order, any.missing = FALSE, unique = TRUE, min.len = 1L, .var.name = "group_order")
    group_order = c(intersect(group_order, source$group_id__), setdiff(unique(source$group_id__), group_order))
  }
  group_labels = .csdg_plot_data_option(options, "group_labels")
  .csdg_plot_data_validate_labels(group_labels, "group_labels")
  source[, ("group_label") := .csdg_plot_data_labels(group_id__, group_labels)]
  group_label_order = source[["group_label"]][match(group_order, source$group_id__)]
  if (anyNA(group_label_order) || any(!nzchar(group_label_order)) || anyDuplicated(group_label_order)) {
    .csdg_stop("`group_labels` must map each gate-status group to a unique non-empty label.")
  }
  if (length(group_order) > 1L && nrow(source) != length(group_order) * length(gate_order)) {
    .csdg_stop("Multi-group gate-status data must contain a complete group-by-gate grid.")
  }
  if (length(group_order) > 1L) {
    group_gate_counts = source[, .N, by = group_id__]
    gate_group_counts = source[, .N, by = gate_id]
    if (any(group_gate_counts$N != length(gate_order)) || any(gate_group_counts$N != length(group_order))) {
      .csdg_stop("Multi-group gate-status data must contain a complete group-by-gate grid.")
    }
  }
  source[, `:=`(
    gate_factor__ = factor(gate_id, levels = gate_order),
    group_factor__ = factor(get("group_label"), levels = group_label_order)
  )]
  palette = .csdg_plot_data_palette(options)
  monochrome = .csdg_plot_data_is_monochrome(options)
  status_colors = palette$status[c("pass", "warn", "fail", "skip", "error")]
  status_point_colors = if (monochrome) {
    setNames(rep("#111111", length(status_colors)), names(status_colors))
  } else {
    status_colors
  }
  status_text_colors = if (monochrome) {
    c(pass = "grey12", warn = "grey12", fail = "white", skip = "grey12", error = "white")
  } else {
    c(pass = "white", warn = "grey12", fail = "white", skip = "grey12", error = "white")
  }
  tile_border = if (monochrome) "grey55" else "white"
  cell_label_size = .csdg_plot_data_option(options, "cell_label_size", 3.3 * base_size / 11)
  multiple_groups = uniqueN(source$group_factor__) > 1L

  if (!multiple_groups) {
    gate_names = if ("gate_name" %in% names(source)) as.character(source$gate_name) else rep("", nrow(source))
    source[, gate_label__ := ifelse(
      !is.na(gate_names) & nzchar(gate_names),
      paste(gate_id, gate_names, sep = " - "),
      gate_id
    )]
    gate_label_order = source$gate_label__[match(gate_order, source$gate_id)]
    source[, gate_label__ := factor(gate_label__, levels = rev(gate_label_order))]
    return(
      ggplot(source, aes(x = 0, y = gate_label__, color = status_key, shape = status_key)) +
        geom_point(size = 3.2, show.legend = FALSE) +
        geom_text(
          aes(x = 0.04, label = status_label),
          hjust = 0,
          color = "grey20",
          size = cell_label_size
        ) +
        scale_color_manual(values = status_point_colors) +
        scale_shape_manual(values = .autoiml_status_shapes) +
        scale_x_continuous(limits = c(-0.015, 0.42), breaks = NULL, expand = expansion(mult = 0)) +
        labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
        .csdg_plot_theme(base_size) +
        theme(panel.grid = element_blank())
    )
  }

  ggplot(source, aes(x = gate_factor__, y = group_factor__, fill = status_key)) +
    geom_tile(color = tile_border, linewidth = 1.1, height = 0.82) +
    geom_text(aes(label = status_label, color = status_key), size = cell_label_size, lineheight = 0.9) +
    scale_fill_manual(values = status_colors, guide = "none") +
    scale_color_manual(values = status_text_colors, guide = "none") +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Diagnostic gate"),
      y = .csdg_plot_data_option(options, "y_label"),
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(panel.grid = element_blank())
}

.csdg_plot_data_explanation_sensitivity_aligned = function(
    source,
    group_label_order,
    title,
    subtitle,
    base_size,
    options) {
  include_focal = .csdg_plot_data_option(options, "include_focal", TRUE)
  reference_learner = .csdg_plot_data_option(options, "reference_learner")
  learner_order = .csdg_plot_data_option(options, "learner_order")
  assert_flag(include_focal)
  if (!is.null(reference_learner)) assert_string(reference_learner, min.chars = 1L)
  if (!is.null(learner_order)) {
    assert_character(learner_order, any.missing = FALSE, unique = TRUE, min.len = 1L)
  }
  if (!include_focal) {
    if (is.null(reference_learner)) {
      .csdg_stop("`reference_learner` is required when `include_focal = FALSE`.")
    }
    source = source[learner_id__ != reference_learner]
  }
  if (!nrow(source)) .csdg_stop("The aligned explanation-sensitivity plot has no learner rows to display.")

  y_limits = .csdg_plot_data_option(options, "y_limits")
  if (!is.null(y_limits)) {
    .csdg_stop("`y_limits` applies only to the scatter explanation-sensitivity display.")
  }
  x_limits = .csdg_plot_data_option(options, "x_limits")
  plotted_values = c(source$performance_fraction__, source$reversal_fraction__)
  if (is.null(x_limits)) {
    upper = min(1, max(0.10, ceiling(20 * 1.12 * max(plotted_values)) / 20))
    x_limits = c(0, upper)
  } else {
    assert_numeric(x_limits, finite = TRUE, len = 2L, sorted = TRUE, unique = TRUE, .var.name = "x_limits")
  }
  if (x_limits[[1L]] > 0 || x_limits[[2L]] < max(plotted_values)) {
    .csdg_stop("`x_limits` must include zero and every displayed explanation-sensitivity fraction.")
  }

  learner_ids = unique(source$learner_id__)
  if (is.null(learner_order)) {
    learner_order = learner_ids
  } else {
    learner_order = c(intersect(learner_order, learner_ids), setdiff(learner_ids, learner_order))
  }
  learner_labels = source$learner_label__[match(learner_order, source$learner_id__)]
  learner_levels = rev(unique(learner_labels))
  source[, learner_label__ := factor(learner_label__, levels = learner_levels)]
  metric_levels = c("Tolerance consumed", "PFI rank reversals")
  performance = source[, .(
    group_label__,
    learner_label__,
    metric_label__ = factor(metric_levels[[1L]], levels = metric_levels),
    value__ = performance_fraction__
  )]
  reversals = source[, .(
    group_label__,
    learner_label__,
    metric_label__ = factor(metric_levels[[2L]], levels = metric_levels),
    value__ = reversal_fraction__
  )]
  plot_data = rbindlist(list(performance, reversals), use.names = TRUE)
  monochrome = .csdg_plot_data_is_monochrome(options)
  palette = .csdg_plot_data_palette(options)
  fills = if (monochrome) {
    setNames(c("white", "grey20"), metric_levels)
  } else {
    setNames(c(palette$metric[["primary"]], palette$metric[["secondary"]]), metric_levels)
  }

  plot = ggplot(plot_data, aes(x = value__, y = learner_label__, shape = metric_label__, fill = metric_label__)) +
    geom_point(
      data = plot_data[metric_label__ == metric_levels[[1L]]],
      position = position_nudge(y = 0.12),
      color = "grey10",
      size = 3.2,
      stroke = 0.7
    ) +
    geom_point(
      data = plot_data[metric_label__ == metric_levels[[2L]]],
      position = position_nudge(y = -0.12),
      color = "grey10",
      size = 3.2,
      stroke = 0.7
    ) +
    scale_shape_manual(values = setNames(c(21L, 22L), metric_levels)) +
    scale_fill_manual(values = fills) +
    scale_x_continuous(
      limits = x_limits,
      breaks = function(limits) pretty(limits, n = 4L),
      labels = function(value) paste0(round(100 * value), "%"),
      expand = expansion(mult = c(0.01, 0.03))
    ) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Fraction of the prespecified reference quantity"),
      y = NULL,
      shape = NULL,
      fill = NULL,
      title = title,
      subtitle = subtitle,
      caption = .csdg_plot_data_option(options, "caption")
    ) +
    .csdg_plot_theme(base_size) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    guides(
      shape = guide_legend(order = 1L, override.aes = list(size = 3.2)),
      fill = "none"
    )
  if (length(group_label_order) > 1L) {
    plot = plot + facet_wrap(~ group_label__, nrow = 1L, scales = "free_y")
  }
  plot
}

.csdg_plot_data_explanation_sensitivity_diverging = function(
    source,
    group_label_order,
    title,
    subtitle,
    base_size,
    options) {
  include_focal = .csdg_plot_data_option(options, "include_focal", TRUE)
  reference_learner = .csdg_plot_data_option(options, "reference_learner")
  learner_order = .csdg_plot_data_option(options, "learner_order")
  bar_width = .csdg_plot_data_option(options, "bar_width", 0.38)
  assert_flag(include_focal)
  if (!is.null(reference_learner)) assert_string(reference_learner, min.chars = 1L)
  if (!is.null(learner_order)) {
    assert_character(learner_order, any.missing = FALSE, unique = TRUE, min.len = 1L)
  }
  assert_number(bar_width, lower = 0.05, upper = 0.9, finite = TRUE, .var.name = "bar_width")
  if (!include_focal) {
    if (is.null(reference_learner)) {
      .csdg_stop("`reference_learner` is required when `include_focal = FALSE`.")
    }
    source = source[learner_id__ != reference_learner]
  }
  if (!nrow(source)) .csdg_stop("The diverging explanation-sensitivity plot has no learner rows to display.")

  y_limits = .csdg_plot_data_option(options, "y_limits")
  if (!is.null(y_limits)) {
    .csdg_stop("`y_limits` applies only to the scatter explanation-sensitivity display.")
  }
  x_limits = .csdg_plot_data_option(options, "x_limits")
  if (is.null(x_limits)) {
    limit = min(1, max(0.10, ceiling(20 * 1.12 * max(
      source$performance_fraction__, source$reversal_fraction__
    )) / 20))
    x_limits = c(-limit, limit)
  } else {
    assert_numeric(x_limits, finite = TRUE, len = 2L, sorted = TRUE, unique = TRUE, .var.name = "x_limits")
  }
  if (x_limits[[1L]] > -max(source$reversal_fraction__) ||
      x_limits[[2L]] < max(source$performance_fraction__) ||
      x_limits[[1L]] >= 0 || x_limits[[2L]] <= 0) {
    .csdg_stop(
      "`x_limits` must straddle zero and include every displayed performance and rank-reversal fraction."
    )
  }

  learner_ids = unique(source$learner_id__)
  if (is.null(learner_order)) {
    learner_order = learner_ids
  } else {
    learner_order = c(intersect(learner_order, learner_ids), setdiff(learner_ids, learner_order))
  }
  learner_labels = source$learner_label__[match(learner_order, source$learner_id__)]
  source[, learner_label__ := factor(learner_label__, levels = rev(unique(learner_labels)))]
  source[, `:=`(
    performance_display__ = performance_fraction__,
    reversal_display__ = -reversal_fraction__
  )]
  palette = .csdg_plot_data_palette(options)
  reversal_fill = if (.csdg_plot_data_is_monochrome(options)) "grey25" else palette$metric[["secondary"]]

  plot = ggplot(source, aes(y = learner_label__)) +
    geom_vline(xintercept = 0, color = "grey25", linewidth = 0.75) +
    geom_col(
      aes(x = reversal_display__),
      width = bar_width,
      color = "grey15",
      fill = reversal_fill,
      linewidth = 0.55
    ) +
    geom_col(
      aes(x = performance_display__),
      width = bar_width,
      color = "grey15",
      fill = "white",
      linewidth = 0.55
    ) +
    scale_x_continuous(
      limits = x_limits,
      breaks = function(limits) pretty(limits, n = 5L),
      labels = function(value) paste0(round(100 * abs(value)), "%"),
      expand = expansion(mult = c(0.035, 0.035))
    ) +
    labs(
      x = .csdg_plot_data_option(
        options,
        "x_label",
        "PFI order reversals  \u2190  Fraction  \u2192  Performance tolerance used"
      ),
      y = NULL,
      title = title,
      subtitle = subtitle,
      caption = .csdg_plot_data_option(options, "caption")
    ) +
    .csdg_plot_theme(base_size) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(margin = margin(r = 9)),
      legend.position = "none",
      plot.margin = margin(10, 20, 14, 22)
    )
  if (length(group_label_order) > 1L) {
    plot = plot + facet_wrap(~ group_label__, ncol = 1L, scales = "free_y")
  }
  plot
}

.csdg_plot_data_explanation_sensitivity = function(x, title, subtitle, base_size, options) {
  source = .csdg_plot_data_table(x, c("sensitivity", "summary"), "Explanation-sensitivity plot data")
  learner_column = .csdg_plot_data_column(source, c("learner_name", "learner_id"), "Learner name")
  x_column = .csdg_plot_data_column(
    source,
    c("performance_tolerance_fraction", "fraction_of_prespecified_tolerance"),
    "Performance-tolerance fraction"
  )
  y_column = .csdg_plot_data_column(
    source,
    c("rank_discordance_vs_focal", "full_pair_reversal_rate_vs_focal"),
    "Rank-reversal fraction"
  )
  if ("accepted" %in% names(source)) {
    assert_logical(source$accepted, any.missing = FALSE, .var.name = "x$accepted")
    source = source[accepted == TRUE]
  }
  source[, `:=`(
    learner_id__ = as.character(get(learner_column)),
    performance_fraction__ = as.numeric(get(x_column)),
    reversal_fraction__ = as.numeric(get(y_column))
  )]
  tolerance = sqrt(.Machine$double.eps)
  if (!nrow(source) || anyNA(source$learner_id__) || any(!nzchar(source$learner_id__)) ||
      any(!is.finite(source$performance_fraction__)) || any(!is.finite(source$reversal_fraction__)) ||
      any(source$performance_fraction__ < -tolerance | source$performance_fraction__ > 1 + tolerance) ||
      any(source$reversal_fraction__ < -tolerance | source$reversal_fraction__ > 1 + tolerance)) {
    .csdg_stop("Explanation-sensitivity fractions must be finite and lie between zero and one.")
  }
  source[, `:=`(
    performance_fraction__ = pmin(pmax(performance_fraction__, 0), 1),
    reversal_fraction__ = pmin(pmax(reversal_fraction__, 0), 1)
  )]
  group_column = intersect(c("study", "group", "case"), names(source))
  group_column = if (length(group_column)) group_column[[1L]] else NULL
  source[, group_id__ := if (is.null(group_column)) "Model set" else as.character(get(group_column))]
  if (anyNA(source$group_id__) || any(!nzchar(source$group_id__)) ||
      anyDuplicated(source[, .(group_id__, learner_id__)])) {
    .csdg_stop("Explanation-sensitivity data must contain one learner row per non-empty group.")
  }

  learner_labels = .csdg_plot_data_option(options, "learner_labels")
  .csdg_plot_data_validate_labels(learner_labels, "learner_labels")
  source[, learner_label__ := if (!is.null(learner_labels)) {
    .csdg_plot_data_labels(learner_id__, learner_labels)
  } else if ("learner_label" %in% names(source)) {
    as.character(learner_label)
  } else {
    .csdg_plot_model_labels(learner_id__)
  }]
  if (anyNA(source$learner_label__) || any(!nzchar(source$learner_label__))) {
    .csdg_stop("Explanation-sensitivity learner labels must be non-empty.")
  }

  group_order = .csdg_plot_data_option(options, "group_order")
  if (is.null(group_order)) {
    group_order = unique(source$group_id__)
  } else {
    assert_character(group_order, any.missing = FALSE, unique = TRUE, min.len = 1L, .var.name = "group_order")
    group_order = c(intersect(group_order, source$group_id__), setdiff(unique(source$group_id__), group_order))
  }
  group_labels = .csdg_plot_data_option(options, "group_labels")
  .csdg_plot_data_validate_labels(group_labels, "group_labels")
  source[, group_label__ := .csdg_plot_data_labels(group_id__, group_labels)]
  group_label_order = source$group_label__[match(group_order, source$group_id__)]
  if (anyNA(group_label_order) || any(!nzchar(group_label_order)) || anyDuplicated(group_label_order)) {
    .csdg_stop("`group_labels` must map each explanation-sensitivity group to a unique non-empty label.")
  }
  source[, group_label__ := factor(group_label__, levels = group_label_order)]

  display = .csdg_plot_data_option(options, "display", "scatter")
  assert_choice(display, c("scatter", "aligned", "diverging"), .var.name = "display")
  if (!identical(display, "diverging") && !is.null(options$bar_width)) {
    .csdg_stop("`bar_width` applies only to the diverging explanation-sensitivity display.")
  }
  if (identical(display, "aligned")) {
    return(.csdg_plot_data_explanation_sensitivity_aligned(
      source,
      group_label_order,
      title,
      subtitle,
      base_size,
      options
    ))
  }
  if (identical(display, "diverging")) {
    return(.csdg_plot_data_explanation_sensitivity_diverging(
      source,
      group_label_order,
      title,
      subtitle,
      base_size,
      options
    ))
  }

  x_limits = .csdg_plot_data_option(options, "x_limits", c(0, 1.03))
  assert_numeric(x_limits, finite = TRUE, len = 2L, sorted = TRUE, unique = TRUE, .var.name = "x_limits")
  if (x_limits[[1L]] > 0 || x_limits[[2L]] < 1) {
    .csdg_stop("`x_limits` must include the complete prespecified zero-to-one tolerance region.")
  }
  y_limits = .csdg_plot_data_option(options, "y_limits")
  if (is.null(y_limits)) {
    upper = min(1, max(0.1, ceiling(20 * 1.12 * max(source$reversal_fraction__)) / 20))
    y_limits = c(0, upper)
  } else {
    assert_numeric(y_limits, finite = TRUE, len = 2L, sorted = TRUE, unique = TRUE, .var.name = "y_limits")
  }
  if (y_limits[[1L]] > 0 || y_limits[[2L]] < max(source$reversal_fraction__)) {
    .csdg_stop("`y_limits` must include zero and every rank-reversal fraction.")
  }

  x_span = diff(x_limits)
  y_span = diff(y_limits)
  source[, label_hjust__ := ifelse(performance_fraction__ > x_limits[[2L]] - 0.18 * x_span, 1, 0)]
  source[, `:=`(
    label_x__ = performance_fraction__ + ifelse(label_hjust__ == 1, -0.012, 0.012) * x_span,
    label_y__ = pmin(reversal_fraction__ + 0.025 * y_span, y_limits[[2L]] - 0.01 * y_span)
  )]
  palette = .csdg_plot_data_palette(options)
  default_colors = c(
    palette$metric[["primary"]],
    palette$metric[["secondary"]],
    palette$metric[["tertiary"]],
    palette$metric[["quaternary"]]
  )
  group_colors = .csdg_plot_data_option(options, "group_colors")
  if (!is.null(group_colors)) .csdg_plot_data_validate_labels(group_colors, "group_colors")
  if (.csdg_plot_data_is_monochrome(options)) {
    group_colors = setNames(rep("#111111", length(group_label_order)), group_label_order)
  } else if (is.null(group_colors)) {
    group_colors = setNames(rep(default_colors, length.out = length(group_label_order)), group_label_order)
  } else {
    original_complete = all(group_order %in% names(group_colors))
    label_complete = all(group_label_order %in% names(group_colors))
    if (original_complete && label_complete) {
      original_colors = unname(group_colors[group_order])
      label_colors = unname(group_colors[group_label_order])
      if (!identical(original_colors, label_colors)) {
        .csdg_stop("`group_colors` is ambiguous between original group IDs and mapped group labels.")
      }
      group_colors = setNames(original_colors, group_label_order)
    } else if (original_complete) {
      group_colors = setNames(unname(group_colors[group_order]), group_label_order)
    } else if (label_complete) {
      group_colors = group_colors[group_label_order]
    } else {
      missing_ids = setdiff(group_order, names(group_colors))
      missing_labels = setdiff(group_label_order, names(group_colors))
      .csdg_stop(
        "`group_colors` must cover every original group ID or every mapped label; missing IDs [%s], labels [%s].",
        paste(missing_ids, collapse = ", "),
        paste(missing_labels, collapse = ", ")
      )
    }
  }
  direct_label_size = .csdg_plot_data_option(options, "direct_label_size", 3.1 * base_size / 11)
  monochrome = .csdg_plot_data_is_monochrome(options)
  point_layer = if (monochrome) {
    geom_point(aes(shape = group_label__), size = 3.1)
  } else {
    geom_point(size = 3.1)
  }

  plot = ggplot(source, aes(
    x = performance_fraction__,
    y = reversal_fraction__,
    color = group_label__
  )) +
    annotate("rect", xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf, fill = palette$surface[["panel"]], alpha = 0.45) +
    geom_vline(xintercept = 1, color = "grey45", linetype = 2, linewidth = 0.75) +
    point_layer +
    geom_text(
      aes(x = label_x__, y = label_y__, label = learner_label__, hjust = label_hjust__),
      show.legend = FALSE,
      size = direct_label_size
    ) +
    scale_color_manual(values = group_colors, guide = "none") +
    scale_x_continuous(
      limits = x_limits,
      breaks = seq(0, 1, by = 0.25),
      labels = function(value) paste0(round(100 * value), "%"),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = pretty(y_limits, n = 4L),
      labels = function(value) paste0(round(100 * value), "%"),
      expand = expansion(mult = c(0.025, 0.025))
    ) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Performance tolerance consumed"),
      y = .csdg_plot_data_option(options, "y_label", "Rank reversals versus focal model"),
      title = title,
      subtitle = subtitle,
      caption = .csdg_plot_data_option(options, "caption")
    ) +
    .csdg_plot_theme(base_size) +
    theme(panel.grid.minor = element_blank())
  if (monochrome) {
    plot = plot + scale_shape_manual(values = .autoiml_model_shapes(group_label_order), guide = "none")
  }
  if (uniqueN(source$group_label__) > 1L) {
    plot = plot + facet_wrap(~ group_label__, nrow = 1L)
  }
  plot
}

.csdg_plot_data_ale = function(x, title, subtitle, base_size, options) {
  ale = .csdg_plot_data_table(x, character(), "One-dimensional ALE plot data")
  .csdg_plot_data_columns(ale, c("feature", "x_left", "x_right", "x", "ale", "n_interval"), "ALE data")
  feature_labels = .csdg_plot_data_option(options, "feature_labels")
  .csdg_plot_data_validate_labels(feature_labels, "feature_labels")
  if (!"supported" %in% names(ale)) ale[, supported := is.finite(get("ale"))]
  interval_columns = c("ale_lower", "ale_upper")
  has_interval = all(interval_columns %in% names(ale))
  if (any(interval_columns %in% names(ale)) && !has_interval) {
    .csdg_stop("ALE interval data must contain both `ale_lower` and `ale_upper`.")
  }
  if (has_interval) {
    ale[, (interval_columns) := lapply(.SD, as.numeric), .SDcols = interval_columns]
    if (!"interval_supported" %in% names(ale)) {
      ale[, ("interval_supported") := is.finite(ale_lower) & is.finite(ale_upper)]
    }
    if (!is.logical(ale$interval_supported) || anyNA(ale$interval_supported) ||
        any(ale$interval_supported & !ale$supported)) {
      .csdg_stop("`interval_supported` must be a complete logical subset of supported ALE intervals.")
    }
    invalid_interval = ale$interval_supported & (
      !is.finite(ale$ale_lower) | !is.finite(ale$ale_upper) | ale$ale_lower > ale$ale_upper
    )
    if (any(invalid_interval)) {
      .csdg_stop("Supported ALE intervals must have finite ordered lower and upper limits.")
    }
    ale[get("interval_supported") == FALSE, (interval_columns) := list(NA_real_, NA_real_)]
  }
  ale[, feature_label := .csdg_plot_data_labels(feature, feature_labels)]
  setorder(ale, feature_label, x)
  ale[, segment_id__ := rleid(supported), by = feature_label]
  finite_values = ale$ale[is.finite(ale$ale)]
  if (has_interval) {
    finite_values = c(
      finite_values,
      ale$ale_lower[is.finite(ale$ale_lower)],
      ale$ale_upper[is.finite(ale$ale_upper)]
    )
  }
  y_limit = if (length(finite_values)) max(abs(finite_values)) else 0
  y_limit = if (!is.finite(y_limit) || y_limit <= sqrt(.Machine$double.eps)) 0.01 else 1.08 * y_limit
  palette = .csdg_plot_data_palette(options)
  interval_layer = if (has_interval) {
    geom_ribbon(
      data = ale[supported == TRUE & get("interval_supported") == TRUE],
      aes(
        ymin = ale_lower,
        ymax = ale_upper,
        group = interaction(feature_label, segment_id__)
      ),
      fill = if (.csdg_plot_data_is_monochrome(options)) "grey70" else palette$metric[["primary"]],
      alpha = if (.csdg_plot_data_is_monochrome(options)) 0.50 else 0.20,
      color = NA
    )
  }

  ggplot(ale, aes(x = x, y = ale)) +
    geom_rect(
      data = ale[supported == FALSE],
      aes(xmin = x_left, xmax = x_right, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = "grey93",
      color = NA
    ) +
    interval_layer +
    geom_hline(yintercept = 0, color = "grey55", linetype = 2, linewidth = 0.6) +
    geom_line(
      data = ale[supported == TRUE],
      aes(group = interaction(feature_label, segment_id__)),
      color = palette$metric[["primary"]],
      linewidth = 0.9
    ) +
    geom_point(
      data = ale[supported == TRUE],
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 2.2,
      stroke = 0.5
    ) +
    facet_wrap(~ feature_label, scales = "free_x") +
    scale_y_continuous(limits = c(-y_limit, y_limit)) +
    labs(
      x = NULL,
      y = .csdg_plot_data_option(options, "y_label", "Accumulated local prediction difference"),
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(legend.position = "none")
}

.csdg_plot_data_ice = function(x, title, subtitle, base_size, options) {
  quantiles = .csdg_plot_data_table(x, character(), "ICE-quantile plot data")
  .csdg_plot_data_columns(
    quantiles,
    c(
      "feature", "x", "q05_prediction", "q25_prediction", "median_prediction", "q75_prediction",
      "q95_prediction", "n_curves"
    ),
    "ICE-quantile plot data"
  )
  feature_labels = .csdg_plot_data_option(options, "feature_labels")
  .csdg_plot_data_validate_labels(feature_labels, "feature_labels")
  quantiles[, `:=`(
    feature = as.character(feature),
    x = suppressWarnings(as.numeric(as.character(x)))
  )]
  quantiles[, ("n_curves") := suppressWarnings(as.numeric(as.character(get("n_curves"))))]
  if (!nrow(quantiles) || anyNA(quantiles$feature) || any(!nzchar(quantiles$feature)) ||
      any(!is.finite(quantiles$x)) || anyDuplicated(quantiles[, .(feature, x)])) {
    .csdg_stop("ICE data must contain one finite x value per non-empty feature and no duplicate feature-x rows.")
  }
  quantile_columns = c(
    "q05_prediction", "q25_prediction", "median_prediction", "q75_prediction", "q95_prediction"
  )
  quantiles[, (quantile_columns) := lapply(.SD, as.numeric), .SDcols = quantile_columns]
  finite_quantiles = vapply(
    quantiles[, quantile_columns, with = FALSE],
    function(column) all(is.finite(column)),
    logical(1L)
  )
  valid_quantiles = nrow(quantiles) > 0L &&
    all(finite_quantiles) &&
    all(quantiles$q05_prediction <= quantiles$q25_prediction) &&
    all(quantiles$q25_prediction <= quantiles$median_prediction) &&
    all(quantiles$median_prediction <= quantiles$q75_prediction) &&
    all(quantiles$q75_prediction <= quantiles$q95_prediction) &&
    all(
      is.finite(quantiles$n_curves) &
        quantiles$n_curves >= 1 &
        quantiles$n_curves == floor(quantiles$n_curves)
    )
  if (!valid_quantiles) {
    .csdg_stop("ICE quantiles must be finite, ordered, and based on a positive integer number of curves.")
  }
  quantiles[, feature_label := .csdg_plot_data_labels(feature, feature_labels)]
  feature_label_map = unique(quantiles[, .(feature, feature_label)])
  if (anyNA(feature_label_map$feature_label) || any(!nzchar(feature_label_map$feature_label)) ||
      anyDuplicated(feature_label_map$feature_label)) {
    .csdg_stop("`feature_labels` must map each ICE feature to a unique non-empty label.")
  }
  palette = .csdg_plot_data_palette(options)
  outer_band = "5th-95th percentile band"
  inner_band = "25th-75th percentile band"
  median_curve = "Pointwise median"
  band_colors = if (.csdg_plot_data_is_monochrome(options)) {
    c("#E8E8E8", "#A6A6A6")
  } else {
    c("#DCE6F2", "#92AED0")
  }
  plot = ggplot(quantiles, aes(x = x, y = median_prediction)) +
    geom_ribbon(
      aes(ymin = q05_prediction, ymax = q95_prediction, fill = outer_band)
    ) +
    geom_ribbon(
      aes(ymin = q25_prediction, ymax = q75_prediction, fill = inner_band)
    ) +
    geom_line(aes(color = median_curve), linewidth = 0.9) +
    facet_wrap(~ feature_label, scales = "free_x") +
    scale_fill_manual(
      name = NULL,
      breaks = c(outer_band, inner_band),
      labels = c("q05-q95 band", "q25-q75 band"),
      values = setNames(band_colors, c(outer_band, inner_band))
    ) +
    scale_color_manual(
      name = NULL,
      breaks = median_curve,
      labels = "Median",
      values = setNames(palette$metric[["secondary"]], median_curve)
    ) +
    guides(
      fill = guide_legend(order = 1L, nrow = 1L, byrow = TRUE),
      color = guide_legend(order = 2L, override.aes = list(linewidth = 1.1))
    ) +
    labs(
      x = NULL,
      y = .csdg_plot_data_option(options, "y_label", "Fitted prediction"),
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal"
    )
  y_limits = .csdg_plot_data_option(options, "y_limits")
  y_scale = .csdg_plot_data_option(options, "y_scale", "linear")
  assert_choice(y_scale, c("linear", "logit"), .var.name = "y_scale")
  y_breaks = .csdg_plot_data_option(options, "y_breaks")
  if (!is.null(y_breaks)) {
    assert_numeric(y_breaks, finite = TRUE, sorted = TRUE, unique = TRUE, min.len = 2L, .var.name = "y_breaks")
  }
  if (!is.null(y_limits)) {
    assert_numeric(y_limits, finite = TRUE, len = 2L, sorted = TRUE, unique = TRUE, .var.name = "y_limits")
    band_range = range(c(quantiles$q05_prediction, quantiles$q95_prediction))
    tolerance = sqrt(.Machine$double.eps) * max(1, abs(band_range), abs(y_limits))
    if (y_limits[[1L]] > band_range[[1L]] + tolerance ||
        y_limits[[2L]] < band_range[[2L]] - tolerance) {
      .csdg_stop("`y_limits` must contain every pointwise q05-q95 ICE band.")
    }
  }
  if (y_scale == "logit") {
    probability_values = c(
      quantiles$q05_prediction,
      quantiles$q25_prediction,
      quantiles$median_prediction,
      quantiles$q75_prediction,
      quantiles$q95_prediction
    )
    if (any(probability_values <= 0 | probability_values >= 1)) {
      .csdg_stop("A logit ICE scale requires every plotted prediction to lie strictly between zero and one.")
    }
    if (is.null(y_limits)) {
      y_limits = range(c(quantiles$q05_prediction, quantiles$q95_prediction))
    }
    if (any(y_limits <= 0 | y_limits >= 1)) {
      .csdg_stop("A logit ICE scale requires `y_limits` to lie strictly between zero and one.")
    }
    if (is.null(y_breaks)) {
      candidate_breaks = c(0.005, 0.01, 0.02, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
      y_breaks = candidate_breaks[candidate_breaks >= y_limits[[1L]] & candidate_breaks <= y_limits[[2L]]]
    }
    if (length(y_breaks) < 2L || any(y_breaks <= 0 | y_breaks >= 1) ||
        any(y_breaks < y_limits[[1L]] | y_breaks > y_limits[[2L]])) {
      .csdg_stop("Logit-scale `y_breaks` must contain at least two probabilities within `y_limits`.")
    }
    plot = plot + scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      labels = function(value) paste0(format(100 * value, trim = TRUE, scientific = FALSE), "%"),
      transform = "logit"
    )
  } else if (!is.null(y_limits) && is.null(y_breaks)) {
    plot = plot + scale_y_continuous(limits = y_limits)
  } else if (!is.null(y_limits) || !is.null(y_breaks)) {
    plot = plot + scale_y_continuous(limits = y_limits, breaks = y_breaks)
  }
  plot
}

.csdg_plot_data_interaction = function(x, title, subtitle, base_size, options) {
  screen = .csdg_plot_data_table(x, character(), "Interaction-screen plot data")
  .csdg_plot_data_columns(screen, c("feature_1", "feature_2", "h_statistic"), "Interaction screen")
  feature_labels = .csdg_plot_data_option(options, "feature_labels")
  .csdg_plot_data_validate_labels(feature_labels, "feature_labels")
  top_n = .csdg_plot_data_option(options, "top_n", nrow(screen))
  assert_int(top_n, lower = 1L)
  screen[, `:=`(
    estimable__ = is.finite(h_statistic),
    value__ = fifelse(is.finite(h_statistic), pmin(pmax(h_statistic, 0), 1), 0)
  )]
  setorder(screen, -estimable__, -value__, feature_1, feature_2)
  screen = head(screen, top_n)
  screen[, `:=`(
    pair_label = paste(
      .csdg_plot_data_labels(feature_1, feature_labels),
      .csdg_plot_data_labels(feature_2, feature_labels),
      sep = " x "
    ),
    value_label = fcase(
      !estimable__, "Not estimable",
      h_statistic == 0, "0.000",
      value__ < 0.001, "<0.001",
      default = sprintf("%.3f", value__)
    )
  )]
  screen[, pair_label := factor(pair_label, levels = rev(pair_label))]
  direct_label_size = .csdg_plot_data_option(options, "direct_label_size", 3 * base_size / 11)
  x_limits = .csdg_plot_data_option(options, "x_limits")
  if (is.null(x_limits)) {
    upper = max(screen$value__)
    upper = max(pretty(c(0, max(upper * 1.12, sqrt(.Machine$double.eps))), n = 4L))
    x_limits = c(0, min(1, max(upper, sqrt(.Machine$double.eps))))
  } else {
    assert_numeric(x_limits, finite = TRUE, len = 2L, sorted = TRUE, unique = TRUE, .var.name = "x_limits")
  }
  if (x_limits[[1L]] > 0 || x_limits[[2L]] < max(screen$value__)) {
    .csdg_stop("`x_limits` must include zero and every displayed interaction statistic.")
  }
  span = diff(x_limits)
  size_scale = direct_label_size / (2.9 * base_size / 11)
  label_reserve = span * pmin(0.35, pmax(0.08, 0.014 * nchar(screen$value_label) * size_scale))
  screen[, `:=`(
    label_hjust = 0,
    label_x = value__ + 0.04 * span,
    scale_max__ = value__ + 0.04 * span + label_reserve
  )]
  plot_limits = c(
    x_limits[[1L]] - 0.012 * span,
    max(x_limits[[2L]], screen$scale_max__) + 0.012 * span
  )
  axis_breaks = pretty(x_limits, n = 4L)
  axis_breaks = axis_breaks[axis_breaks >= x_limits[[1L]] & axis_breaks <= x_limits[[2L]]]
  palette = .csdg_plot_data_palette(options)

  ggplot(screen, aes(x = value__, y = pair_label)) +
    geom_segment(
      aes(x = 0, xend = value__, yend = pair_label),
      color = palette$metric[["primary"]],
      linewidth = 0.9
    ) +
    geom_point(
      data = screen[estimable__ == TRUE],
      color = palette$metric[["secondary"]],
      fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
      shape = .csdg_plot_data_point_shape(options),
      size = 2.5,
      stroke = 0.55
    ) +
    geom_point(data = screen[estimable__ == FALSE], color = "grey55", shape = 4, size = 2.5) +
    geom_text(
      aes(x = label_x, label = value_label, hjust = label_hjust),
      size = direct_label_size,
      color = "grey20"
    ) +
    geom_blank(aes(x = scale_max__)) +
    scale_x_continuous(limits = plot_limits, breaks = axis_breaks, expand = expansion(mult = 0)) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Friedman-Popescu H statistic (0-1)"),
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
}

.csdg_plot_data_ale_2d = function(x, title, subtitle, base_size, options) {
  surface = .csdg_plot_data_table(x, character(), "Two-dimensional ALE plot data")
  first_column = .csdg_plot_data_column(surface, c("feature1", "feature_1"), "First ALE feature")
  second_column = .csdg_plot_data_column(surface, c("feature2", "feature_2"), "Second ALE feature")
  .csdg_plot_data_columns(surface, c("x1_left", "x1_right", "x2_bottom", "x2_top", "ale2d"), "2D ALE")
  surface = surface[
    is.finite(x1_left) & is.finite(x1_right) & is.finite(x2_bottom) & is.finite(x2_top)
  ]
  if (!nrow(surface)) .csdg_stop("Two-dimensional ALE data contains no finite cells.")
  feature_labels = .csdg_plot_data_option(options, "feature_labels")
  .csdg_plot_data_validate_labels(feature_labels, "feature_labels")
  surface[, pair_label := paste(
    .csdg_plot_data_labels(get(first_column), feature_labels),
    .csdg_plot_data_labels(get(second_column), feature_labels),
    sep = " x "
  )]
  single_pair = uniqueN(surface[, c(first_column, second_column), with = FALSE]) == 1L
  default_x_label = if (single_pair) {
    unique(.csdg_plot_data_labels(surface[[first_column]], feature_labels))[[1L]]
  } else {
    "First feature value"
  }
  default_y_label = if (single_pair) {
    unique(.csdg_plot_data_labels(surface[[second_column]], feature_labels))[[1L]]
  } else {
    "Second feature value"
  }
  finite_values = surface$ale2d[is.finite(surface$ale2d)]
  limit = if (length(finite_values)) max(abs(finite_values)) else sqrt(.Machine$double.eps)
  if (!is.finite(limit) || limit <= sqrt(.Machine$double.eps)) limit = sqrt(.Machine$double.eps)
  palette = .csdg_plot_data_palette(options)
  monochrome = .csdg_plot_data_is_monochrome(options)
  if (monochrome) {
    surface[, `:=`(
      absolute_ale2d__ = abs(ale2d),
      sign_label__ = fifelse(is.finite(ale2d), fifelse(ale2d >= 0, "+", "-"), ""),
      sign_color__ = fifelse(abs(ale2d) / limit >= 0.58, "white", "grey15")
    )]
    plot = ggplot(surface) +
      geom_rect(
        aes(xmin = x1_left, xmax = x1_right, ymin = x2_bottom, ymax = x2_top, fill = absolute_ale2d__),
        color = "grey55",
        linewidth = 0.25
      ) +
      geom_text(
        aes(
          x = (x1_left + x1_right) / 2,
          y = (x2_bottom + x2_top) / 2,
          label = sign_label__,
          color = sign_color__
        ),
        fontface = "bold",
        size = 3
      ) +
      scale_color_identity() +
      scale_fill_gradient(
        low = palette$gradient[["low"]],
        high = palette$gradient[["high"]],
        limits = c(0, limit),
        na.value = "grey82"
      )
  } else {
    plot = ggplot(surface) +
      geom_rect(
        aes(xmin = x1_left, xmax = x1_right, ymin = x2_bottom, ymax = x2_top, fill = ale2d),
        color = "grey72",
        linewidth = 0.25
      ) +
      scale_fill_gradient2(
        low = palette$gradient[["low"]],
        mid = "white",
        high = palette$gradient[["high"]],
        midpoint = 0,
        limits = c(-limit, limit),
        na.value = "grey82"
      )
  }

  plot +
    facet_wrap(~ pair_label, scales = "free") +
    labs(
      x = .csdg_plot_data_option(options, "x_label", default_x_label),
      y = .csdg_plot_data_option(options, "y_label", default_y_label),
      fill = if (monochrome) "Absolute 2D ALE\n(sign in cell)" else "2D ALE",
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(panel.grid = element_blank())
}

.csdg_plot_data_multiplicity = function(x, title, subtitle, base_size, options) {
  bins = .csdg_plot_data_table(x, c("bins"), "Prediction-multiplicity plot data")
  .csdg_plot_data_columns(bins, c("bin_left", "bin_right", "N"), "Prediction-multiplicity bins")
  bins[, `:=`(bin_left = as.numeric(bin_left), bin_right = as.numeric(bin_right), N = as.numeric(N))]
  bins = bins[is.finite(bin_left) & is.finite(bin_right) & is.finite(N)]
  setorder(bins, bin_left, bin_right)
  if (!nrow(bins) || any(bins$bin_right <= bins$bin_left) || any(bins$N < 0 | bins$N != floor(bins$N)) ||
      sum(bins$N) < 1L || anyDuplicated(bins[, .(bin_left, bin_right)])) {
    .csdg_stop("Prediction-multiplicity bins are empty or invalid.")
  }
  if (nrow(bins) > 1L) {
    delta = abs(bins$bin_right[-nrow(bins)] - bins$bin_left[-1L])
    scale = pmax(1, abs(bins$bin_right[-nrow(bins)]), abs(bins$bin_left[-1L]))
    if (any(delta > sqrt(.Machine$double.eps) * scale)) {
      .csdg_stop("Prediction-multiplicity bins must be contiguous and non-overlapping.")
    }
  }
  bins[, cumulative_probability := cumsum(N) / sum(N)]
  binned_curve = rbindlist(list(
    data.table(prediction_range = bins$bin_left[[1L]], cumulative_probability = 0),
    bins[, .(prediction_range = bin_right, cumulative_probability)]
  ))
  markers = .csdg_plot_data_option(options, "quantiles")
  if (is.null(markers)) {
    probabilities = c(0.50, 0.90, 0.95)
    markers = data.table(
      statistic = c("p50", "p90", "p95"),
      probability = probabilities,
      prediction_range = vapply(probabilities, function(probability) {
        index = which(bins$cumulative_probability >= probability)[[1L]]
        lower_probability = if (index == 1L) 0 else bins$cumulative_probability[[index - 1L]]
        bin_fraction = (probability - lower_probability) /
          (bins$cumulative_probability[[index]] - lower_probability)
        bins$bin_left[[index]] + bin_fraction * (bins$bin_right[[index]] - bins$bin_left[[index]])
      }, numeric(1L))
    )
  } else {
    assert_data_frame(markers, min.rows = 1L, .var.name = "quantiles")
    markers = .as_dt(markers)
    .csdg_plot_data_columns(markers, c("statistic", "prediction_range"), "Multiplicity quantiles")
    markers = markers[statistic %in% c("p50", "p90", "p95")]
    markers[, probability := c(p50 = 0.50, p90 = 0.90, p95 = 0.95)[statistic]]
  }
  if (!setequal(markers$statistic, c("p50", "p90", "p95")) || anyDuplicated(markers$statistic) ||
      any(!is.finite(markers$prediction_range))) {
    .csdg_stop("Multiplicity markers must contain one finite p50, p90, and p95 value.")
  }
  setorder(markers, probability)
  if (any(markers$prediction_range < bins$bin_left[[1L]]) ||
      any(markers$prediction_range > bins$bin_right[[nrow(bins)]]) ||
      any(diff(markers$prediction_range) < 0)) {
    .csdg_stop("Multiplicity markers must be ordered and lie within the displayed binned distribution.")
  }
  markers[, label := paste0(statistic, " = ", .csdg_plot_data_direct_label(prediction_range, 3L))]
  marker_levels = c("p50", "p90", "p95")
  markers[, statistic := factor(statistic, levels = marker_levels)]
  curve = unique(rbindlist(list(
    binned_curve,
    markers[, .(prediction_range, cumulative_probability = probability)]
  )))
  setorder(curve, prediction_range, cumulative_probability)
  tolerance = sqrt(.Machine$double.eps)
  if (any(diff(curve$cumulative_probability) < -tolerance)) {
    .csdg_stop("Exact multiplicity quantiles are incompatible with the binned cumulative distribution.")
  }
  curve[, cumulative_probability := pmin(pmax(cumulative_probability, 0), 1)]
  direct_label_size = .csdg_plot_data_option(options, "direct_label_size", 2.9 * base_size / 11)
  palette = .csdg_plot_data_palette(options)
  x_span = diff(range(curve$prediction_range))
  label_y = c(p50 = 0.55, p90 = 0.865, p95 = 0.975)
  markers[, `:=`(
    label_x__ = prediction_range + 0.018 * x_span,
    label_y__ = unname(label_y[as.character(statistic)]),
    connector_x__ = prediction_range + 0.010 * x_span
  )]
  marker_segment = geom_segment(
    data = markers,
    aes(x = prediction_range, xend = prediction_range, y = 0, yend = probability, linetype = statistic),
    inherit.aes = FALSE,
    color = palette$metric[["secondary"]],
    linewidth = 0.65
  )
  label_connector = geom_segment(
    data = markers,
    aes(
      x = prediction_range,
      xend = connector_x__,
      y = probability,
      yend = label_y__
    ),
    inherit.aes = FALSE,
    color = "grey25",
    linewidth = 0.45
  )
  marker_label = geom_label(
    data = markers,
    aes(x = label_x__, y = label_y__, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = direct_label_size,
    color = "grey10",
    fill = "white",
    linewidth = 0,
    label.padding = grid::unit(0.08, "lines")
  )
  marker_point = geom_point(
    data = markers,
    aes(x = prediction_range, y = probability, shape = statistic),
    inherit.aes = FALSE,
    color = palette$metric[["secondary"]],
    fill = .csdg_plot_data_point_fill(options, palette$metric[["secondary"]]),
    size = 2.3,
    stroke = 0.6
  )

  plot = ggplot(curve, aes(x = prediction_range, y = cumulative_probability)) +
    geom_step(color = palette$metric[["primary"]], linewidth = 1, direction = "hv") +
    marker_segment +
    label_connector +
    marker_label +
    marker_point +
    scale_linetype_manual(
      values = .autoiml_quantile_linetypes,
      guide = "none"
    ) +
    scale_shape_manual(
      values = .autoiml_quantile_shapes,
      guide = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.015, 0.08))) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = function(value) paste0(round(100 * value), "%"),
      expand = expansion(mult = c(0.01, 0.035))
    ) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Within-row prediction range"),
      y = "Cumulative share of rows (binned)",
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size) +
    theme(
      legend.position = "none",
      plot.margin = margin(12, 24, 16, 18)
    )
  plot
}

.csdg_plot_data_decision_curve = function(x, title, subtitle, base_size, options) {
  decision = .csdg_plot_data_table(x, character(), "Decision-curve plot data")
  .csdg_plot_data_columns(
    decision,
    c("threshold", "net_benefit_model", "net_benefit_treat_all", "net_benefit_treat_none"),
    "Decision-curve plot data"
  )
  measure_columns = c("net_benefit_model", "net_benefit_treat_all", "net_benefit_treat_none")
  decision[, (measure_columns) := lapply(.SD, as.numeric), .SDcols = measure_columns]
  long = melt(
    decision,
    id.vars = "threshold",
    measure.vars = c("net_benefit_model", "net_benefit_treat_all", "net_benefit_treat_none"),
    variable.name = "strategy",
    value.name = "net_benefit"
  )
  long[, strategy := factor(
    strategy,
    levels = c("net_benefit_model", "net_benefit_treat_all", "net_benefit_treat_none"),
    labels = c("Focal model", "Treat all", "Treat none")
  )]
  palette = .csdg_plot_data_palette(options)
  strategy_colors = if (.csdg_plot_data_is_monochrome(options)) {
    setNames(rep("#111111", 3L), levels(long$strategy))
  } else {
    c(
      "Focal model" = palette$metric[["secondary"]],
      "Treat all" = palette$metric[["primary"]],
      "Treat none" = "grey45"
    )
  }

  ggplot(long, aes(x = threshold, y = net_benefit, color = strategy, linetype = strategy)) +
    geom_hline(yintercept = 0, color = "grey75", linewidth = 0.5) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = strategy_colors) +
    scale_linetype_manual(values = .autoiml_decision_linetypes) +
    scale_x_continuous(limits = range(decision$threshold, finite = TRUE)) +
    labs(
      x = .csdg_plot_data_option(options, "x_label", "Decision threshold"),
      y = .csdg_plot_data_option(options, "y_label", "Net benefit"),
      color = NULL,
      linetype = NULL,
      title = title,
      subtitle = subtitle
    ) +
    .csdg_plot_theme(base_size)
}

#' Plot tabular CSDG diagnostics
#'
#' Builds publication-ready diagnostic plots from package result tables or their enclosing result lists.
#'
#' `csdg_plot_data()` is the table-oriented complement to [csdg_plot()].
#' It is intended for analyses that combine or relabel package outputs before plotting while preserving the package's
#' visual system and diagnostic semantics.
#'
#' @param x A data frame or structured result returned by a CSDG diagnostic function.
#' @param type One of `"performance"`, `"calibration"`, `"pfi"`, `"importance_comparison"`,
#'   `"model_comparison"`, `"rank_heatmap"`, `"dependence"`, `"measurement_trajectory"`,
#'   `"setting_generalization"`, `"setting_penalty"`, `"subgroup"`, `"subgroup_contrast"`, `"local_fidelity"`,
#'   `"gate_status"`, `"explanation_sensitivity"`, `"ale"`, `"ice"`, `"interaction"`, `"ale_2d"`,
#'   `"multiplicity"`, or `"decision_curve"`.
#' @param title,subtitle Optional plot text.
#' @param base_size Base text size in points.
#' @param style Either `"color"` for the default blue-red rendering or `"monochrome"` for an achromatic
#'   print-oriented rendering with redundant shapes and line types where categories must be distinguished.
#' @param ... Type-specific options.
#'
#'   Common options are `x_label`, `y_label`, `feature_labels`, `direct_label_size`, `top_n`, `axis_text_size`,
#'   `axis_title_size`, `legend_text_size`, `legend_title_size`, and `strip_text_size`.
#'
#'   `performance` accepts `learner_labels` and `include_zero`.
#'   Point-and-interval panels use data-adaptive axes by default because a zero baseline is not required for positional
#'   encodings; set `include_zero = TRUE` when zero is substantively necessary.
#'
#'   `calibration` accepts `confidence_level`, uses fixed-size bin markers without a redundant bin-count legend,
#'   and computes Wilson score intervals only when the input contains valid binomial bin counts and no intervals.
#'
#'   `model_comparison` accepts `fold_scores`, `reference_learner`, `limit_label_size`, and `x_limits`.
#'
#'   `importance_comparison` accepts `feature_labels`, `learner_order`, and `top_n`.
#'   It plots mean importance in aligned learner panels and uses the supplied lower and upper endpoints as
#'   descriptive ranges rather than confidence intervals.
#'
#'   `dependence` accepts `feature_labels` and `top_n`.
#'
#'   `measurement_trajectory` accepts `learner_labels`, `outcome_order`, `x_label`, `y_label`, and `include_zero`.
#'
#'   `setting_generalization` accepts `measure`.
#'   `setting_penalty` accepts `x_label` and `y_label`.
#'
#'   `rank_heatmap` accepts `learner_order`.
#'
#'   `subgroup` and `subgroup_contrast` require `metric` and accept `audit_labels` and `subgroup_labels`.
#'   `subgroup` also accepts `subgroup_order` and `x_limits`; `subgroup_contrast` accepts `caption`.
#'   Direct labels for `pfi` and `subgroup` are placed inside the available horizontal scale.
#'
#'   `gate_status` accepts `gate_order`, `group_order`, `group_labels`, and `cell_label_size`.
#'   Multi-group inputs must provide one status for every group-by-gate combination.
#'
#'   `explanation_sensitivity` accepts `learner_labels`, `group_order`, `group_labels`, `group_colors`, `x_limits`,
#'   `y_limits`, `caption`, and `display`.
#'   Set `display = "aligned"` for an aligned dot display, or `display = "diverging"` for opposing bars of
#'   performance-tolerance consumption and PFI pair-order reversals on a common proportion scale.
#'   Both displays also accept `include_focal`, `reference_learner`, and `learner_order`; the diverging display accepts
#'   `bar_width` between 0.05 and 0.9.
#'   In the color style, group colors may be named by original group IDs or by uniquely mapped group labels.
#'   The monochrome style uses black shapes instead.
#'
#'   `ale` draws a pointwise uncertainty ribbon when the input contains both `ale_lower` and `ale_upper`.
#'   Interval semantics must be supplied by the source table and should be stated in the figure caption.
#'
#'   `ice` accepts `y_limits`, `y_scale`, and `y_breaks`, and identifies the pointwise 5th-95th percentile band,
#'   25th-75th percentile band, and median across ICE curves in a legend.
#'   Set `y_scale = "logit"` for probability predictions that would otherwise be compressed near zero or one.
#'   Supplied limits must contain every pointwise 5th-95th percentile band.
#'
#'   `interaction` accepts `x_limits`; by default, its horizontal axis is restricted to the displayed statistics while
#'   retaining the bounded-statistic definition in the axis title.
#'
#'   `multiplicity` draws a stepwise cumulative summary from fixed-width aggregate bins.
#'   It accepts an optional aggregate `quantiles` table and directly labels its exact p50, p90, and p95 values on the
#'   plotted path.
#' @return A [ggplot2::ggplot] object.
#' @examples
#' calibration = data.frame(
#'   predicted = c(0.1, 0.3, 0.7),
#'   observed = c(0.08, 0.28, 0.73),
#'   n = c(100L, 100L, 100L)
#' )
#' csdg_plot_data(calibration, type = "calibration")
#' @export
csdg_plot_data = function(
    x,
    type,
    title = NULL,
    subtitle = NULL,
    base_size = 11,
    style = c("color", "monochrome"),
    ...) {
  assert_true(is.data.frame(x) || is.list(x), .var.name = "x")
  assert_choice(type, .csdg_plot_data_types)
  if (!is.null(title)) assert_string(title, min.chars = 1L)
  if (!is.null(subtitle)) assert_string(subtitle, min.chars = 1L)
  assert_number(base_size, lower = 6, finite = TRUE)
  style = match.arg(style)
  assert_choice(style, .autoiml_plot_styles, .var.name = "style")
  options = list(...)
  options$style = style
  typography_options = c(
    "axis_text_size", "axis_title_size", "legend_text_size", "legend_title_size", "strip_text_size"
  )
  allowed = switch(type,
    performance = c("learner_labels", "direct_label_size", "x_label", "include_zero"),
    calibration = c("confidence_level", "x_label", "y_label"),
    pfi = c("feature_labels", "top_n", "direct_label_size", "x_label"),
    importance_comparison = c("feature_labels", "learner_order", "top_n", "x_label"),
    model_comparison = c(
      "fold_scores", "reference_learner", "direct_label_size", "limit_label_size", "x_label", "x_limits"
    ),
    rank_heatmap = c("feature_labels", "learner_order", "top_n", "cell_label_size"),
    dependence = c("feature_labels", "top_n"),
    measurement_trajectory = c("learner_labels", "outcome_order", "x_label", "y_label", "include_zero"),
    setting_generalization = c("measure", "x_label"),
    setting_penalty = c("x_label", "y_label"),
    subgroup = c(
      "metric", "audit_labels", "subgroup_labels", "subgroup_order", "direct_label_size", "x_label", "x_limits"
    ),
    subgroup_contrast = c(
      "metric", "audit_labels", "subgroup_labels", "direct_label_size", "x_label", "caption"
    ),
    local_fidelity = c("direct_label_size", "x_label"),
    gate_status = c(
      "gate_order", "group_order", "group_labels", "cell_label_size", "x_label", "y_label"
    ),
    explanation_sensitivity = c(
      "learner_labels", "group_order", "group_labels", "group_colors", "x_limits", "y_limits",
      "direct_label_size", "x_label", "y_label", "caption", "display", "include_focal", "reference_learner",
      "learner_order", "bar_width"
    ),
    ale = c("feature_labels", "y_label"),
    ice = c("feature_labels", "y_label", "y_limits", "y_scale", "y_breaks"),
    interaction = c("feature_labels", "top_n", "direct_label_size", "x_label", "x_limits"),
    ale_2d = c("feature_labels", "x_label", "y_label"),
    multiplicity = c("quantiles", "direct_label_size", "x_label"),
    decision_curve = c("x_label", "y_label")
  )
  options = .csdg_plot_data_options(options, unique(c(allowed, typography_options, "style")), type)
  for (name in intersect(c("x_label", "y_label", "caption"), names(options))) {
    if (!is.null(options[[name]])) assert_string(options[[name]], min.chars = 1L, .var.name = name)
  }
  size_options = c("direct_label_size", "cell_label_size", "limit_label_size", typography_options)
  for (name in intersect(size_options, names(options))) {
    if (!is.null(options[[name]])) {
      assert_number(options[[name]], lower = 1, finite = TRUE, .var.name = name)
    }
  }
  builder = switch(type,
    performance = .csdg_plot_data_performance,
    calibration = .csdg_plot_data_calibration,
    pfi = .csdg_plot_data_pfi,
    importance_comparison = .csdg_plot_data_importance_comparison,
    model_comparison = .csdg_plot_data_model_comparison,
    rank_heatmap = .csdg_plot_data_rank_heatmap,
    dependence = .csdg_plot_data_dependence,
    measurement_trajectory = .csdg_plot_data_measurement_trajectory,
    setting_generalization = .csdg_plot_data_setting,
    setting_penalty = .csdg_plot_data_setting_penalty,
    subgroup = .csdg_plot_data_subgroup,
    subgroup_contrast = .csdg_plot_data_subgroup_contrast,
    local_fidelity = .csdg_plot_data_local_fidelity,
    gate_status = .csdg_plot_data_gate_status,
    explanation_sensitivity = .csdg_plot_data_explanation_sensitivity,
    ale = .csdg_plot_data_ale,
    ice = .csdg_plot_data_ice,
    interaction = .csdg_plot_data_interaction,
    ale_2d = .csdg_plot_data_ale_2d,
    multiplicity = .csdg_plot_data_multiplicity,
    decision_curve = .csdg_plot_data_decision_curve
  )
  builder(x, title, subtitle, base_size, options) + .csdg_plot_data_typography(options)
}

#' Extract a standalone legend from a CSDG plot
#'
#' Creates a separate plot component containing only the legend of an existing [ggplot2::ggplot] object.
#' This is useful when LaTeX assembles several separately saved panels that share one legend.
#'
#' @param plot A `ggplot` object with a visible legend.
#' @param position The legend position used by `plot`.
#' @return A `ggplot` object containing only the selected legend.
#' @examples
#' data = data.frame(x = 1:2, y = 1:2, group = c("A", "B"))
#' plot = ggplot2::ggplot(data, ggplot2::aes(x, y, color = group)) +
#'   ggplot2::geom_point() +
#'   ggplot2::theme(legend.position = "bottom")
#' csdg_plot_legend(plot)
#' @export
csdg_plot_legend = function(plot, position = "bottom") {
  assert_true(inherits(plot, c("gg", "ggplot")), .var.name = "plot")
  assert_choice(position, c("bottom", "top", "left", "right", "inside"))
  grob = ggplotGrob(plot)
  legend_index = which(grob$layout$name == paste0("guide-box-", position))
  if (length(legend_index) != 1L || inherits(grob$grobs[[legend_index]], "zeroGrob")) {
    .csdg_stop("The plot does not contain a visible legend at position `%s`.", position)
  }
  legend = grob$grobs[[legend_index]]
  ggplot() +
    annotation_custom(legend, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void()
}
