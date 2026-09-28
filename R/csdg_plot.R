
.extract_gate_evidence = function(x, gate) {
  if (inherits(x, "CSDGResult")) {
    if (!gate %in% names(x$gates)) .csdg_stop("Unknown gate: %s.", gate)
    return(x$gates[[gate]]$evidence)
  }
  if (inherits(x, "CSDGGateResult")) return(x$evidence)
  if (is.list(x)) return(x)
  .csdg_stop("Unsupported object for plotting.")
}

.csdg_plot_measure_labels = function(values) {
  labels = c(
    "auc" = "ROC AUC",
    "classif.auc" = "ROC AUC",
    "classif.prauc" = "Precision-recall AUC",
    "classif.bbrier" = "Brier score",
    "classif.logloss" = "Log loss",
    "rmse" = "RMSE",
    "regr.rmse" = "RMSE",
    "regr.mae" = "MAE",
    "regr.rsq" = "R-squared"
  )
  result = unname(labels[as.character(values)])
  missing = is.na(result)
  result[missing] = tools::toTitleCase(gsub("[._]", " ", as.character(values)[missing]))
  result
}

.csdg_plot_model_labels = function(values) {
  labels = c(
    xgboost = "XGBoost",
    logistic = "Logistic regression",
    ridge_logistic = "Ridge logistic regression",
    random_forest = "Random forest",
    ridge = "Ridge regression",
    decision_tree = "Decision tree",
    featureless = "Featureless baseline"
  )
  result = unname(labels[as.character(values)])
  missing = is.na(result)
  result[missing] = tools::toTitleCase(gsub("_", " ", as.character(values)[missing]))
  result
}

.csdg_plot_theme = function(base_size) {
  font_family = .autoiml_plot_font_family()
  .autoiml_theme_iml(base_size = 11) +
    ggplot2::theme(text = ggplot2::element_text(family = font_family, size = base_size))
}

#' @rdname csdg_reporting
#' @export
csdg_plot = function(
    x,
    gate = NULL,
    type = NULL,
    base_size = 11,
    style = c("color", "monochrome"),
    ...) {
  dots = list(...)
  .assert_named_dots(dots)
  if (length(dots)) .csdg_stop("`csdg_plot()` does not currently accept additional arguments.")
  checkmate::assert_true(
    inherits(x, c("CSDGResult", "CSDGGateResult")) || is.list(x),
    .var.name = "x"
  )
  if (!is.null(gate)) checkmate::assert_choice(gate, .csdg_gate_ids)
  if (!is.null(type)) checkmate::assert_string(type, min.chars = 1L)
  checkmate::assert_number(base_size, lower = 6, finite = TRUE, .var.name = "base_size")
  style = match.arg(style)
  checkmate::assert_choice(style, .autoiml_plot_styles, .var.name = "style")
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .csdg_stop("Install the suggested package `ggplot2` to use csdg_plot().")
  }
  palette = .autoiml_plot_palette(style)
  monochrome = identical(style, "monochrome")
  point_shape = if (monochrome) 21L else 16L
  point_fill = if (monochrome) "white" else palette$metric[["secondary"]]
  report_status_colors = if (monochrome) {
    c(
      error = "#111111",
      contradicted = "#111111",
      open = "#111111",
      supported = "#111111",
      context = "#111111",
      not_required = "#111111"
    )
  } else {
    c(
      error = palette$status[["error"]],
      contradicted = palette$status[["fail"]],
      open = palette$status[["warn"]],
      supported = palette$status[["pass"]],
      context = palette$status[["skip"]],
      not_required = palette$status[["skip"]]
    )
  }
  label_scale = base_size / 11
  if (is.null(gate)) {
    if (inherits(x, "CSDGResult")) {
      card = csdg_report_card(x)
      card[, `:=`(
        gate_label = factor(
          paste(gate_id, gate_name, sep = " - "),
          levels = rev(paste(gate_id, gate_name, sep = " - "))
        ),
        status_label = unname(.csdg_gate_status_labels[as.character(status)]),
        status = factor(
          status,
          levels = c("error", "contradicted", "open", "supported", "context", "not_required")
        )
      )]
      return(
        ggplot2::ggplot(card, ggplot2::aes(x = 0, y = gate_label, color = status, shape = status)) +
          ggplot2::geom_point(size = 3.2, show.legend = FALSE) +
          ggplot2::geom_text(
            ggplot2::aes(x = 0.035, label = status_label),
            hjust = 0,
            color = "grey20",
            size = 3.4 * label_scale
          ) +
          ggplot2::scale_color_manual(values = report_status_colors) +
          ggplot2::scale_shape_manual(values = c(
            error = unname(.autoiml_status_shapes[["error"]]),
            contradicted = unname(.autoiml_status_shapes[["fail"]]),
            open = unname(.autoiml_status_shapes[["warn"]]),
            supported = unname(.autoiml_status_shapes[["pass"]]),
            context = 5L,
            not_required = unname(.autoiml_status_shapes[["skip"]])
          )) +
          ggplot2::scale_x_continuous(limits = c(-0.015, 0.20), breaks = NULL) +
          ggplot2::labs(
            x = NULL, y = NULL,
            title = "Gates of the claim: status of required properties and context"
          ) +
          .csdg_plot_theme(base_size) +
          ggplot2::theme(panel.grid = ggplot2::element_blank())
      )
    }
    .csdg_stop("Supply `gate` unless `x` is a CSDGResult.")
  }

  gate_thresholds = if (inherits(x, "CSDGResult")) x$gates[[gate]]$thresholds else list()
  ev = .extract_gate_evidence(x, gate)
  if (identical(gate, "G1")) {
    tab = ev$performance$summary %||% ev$summary
    tab = .as_dt(tab)
    if (!all(c("measure", "mean") %in% names(tab))) {
      .csdg_stop("G1 evidence does not contain a performance summary.")
    }
    tab[, `:=`(
      measure_label = .csdg_plot_measure_labels(measure),
      plot_row = 1,
      value_label = formatC(mean, digits = 3L, format = "fg")
    )]
    n_measures = data.table::uniqueN(tab$measure_label)
    facet_columns = if (n_measures == 3L) 3L else 2L
    right_expansion = if (n_measures == 3L) 0.20 else 0.05
    ggplot2::ggplot(tab, ggplot2::aes(x = mean, y = plot_row)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = q10, xmax = q90),
        orientation = "y",
        color = palette$metric[["primary"]],
        width = 0.06,
        linewidth = 0.6,
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        size = 1.8,
        color = palette$metric[["secondary"]],
        fill = point_fill,
        shape = point_shape,
        stroke = 0.55
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = value_label),
        nudge_y = 0.07,
        color = "grey20",
        size = 2.55 * label_scale
      ) +
      ggplot2::facet_wrap(~ measure_label, scales = "free_x", ncol = facet_columns) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.04, right_expansion))) +
      ggplot2::scale_y_continuous(limits = c(0.75, 1.35), breaks = NULL) +
      ggplot2::labs(
        x = "Held-out metric value", y = NULL,
        title = "Held-out predictive performance",
        subtitle = paste0(
          "Points are means; whiskers show descriptive 10th-90th percentiles across iterations,",
          "\nnot confidence intervals"
        )
      ) +
      .csdg_plot_theme(base_size) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  } else if (identical(gate, "G2")) {
    tab = ev$pairwise %||% ev$dependence$pairwise
    tab = .as_dt(tab)
    feature_scores = data.table::rbindlist(list(
      tab[, .(feature = feature_1, association_strength = abs(association))],
      tab[, .(feature = feature_2, association_strength = abs(association))]
    ))[, .(
      maximum_association = if (any(is.finite(association_strength))) {
        max(association_strength, na.rm = TRUE)
      } else {
        NA_real_
      }
    ), by = feature]
    data.table::setorder(feature_scores, -maximum_association, feature)
    shown = utils::head(feature_scores$feature, 15L)
    tab = tab[feature_1 %in% shown & feature_2 %in% shown]
    tab = data.table::rbindlist(list(
      tab,
      tab[, .(
        feature_1 = feature_2,
        feature_2 = feature_1,
        kind_1 = kind_2,
        kind_2 = kind_1,
        method,
        association,
        n_complete
      )]
    ), use.names = TRUE)
    tab[, `:=`(
      feature_1_index = match(feature_1, shown),
      feature_2_index = match(feature_2, shown)
    )]
    tab = tab[feature_2_index > feature_1_index]
    tab[, `:=`(
      association_strength = abs(association),
      feature_1 = factor(feature_1, levels = shown),
      feature_2 = factor(feature_2, levels = rev(shown))
    )]
    ggplot2::ggplot(
      tab,
      ggplot2::aes(x = feature_1, y = feature_2, fill = association_strength)
    ) +
      ggplot2::geom_tile(color = if (monochrome) "grey65" else "white", linewidth = 0.25) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_y_discrete(drop = FALSE) +
      ggplot2::scale_fill_gradient(
        low = palette$gradient[["low"]],
        high = palette$gradient[["high"]],
        limits = c(0, 1)
      ) +
      ggplot2::labs(
        x = NULL, y = NULL, fill = "Absolute\nassociation",
        title = "Mixed-type pairwise dependence strength",
        subtitle = paste(
          "Lower triangle for the 15 strongest variables. Cells combine absolute Spearman correlation,",
          "Cramer's V, and eta-squared; these descriptive magnitudes are not directly equivalent."
        )
      ) +
      ggplot2::coord_fixed() +
      .csdg_plot_theme(base_size) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  } else if (identical(gate, "G3a")) {
    tab = ev$calibration$curve %||% ev$curve
    tab = .as_dt(tab)
    if (all(c("predicted", "observed") %in% names(tab))) {
      ggplot2::ggplot(tab, ggplot2::aes(x = predicted, y = observed)) +
        ggplot2::geom_abline(
          slope = 1, intercept = 0, linetype = 2,
          color = palette$reference[["neutral"]]
        ) +
        ggplot2::geom_line(color = palette$metric[["primary"]]) +
        ggplot2::geom_point(
          color = palette$metric[["secondary"]],
          fill = point_fill,
          shape = point_shape,
          size = 2.8,
          stroke = 0.55
        ) +
        ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
        ggplot2::labs(
          x = "Mean predicted probability", y = "Observed event fraction",
          title = "Out-of-fold calibration",
          subtitle = "Equal-count bins; the diagonal denotes perfect calibration"
        ) +
        .csdg_plot_theme(base_size)
    } else {
      ggplot2::ggplot(tab, ggplot2::aes(x = prediction, y = truth)) +
        ggplot2::geom_point(
          alpha = 0.2,
          color = palette$metric[["primary"]],
          fill = if (monochrome) "white" else palette$metric[["primary"]],
          shape = point_shape
        ) +
        ggplot2::geom_smooth(
          method = "lm", se = FALSE,
          color = palette$metric[["secondary"]]
        ) +
        ggplot2::labs(
          x = "Out-of-fold prediction", y = "Observed outcome",
          title = "Regression calibration"
        ) +
        .csdg_plot_theme(base_size)
    }
  } else if (identical(gate, "G4")) {
    tab = ev$summary
    tab = .as_dt(tab)
    tab[, case_label := sprintf("Communication case %d", seq_len(.N))]
    tab[, case_label := factor(case_label, levels = rev(unique(case_label)))]
    tab[, value_label := sprintf("%.4f", weighted_r2)]
    threshold = gate_thresholds$min_weighted_r2 %||% NA_real_
    finite_values = c(0, 1, tab$weighted_r2[is.finite(tab$weighted_r2)])
    if (length(threshold) == 1L && is.finite(threshold)) {
      finite_values = c(finite_values, threshold)
    }
    plot_span = diff(range(finite_values))
    if (!is.finite(plot_span) || plot_span <= 0) plot_span = 1
    midpoint = mean(range(finite_values))
    tab[, `:=`(
      label_x = weighted_r2 + ifelse(weighted_r2 > midpoint, -0.025, 0.025) * plot_span,
      label_hjust = ifelse(weighted_r2 > midpoint, 1, 0)
    )]
    x_limits = range(c(finite_values, tab$label_x), finite = TRUE)
    x_limits = x_limits + c(-0.03, 0.03) * diff(x_limits)
    plot = ggplot2::ggplot(
      tab,
      ggplot2::aes(x = weighted_r2, y = case_label)
    )
    if (length(threshold) == 1L && is.finite(threshold)) {
      plot = plot + ggplot2::geom_vline(
        xintercept = threshold,
        color = palette$reference[["neutral"]],
        linetype = 2,
        linewidth = 0.8
      )
    }
    plot +
      ggplot2::geom_vline(xintercept = 0, color = "grey75", linewidth = 0.5) +
      ggplot2::geom_point(
        size = 3,
        color = palette$metric[["secondary"]],
        fill = point_fill,
        shape = point_shape,
        stroke = 0.6
      ) +
      ggplot2::geom_text(
        ggplot2::aes(x = label_x, label = value_label, hjust = label_hjust),
        size = 3.2 * label_scale
      ) +
      ggplot2::scale_x_continuous(limits = x_limits) +
      ggplot2::labs(
        x = "Cross-fitted weighted local R-squared", y = NULL,
        title = "Held-out local surrogate fidelity",
        subtitle = if (is.finite(threshold)) {
          paste0(
            "Post-hoc communication cases; dashed line is the prespecified minimum of ",
            formatC(threshold, digits = 2L, format = "f"), "; points are not uncertainty estimates"
          )
        } else {
          "Post-hoc communication cases; points are diagnostics, not uncertainty estimates"
        }
      ) +
      .csdg_plot_theme(base_size)
  } else if (identical(gate, "G5")) {
    tab = ev$pfi$summary %||% ev$summary
    tab = .as_dt(tab)
    data.table::setorder(tab, -mean_importance, feature_group)
    tab = utils::head(tab, 15L)
    ggplot2::ggplot(
      tab,
      ggplot2::aes(
        x = mean_importance,
        y = stats::reorder(feature_group, mean_importance)
      )
    ) +
      ggplot2::geom_vline(xintercept = 0, color = palette$reference[["neutral"]], linewidth = 0.4) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = q10_importance, xmax = q90_importance),
        orientation = "y",
        color = palette$metric[["primary"]],
        width = 0.28,
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        size = 2.2,
        color = palette$metric[["secondary"]],
        fill = point_fill,
        shape = point_shape,
        stroke = 0.55
      ) +
      ggplot2::labs(
        x = "Increase in held-out loss", y = NULL,
        title = "Foldwise permutation importance",
        subtitle = paste(
          "Top 15 features by mean importance; whiskers show descriptive 10th-90th percentiles",
          "across folds, not confidence intervals"
        )
      ) +
      .csdg_plot_theme(base_size)
  } else if (gate %in% c("G6a", "G6b")) {
    model_tab = ev$model_generalization$candidates %||% NULL
    setting_tab = ev$setting_generalization$scores %||% NULL
    if (!is.null(setting_tab)) {
      tab = .as_dt(setting_tab)
      measure_cols = setdiff(
        names(tab),
        c("held_out_group", "n_train", "n_assessment")
      )
      measure = type %||% measure_cols[[1L]]
      if (!measure %in% names(tab)) .csdg_stop("Unknown G6 measure: %s.", measure)
      tab[, plot_value := get(measure)]
      return(
        ggplot2::ggplot(
          tab,
          ggplot2::aes(
            x = stats::reorder(held_out_group, plot_value),
            y = plot_value
          )
        ) +
          ggplot2::geom_point(
            size = 3,
            color = palette$metric[["primary"]],
            fill = if (monochrome) "white" else palette$metric[["primary"]],
            shape = point_shape,
            stroke = 0.6
          ) +
          ggplot2::coord_flip() +
          ggplot2::labs(
            x = "Held-out setting", y = measure,
            title = "Leave-one-setting-out performance"
          ) +
          .csdg_plot_theme(base_size)
      )
    }
    if (!is.null(model_tab)) {
      tab = .as_dt(model_tab)
      tab[, `:=`(
        learner_label = .csdg_plot_model_labels(learner_name),
        acceptance_label = ifelse(accepted, "Within tolerance", "Outside tolerance"),
        score_label = formatC(mean_score, digits = 3L, format = "fg")
      )]
      limit = unique(tab$acceptance_limit[is.finite(tab$acceptance_limit)])
      plot = ggplot2::ggplot(
        tab,
        ggplot2::aes(
          x = mean_score,
          y = stats::reorder(learner_label, mean_score),
          color = acceptance_label,
          shape = acceptance_label
        )
      ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_text(
          ggplot2::aes(label = score_label),
          nudge_x = diff(range(tab$mean_score)) * 0.025,
          hjust = 0,
          color = "grey20",
          size = 3.1 * label_scale
        ) +
        ggplot2::scale_color_manual(values = c(
          "Within tolerance" = palette$metric[["primary"]],
          "Outside tolerance" = palette$metric[["secondary"]]
        )) +
        ggplot2::scale_shape_manual(values = c(
          "Within tolerance" = 16L,
          "Outside tolerance" = 4L
        )) +
        ggplot2::labs(
          x = paste("Mean held-out", .csdg_plot_measure_labels(tab$primary_measure[[1L]])),
          y = NULL,
          color = NULL,
          shape = NULL,
          title = "Near-equivalent model screen",
          subtitle = "The vertical line is the prespecified acceptance limit"
        ) +
        .csdg_plot_theme(base_size)
      if (length(limit) == 1L) {
        plot = plot + ggplot2::geom_vline(
          xintercept = limit,
          color = palette$reference[["neutral"]],
          linetype = 2
        )
      }
      if (identical(tab$primary_measure[[1L]], "classif.auc")) {
        plot = plot + ggplot2::scale_x_continuous(limits = c(0.5, 1))
      } else {
        plot = plot + ggplot2::expand_limits(x = 0)
      }
      return(plot)
    }
    .csdg_stop("G6 evidence contains neither model nor setting diagnostics.")
  } else if (identical(gate, "G7a")) {
    tab = ev$subgroup_audit$metrics %||% ev$metrics
    tab = .as_dt(tab)
    id = c("audit_variable", "subgroup", "n", "below_minimum_n", "evidence_scope")
    metric_cols = names(tab)[vapply(tab, is.numeric, logical(1))]
    metric_cols = setdiff(metric_cols, "n")
    metric = type %||% metric_cols[[1L]]
    if (!metric %in% names(tab)) .csdg_stop("Unknown G7 metric: %s.", metric)
    tab[, plot_value := get(metric)]
    tab[, `:=`(
      audit_label = gsub("^audit_", "", as.character(audit_variable)),
      subgroup_label = as.character(subgroup),
      ci_low = NA_real_,
      ci_high = NA_real_
    )]
    tab[, audit_label := tools::toTitleCase(gsub("_", " ", audit_label))]
    uncertainty = ev$subgroup_audit$uncertainty %||% ev$uncertainty %||% NULL
    if (!is.null(uncertainty)) {
      metric_name = metric
      uncertainty = .as_dt(uncertainty)[metric == metric_name, .(
        audit_variable, subgroup = as.character(subgroup), ci_low, ci_high
      )]
      tab = merge(tab, uncertainty, by = c("audit_variable", "subgroup"), all.x = TRUE, suffixes = c("", ".new"))
      tab[is.finite(ci_low.new), ci_low := ci_low.new]
      tab[is.finite(ci_high.new), ci_high := ci_high.new]
    }
    plot = ggplot2::ggplot(
      tab,
      ggplot2::aes(
        x = plot_value,
        y = stats::reorder(subgroup_label, plot_value)
      )
    ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = ci_low, xmax = ci_high),
        orientation = "y",
        color = palette$metric[["primary"]],
        width = 0.09,
        linewidth = 0.7,
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        size = 1.9,
        color = palette$metric[["secondary"]],
        fill = point_fill,
        shape = point_shape,
        stroke = 0.55
      ) +
      ggplot2::facet_wrap(~ audit_label, scales = "free_y") +
      ggplot2::labs(
        x = toupper(metric), y = NULL,
        title = "Selected-model subgroup audit",
        subtitle = if (any(is.finite(tab$ci_low) & is.finite(tab$ci_high))) {
          "Whiskers are conditional percentile bootstrap intervals; model-training variation is excluded"
        } else {
          "Points are descriptive out-of-fold estimates; uncertainty intervals were not requested"
        }
      ) +
      .csdg_plot_theme(base_size)
    if (identical(metric, "auc")) {
      plot = plot + ggplot2::scale_x_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1))
    }
    plot
  } else {
    .csdg_stop("No plot method is defined for gate `%s`.", gate)
  }
}
