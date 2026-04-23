# FILE: R/plot_gate_07a_subgroups.R

#' @title Gate 7A Plotting Helpers
#'
#' @description
#' Internal ggplot2-based plotting helper for Gate 7A (Subgroup and Measurement Audit).
#'
#' Visualizes a chosen subgroup metric (e.g., R-squared for regression or AUC for
#' classification) as a horizontal bar chart, with groups colored by variable and a
#' vertical reference line at the overall (mean across groups) metric value.
#'
#' @name plot_gate_07a_subgroups
#' @keywords internal
NULL

#' @keywords internal
.autoiml_plot_g7a_subgroups = function(result, metric = "auto", top_n = 20L,
  labels = NULL) {
  if (!.autoiml_require_pkg("ggplot2")) {
    stop("Plotting requires package 'ggplot2'. Please install it.", call. = FALSE)
  }

  gr = .autoiml_get_gate_result(result, "G7A")
  if (is.null(gr)) {
    return(NULL)
  }

  sub = gr$artifacts$subgroup
  if (is.null(sub) || nrow(sub) == 0L) {
    return(NULL)
  }

  sub = data.table::as.data.table(sub)
  if (!all(c("group_var", "group") %in% names(sub))) {
    return(NULL)
  }

  # Apply optional label remapping: labels is a named list of named character vectors
  # e.g. list(gender = c("1" = "Female", "2" = "Male"))
  if (!is.null(labels) && is.list(labels)) {
    sub = data.table::copy(sub)
    for (var_name in names(labels)) {
      lmap = labels[[var_name]]
      if (!is.null(lmap) && is.character(lmap) && !is.null(names(lmap))) {
        # Remap group_var name
        sub[group_var == var_name, group := {
          mapped = lmap[as.character(group)]
          ifelse(is.na(mapped), as.character(group), mapped)
        }]
      }
    }
  }

  # Auto-select metric
  if (identical(metric, "auto")) {
    metric = if ("r2" %in% names(sub)) "r2" else if ("auc" %in% names(sub)) "auc" else names(sub)[3L]
  }
  metric = as.character(metric)
  if (!metric %in% names(sub)) {
    warning(sprintf(".autoiml_plot_g7a_subgroups: metric '%s' not in subgroup table. Available: %s",
      metric, paste(setdiff(names(sub), c("group_var", "group", "n")), collapse = ", ")))
    return(NULL)
  }

  top_n = as.integer(top_n)
  dt = sub[seq_len(min(top_n, .N))]
  dt = dt[is.finite(get(metric))]
  if (nrow(dt) == 0L) {
    return(NULL)
  }

  dt[, label := paste(group_var, group, sep = ": ")]
  dt[, label := factor(label, levels = rev(unique(label)))]

  ref_val = mean(dt[[metric]], na.rm = TRUE)

  pal = .autoiml_plot_palette()

  # Use distinct colors per group_var (up to 4 vars)
  gvars = unique(dt$group_var)
  col_vals = setNames(
    pal$metric[seq_len(min(length(gvars), length(pal$metric)))],
    gvars
  )

  metric_label = switch(metric,
    r2          = expression(R^2),
    auc         = "AUC",
    rmse        = "RMSE",
    brier       = "Brier score",
    ece         = "ECE",
    prevalence  = "Prevalence",
    as.character(metric)
  )

  ggplot2::ggplot(dt, ggplot2::aes(x = .data[[metric]], y = label, fill = group_var)) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::geom_vline(
      xintercept = ref_val, linetype = "dashed",
      color = pal$reference[["neutral"]], linewidth = 0.6
    ) +
    ggplot2::scale_fill_manual(values = col_vals, drop = FALSE) +
    ggplot2::labs(
      title = sprintf("G7A: Subgroup audit (%s)", metric),
      x     = metric_label,
      y     = NULL,
      fill  = NULL
    ) +
    .autoiml_theme_iml() +
    ggplot2::theme(legend.position = "bottom")
}
