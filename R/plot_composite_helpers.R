# FILE: R/plot_composite_helpers.R

#' @title Composite Plotting Helpers
#'
#' @description
#' Internal helpers for composite and diagnostic plots:
#' \itemize{
#'   \item A shared internal color palette (\code{.autoiml_plot_palette()})
#'   \item A gate-status tile strip (\code{.autoiml_plot_gate_strip()})
#'   \item A text-panel diagnostic display (\code{.autoiml_plot_text_panel()})
#'   \item A Rashomon feature-importance rank heatmap (\code{.autoiml_plot_g6_rank_heatmap()})
#' }
#'
#' @name plot_composite_helpers
#' @keywords internal
NULL

# ---- Shared color palette ---------------------------------------------------

#' @keywords internal
.autoiml_plot_palette = function() {
  list(
    status = c(
      pass  = "#4FA467",
      warn  = "#E0A24F",
      skip  = "#9B9B9B",
      fail  = "#C44E52",
      error = "#C44E52"
    ),
    metric = c(
      primary   = "#4C72B0",
      secondary = "#C44E52",
      tertiary  = "#6E90C9",
      quaternary = "#2F5D9B"
    ),
    gradient = c(
      low  = "#4C72B0",
      mid  = "#E6DED3",
      high = "#C44E52"
    ),
    surface = c(
      panel     = "#EEF4FA",
      threshold = "#DCE8F6"
    ),
    reference = c(
      neutral = "grey50",
      alert   = "#C44E52"
    )
  )
}

# ---- Gate status strip ------------------------------------------------------

#' @keywords internal
.autoiml_plot_gate_strip = function(result, title = "Gate status") {
  if (!.autoiml_require_pkg("ggplot2")) {
    stop("Plotting requires package 'ggplot2'. Please install it.", call. = FALSE)
  }

  grs = if (inherits(result, "AutoIML")) {
    result$result$gate_results
  } else if (inherits(result, "AutoIMLResult")) {
    result$gate_results
  } else {
    NULL
  }

  if (is.null(grs) || length(grs) == 0L) {
    return(NULL)
  }

  pal = .autoiml_plot_palette()
  ids  = vapply(grs, function(g) as.character(g$gate_id), character(1L))
  stts = vapply(grs, function(g) as.character(g$status %||% "skip"), character(1L))

  dt = data.table::data.table(
    gate_id = factor(ids, levels = ids),
    status  = stts
  )

  all_statuses = c("pass", "warn", "skip", "fail", "error")
  fill_vals = pal$status[all_statuses]

  ggplot2::ggplot(dt, ggplot2::aes(x = gate_id, y = 1L, fill = status)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8, height = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(as.character(gate_id), "\n", toupper(status))
      ),
      color = "white", fontface = "bold", size = 3.3
    ) +
    ggplot2::scale_fill_manual(values = fill_vals, drop = FALSE) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "none"
    )
}

# ---- Text panel -------------------------------------------------------------

#' @keywords internal
.autoiml_plot_text_panel = function(lines, title = NULL) {
  if (!.autoiml_require_pkg("ggplot2")) {
    stop("Plotting requires package 'ggplot2'. Please install it.", call. = FALSE)
  }

  lines = as.character(lines)
  if (length(lines) == 0L) {
    lines = "(no content)"
  }

  pal = .autoiml_plot_palette()
  fill_col = pal$surface[["panel"]]
  n = length(lines)

  dt = data.table::data.table(
    x          = 0,
    y          = rev(seq_len(n)),
    line_label = lines
  )

  ggplot2::ggplot(dt, ggplot2::aes(x = x, y = y, label = line_label)) +
    ggplot2::annotate(
      "rect",
      xmin = -0.05, xmax = 1.05,
      ymin = 0.4,   ymax = n + 0.8,
      fill  = fill_col, color = NA
    ) +
    ggplot2::geom_text(hjust = 0, vjust = 1, lineheight = 1.15, size = 3.2) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0.5, n + 0.8) +
    {
      if (!is.null(title)) {
        ggplot2::labs(title = title)
      } else {
        ggplot2::labs()
      }
    } +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

# ---- G6 Rashomon rank heatmap -----------------------------------------------

#' @keywords internal
.autoiml_plot_g6_rank_heatmap = function(result, top_n = 12L) {
  if (!.autoiml_require_pkg("ggplot2")) {
    stop("Plotting requires package 'ggplot2'. Please install it.", call. = FALSE)
  }

  gr = .autoiml_get_gate_result(result, "G6")
  if (is.null(gr)) {
    return(NULL)
  }

  imp = gr$artifacts$rashomon_importance
  if (is.null(imp) || nrow(imp) == 0L) {
    return(NULL)
  }

  imp = data.table::as.data.table(imp)
  top_n = as.integer(top_n)

  # Select top features by mean importance
  top_feats = imp[, .(m = mean(importance, na.rm = TRUE)), by = feature][
    order(-m)][seq_len(min(top_n, .N))]$feature

  imp = imp[feature %in% top_feats]
  imp[, rank := data.table::frank(-importance, ties.method = "average"), by = "learner_id"]

  pal = .autoiml_plot_palette()

  dt_plot = data.table::copy(imp)
  dt_plot[, learner := factor(learner_id, levels = unique(learner_id))]
  dt_plot[, feature := factor(feature, levels = rev(top_feats))]

  ggplot2::ggplot(dt_plot, ggplot2::aes(
    x = learner, y = feature, fill = rank
  )) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f", rank)),
      size = 3
    ) +
    ggplot2::scale_fill_gradient(
      low  = pal$gradient[["low"]],
      high = pal$gradient[["high"]]
    ) +
    ggplot2::labs(
      title = "G6: Rashomon feature importance ranks",
      x = NULL, y = NULL, fill = "Rank"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "plain", hjust = 0),
      panel.grid    = ggplot2::element_blank(),
      axis.text.x   = ggplot2::element_text(angle = 20, hjust = 1)
    )
}
