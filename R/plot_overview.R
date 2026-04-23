#' @title Gate Evidence Overview Plot Helper
#'
#' @description
#' Internal helper for a claim-adaptive multi-panel gate evidence overview.
#' Panels are only included when the corresponding gate ran and produced the
#' required artifacts.
#'
#' @name plot_overview
#' @keywords internal
NULL

.autoiml_overview_feature = function(result, feature = NULL) {
  if (!is.null(feature)) {
    return(as.character(feature)[1L])
  }

  g2 = .autoiml_get_gate_result(result, "G2")
  if (is.null(g2)) {
    return(NULL)
  }

  rec = g2$artifacts$recommendation %??% list()
  top_feat = as.character((rec$top_features %??% character())[1L])
  if (!is.na(top_feat) && nzchar(top_feat)) {
    return(top_feat)
  }

  pd = g2$artifacts$pd_curves
  if (!is.null(pd) && nrow(pd) > 0L) {
    return(as.character(unique(pd$feature)[1L]))
  }

  ale = g2$artifacts$ale_curves
  if (!is.null(ale) && nrow(ale) > 0L) {
    return(as.character(unique(ale$feature)[1L]))
  }

  NULL
}


.autoiml_overview_iel_lines = function(result) {
  if (inherits(result, "AutoIML")) {
    res = result$result
  } else {
    res = result
  }
  if (is.null(res)) {
    return(character())
  }

  iel = res$iel %??% list()
  lines = character()

  iel_str = paste0(
    "IEL: G=", iel$global %??% "?",
    "  L=", iel$local %??% "?",
    "  D=", iel$decision %??% "?",
    "  (overall ", iel$overall %??% "?", ")"
  )
  lines = c(lines, iel_str)

  g0a = .autoiml_get_gate_result(res, "G0A")
  if (!is.null(g0a)) {
    claim = g0a$artifacts$claim %??% list()
    parts = character()
    if (!is.null(claim$purpose)) parts = c(parts, paste0("Purpose: ", claim$purpose))
    if (!is.null(claim$semantics)) parts = c(parts, paste0("Semantics: ", claim$semantics))
    if (!is.null(claim$stakes)) parts = c(parts, paste0("Stakes: ", claim$stakes))
    if (length(parts) > 0L) lines = c(lines, paste(parts, collapse = " | "))
  }

  g1 = .autoiml_get_gate_result(res, "G1")
  if (!is.null(g1) && !is.null(g1$metrics)) {
    m = g1$metrics
    if (is.data.frame(m) && nrow(m) >= 1L) {
      num_cols = names(m)[vapply(m, is.numeric, logical(1L))]
      if (length(num_cols) > 0L) {
        val = m[[num_cols[1L]]][[1L]]
        if (is.finite(val)) {
          lines = c(lines, sprintf("G1: %s = %.4f", num_cols[1L], val))
        }
      }
    }
  }

  cs = res$claim_scope %??% list()
  if (!is.null(cs$overall) && nzchar(cs$overall)) {
    lines = c(lines, paste0("Claim: ", cs$overall))
  }

  lines
}


.autoiml_plot_overview = function(result, feature = NULL, class_label = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    cli_inform(c(
      "Package {.pkg patchwork} is required for the full gate overview.",
      "i" = "Install it with {.code install.packages('patchwork')}."
    ))
  }

  feature_use = .autoiml_overview_feature(result, feature = feature)

  g2 = .autoiml_get_gate_result(result, "G2")
  rec = if (!is.null(g2)) g2$artifacts$recommendation %??% list() else list()
  method_pref = as.character(rec$recommended_effect_method %??% "auto")

  # ---- Individual panels ---------------------------------------------------

  # Gate strip (row 0)
  p_strip = tryCatch(.autoiml_plot_gate_strip(result), error = function(e) NULL)

  # IEL text panel (row 1)
  iel_lines = .autoiml_overview_iel_lines(result)
  p_text = if (length(iel_lines) > 0L) {
    tryCatch(
      .autoiml_plot_text_panel(iel_lines, title = "Claim summary"),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  # G1 performance (row 2, left)
  p_g1 = tryCatch(
    .autoiml_plot_g1_scores(result),
    error = function(e) NULL
  )

  # G3 DCA (row 2, right) — or G6 performance if G3 not run
  p_g3 = tryCatch(.autoiml_plot_g3_dca(result), error = function(e) NULL)
  if (is.null(p_g3)) {
    p_g3 = tryCatch(.autoiml_plot_g6_performance(result), error = function(e) NULL)
  }

  # G2 ALE (row 3, left)
  p_g2_ale = if (!is.null(feature_use) && nzchar(feature_use)) {
    tryCatch(
      .autoiml_plot_g2_effect(
        result  = result,
        feature = feature_use,
        method  = method_pref,
        class_label = class_label,
        show_ice = identical(method_pref, "pdp")
      ),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  # G2 H-stats (row 3, right)
  p_g2_h = tryCatch(.autoiml_plot_g2_hstats(result, top_n = 10L), error = function(e) NULL)

  # G5 stability (row 4, left)
  p_g5 = tryCatch(.autoiml_plot_g5_stability(result, top_n = 12L), error = function(e) NULL)

  # G6 rank heatmap (row 4, right)
  p_g6h = tryCatch(.autoiml_plot_g6_rank_heatmap(result, top_n = 10L), error = function(e) NULL)
  if (is.null(p_g6h)) {
    p_g6h = tryCatch(.autoiml_plot_g6_rashomon_importance(result, top_n = 10L), error = function(e) NULL)
  }

  # G7A subgroups (row 5, full width — only when ran)
  p_g7a = tryCatch(.autoiml_plot_g7a_subgroups(result), error = function(e) NULL)

  # ---- Compose with patchwork ---------------------------------------------
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    return(Filter(Negate(is.null), list(
      gate_strip = p_strip,
      iel_text   = p_text,
      g1         = p_g1,
      g3_or_g6   = p_g3,
      g2_ale     = p_g2_ale,
      g2_hstats  = p_g2_h,
      g5         = p_g5,
      g6_heatmap = p_g6h,
      g7a        = p_g7a
    )))
  }

  # Row assembly helpers
  .pair = function(left, right) {
    if (!is.null(left) && !is.null(right)) return(left | right)
    if (!is.null(left))  return(left)
    if (!is.null(right)) return(right)
    NULL
  }

  row2 = .pair(p_g1,    p_g3)
  row3 = .pair(p_g2_ale, p_g2_h)
  row4 = .pair(p_g5,    p_g6h)

  content_rows = Filter(Negate(is.null), list(row2, row3, row4))
  n_content = length(content_rows)

  if (n_content == 0L) {
    if (!is.null(p_strip)) return(p_strip)
    return(NULL)
  }

  heights = c(
    if (!is.null(p_strip)) 0.5   else numeric(),
    if (!is.null(p_text))  0.85  else numeric(),
    rep(1.5, n_content),
    if (!is.null(p_g7a))   1.5   else numeric()
  )

  pieces = Filter(Negate(is.null), c(
    list(p_strip),
    list(p_text),
    content_rows,
    list(p_g7a)
  ))

  Reduce(`/`, pieces) + patchwork::plot_layout(heights = heights)
}
