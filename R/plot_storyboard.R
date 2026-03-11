#' @title Guided Storyboard Plot Helper
#'
#' @description
#' Internal helper for a visual-first walkthrough of key evidence.
#'
#' @name plot_storyboard
#' @keywords internal
NULL

.autoiml_storyboard_feature = function(result, feature = NULL) {
  if (!is.null(feature)) {
    return(as.character(feature)[1L])
  }

  g2 = .autoiml_get_gate_result(result, "G2")
  if (is.null(g2)) {
    return(NULL)
  }

  rec = g2$artifacts$recommendation %||% list()
  top_feat = as.character((rec$top_features %||% character())[1L])
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


.autoiml_plot_storyboard = function(result, feature = NULL, class_label = NULL) {
  feature_use = .autoiml_storyboard_feature(result, feature = feature)

  if (is.null(feature_use) || !nzchar(feature_use)) {
    return(NULL)
  }

  g2 = .autoiml_get_gate_result(result, "G2")
  rec = g2$artifacts$recommendation %||% list()
  method_pref = as.character(rec$recommended_effect_method %||% "auto")

  p_effect = .autoiml_plot_g2_effect(
    result = result,
    feature = feature_use,
    method = method_pref,
    class_label = class_label,
    show_ice = identical(method_pref, "pdp")
  )
  p_hstats = .autoiml_plot_g2_hstats(result, top_n = 10L)
  p_g6 = .autoiml_plot_g6_summary(result, top_n = 10L)

  plots = Filter(Negate(is.null), list(p_effect, p_hstats, p_g6))
  if (length(plots) == 0L) {
    return(NULL)
  }

  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(patchwork::wrap_plots(plots, ncol = 1L))
  }

  plots
}
