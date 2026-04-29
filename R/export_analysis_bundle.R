#' @title Save an artifact table (CSV + optional LaTeX)
#'
#' @description
#' Writes a data.frame/data.table to a CSV file at `base_path`.csv.
#' If `caption` is supplied and `knitr` is available, also writes a `.tex`
#' file. List-valued columns are flattened to pipe-separated strings before
#' writing.
#'
#' @param dt data.frame or data.table to write.
#' @param base_path (`character(1)`) Path without extension.
#' @param caption (`character(1)` | `NULL`) LaTeX caption (optional).
#' @param label (`character(1)` | `NULL`) LaTeX label (optional).
#' @param digits (`integer(1)`) Rounding digits for LaTeX table.
#' @param top_n (`integer(1)` | `NULL`) If given, keep only the first `top_n` rows.
#' @return Invisibly `TRUE` on success, `FALSE` if `dt` is empty.
#' @export
save_analysis_table = function(dt, base_path, caption = NULL, label = NULL,
  digits = 3L, top_n = NULL) {
  if (is.null(dt) || (is.data.frame(dt) && nrow(dt) == 0L)) {
    return(invisible(FALSE))
  }

  dt2 = data.table::as.data.table(dt)
  list_cols = names(dt2)[vapply(dt2, is.list, logical(1L))]
  for (lc in list_cols) {
    dt2[[lc]] = vapply(dt2[[lc]], function(x) {
      if (is.null(x)) {
        return(NA_character_)
      }
      x_chr = as.character(x)
      if (length(x_chr) == 0L) {
        return(NA_character_)
      }
      paste(x_chr, collapse = " | ")
    }, character(1L))
  }

  if (!is.null(top_n) && nrow(dt2) > top_n) {
    dt2 = dt2[seq_len(as.integer(top_n))]
  }

  data.table::fwrite(dt2, paste0(base_path, ".csv"))

  if (requireNamespace("knitr", quietly = TRUE) && !is.null(caption)) {
    tex = knitr::kable(dt2, format = "latex", booktabs = TRUE,
      caption = caption, label = label, digits = as.integer(digits))
    writeLines(tex, con = paste0(base_path, ".tex"))
  }

  invisible(TRUE)
}

#' @title Save a ggplot as PDF and/or PNG
#'
#' @description
#' Writes `p` (a ggplot2/patchwork object) to `base_path`.pdf and
#' `base_path`.png. If `p` is a language object (a `quote()`d expression),
#' it is evaluated in the calling frame first. Non-plot objects are silently
#' skipped.
#'
#' @param p ggplot2/patchwork object, or a `quote()`d expression.
#' @param base_path (`character(1)`) Path without extension.
#' @param width,height (`numeric(1)`) Plot dimensions in inches.
#' @param title (`character(1)` | `NULL`) Optional title override.
#' @return Invisibly `TRUE` if at least one file was written.
#' @export
save_analysis_plot = function(p, base_path, width = 7, height = 4.5, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  if (is.language(p)) {
    p = tryCatch(
      eval(p, envir = parent.frame()),
      error = function(e) {
        cli_inform(c("!" = "Skipped plot {.file {basename(base_path)}}: {conditionMessage(e)}"))
        NULL
      }
    )
  }

  if (is.null(p) || !(inherits(p, "gg") || inherits(p, "ggplot") || inherits(p, "patchwork"))) {
    return(invisible(FALSE))
  }

  if (!is.null(title)) {
    p = p + ggplot2::ggtitle(title)
  }

  ok = FALSE
  for (ext in c(".pdf", ".png")) {
    tryCatch(
      {
        ggplot2::ggsave(paste0(base_path, ext), p, width = width, height = height)
        ok = TRUE
      },
      error = function(e) {
        cli_inform(c("!" = "Could not save {.file {basename(base_path)}{ext}}: {conditionMessage(e)}"))
      }
    )
  }
  invisible(ok)
}

#' @title Export a comprehensive analysis bundle from a completed AutoIML run
#'
#' @description
#' Saves all standard gate artifact tables, auto-generated plots, and the
#' audit bundle for any completed [AutoIML] run to a directory tree:
#'
#' ```
#' dir/
#'   audit_bundle/   <- export_audit_bundle() output (report card, guide, RDS)
#'   tables/         <- gate artifact CSVs (G1-G7)
#'   figures/        <- PDF+PNG pairs for each auto$plot() type
#'   objects/        <- full result.rds
#' ```
#'
#' All saves are conditional — artifacts that did not run or are absent are
#' skipped silently. Extend with `extra_tables` and `extra_figures` for
#' analysis-specific additions.
#'
#' @param auto ([AutoIML]) A completed AutoIML runner.
#' @param dir (`character(1)`) Root output directory (created if needed).
#' @param prefix (`character(1)`) File name prefix for all outputs.
#' @param extra_tables (`list`) Named list of `list(dt = ..., path = ..., ...)`;
#'   each element is forwarded to [save_analysis_table()].
#' @param extra_figures (`list`) Named list of `list(p = ..., path = ..., ...)`;
#'   each element is forwarded to [save_analysis_plot()].
#' @param exclude_plot_types (`character()`) Standard `auto$plot()` types to
#'   skip when exporting the default figure set.
#' @param fig_width,fig_height (`numeric(1)`) Default figure dimensions in inches.
#' @return Invisibly, a named list of all paths written.
#' @export
export_analysis_bundle = function(auto, dir = "analysis_bundle", prefix = "run",
  extra_tables = list(), extra_figures = list(), exclude_plot_types = character(),
  fig_width = 10, fig_height = 6) {
  checkmate::assert_class(auto, "AutoIML")
  checkmate::assert_string(dir, min.chars = 1L)
  checkmate::assert_string(prefix, min.chars = 1L)
  checkmate::assert_character(exclude_plot_types, any.missing = FALSE, null.ok = FALSE)

  res = auto$result
  if (is.null(res)) {
    cli_abort("No result yet; call {.code $run()} first.")
  }

  bundle_dir = file.path(dir, "audit_bundle")
  tab_dir = file.path(dir, "tables")
  fig_dir = file.path(dir, "figures")
  obj_dir = file.path(dir, "objects")

  for (d in c(bundle_dir, tab_dir, fig_dir, obj_dir)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  paths = list()

  # ---- Audit bundle (report card, guide, RDS, IEL) -------------------------
  bundle_paths = export_audit_bundle(res, dir = bundle_dir, prefix = prefix)
  paths = c(paths, bundle_paths)

  p_rds = file.path(obj_dir, paste0(prefix, "_result.rds"))
  saveRDS(res, p_rds)
  paths$result_rds = p_rds

  # ---- Helper ---------------------------------------------------------------
  .save_tbl = function(dt, name) {
    if (is.null(dt)) {
      return(NULL)
    }
    p = file.path(tab_dir, paste0(prefix, "_", name))
    save_analysis_table(dt, p)
    p
  }

  .flatten_list_row = function(x) {
    if (!is.list(x) || length(x) == 0L) {
      return(NULL)
    }
    data.table::as.data.table(lapply(x, function(z) {
      if (is.null(z) || length(z) == 0L) {
        return(NA_character_)
      }
      if (length(z) > 1L) {
        return(paste(as.character(z), collapse = ", "))
      }
      z
    }))
  }

  # ---- G1: CV performance metrics ------------------------------------------
  g1 = .autoiml_get_gate_result(res, "G1")
  if (!is.null(g1)) {
    paths$g1_cv_scores = .save_tbl(g1$metrics, "g1_cv_scores")
  }

  # ---- G2: Interaction screening -------------------------------------------
  g2 = .autoiml_get_gate_result(res, "G2")
  if (!is.null(g2)) {
    hstats = g2$artifacts$hstats
    if (!is.null(hstats) && nrow(hstats) > 0L) {
      hs = data.table::copy(data.table::as.data.table(hstats))
      data.table::setnames(hs, c("feature1", "feature2", "hstat"),
        c("feature_1", "feature_2", "h_squared"), skip_absent = TRUE)
      paths$g2_hstats = .save_tbl(hs, "g2_hstats_pairs")
    }
    paths$g2_recommendation = .save_tbl(
      .flatten_list_row(g2$artifacts$recommendation), "g2_recommendation"
    )
    paths$g2_pint = .save_tbl(
      g2$artifacts$pint, "g2_pint_interaction_screen"
    )
    paths$g2_gadget_regions = .save_tbl(
      g2$artifacts$gadget_regions, "g2_gadget_regions"
    )
    paths$g2_gadget_splits = .save_tbl(
      g2$artifacts$gadget_splits, "g2_gadget_splits"
    )
    paths$g2_gadget_feature_metrics = .save_tbl(
      g2$artifacts$gadget_feature_metrics, "g2_gadget_feature_metrics"
    )
    paths$g2_support = .save_tbl(
      g2$artifacts$support_check, "g2_support_check"
    )
  }

  # ---- G3: Calibration and decision utility --------------------------------
  g3 = .autoiml_get_gate_result(res, "G3")
  if (!is.null(g3)) {
    paths$g3_reliability = .save_tbl(
      data.table::as.data.table(g3$artifacts$reliability), "g3_reliability"
    )
    paths$g3_dca = .save_tbl(
      data.table::as.data.table(g3$artifacts$dca), "g3_dca"
    )
  }

  # ---- G4: Faithfulness ----------------------------------------------------
  g4 = .autoiml_get_gate_result(res, "G4")
  if (!is.null(g4)) {
    paths$g4_faithfulness = .save_tbl(
      data.table::as.data.table(g4$artifacts$faithfulness_summary), "g4_faithfulness"
    )
  }

  # ---- G5: Stability -------------------------------------------------------
  g5 = .autoiml_get_gate_result(res, "G5")
  if (!is.null(g5)) {
    paths$g5_perf_ci = .save_tbl(g5$artifacts$perf_ci, "g5_stability_perf_ci")
    paths$g5_perm_imp = .save_tbl(g5$artifacts$perm_importance, "g5_perm_importance")
  }

  # ---- G6: Multiplicity and transport --------------------------------------
  g6 = .autoiml_get_gate_result(res, "G6")
  if (!is.null(g6)) {
    perf = g6$artifacts$alt_learner_performance
    rash = g6$artifacts$rashomon_set
    if (!is.null(perf) && nrow(perf) > 0L) {
      rash_set = data.table::copy(perf)
      if (!is.null(rash) && nrow(rash) > 0L) {
        rash_set[, in_rashomon := learner_id %in% rash$learner_id]
      }
      paths$g6_rashomon_set = .save_tbl(rash_set, "g6_rashomon_set")
    }
    paths$g6_pred_mult = .save_tbl(
      g6$artifacts$predictive_multiplicity, "g6_predictive_multiplicity"
    )
    imp = g6$artifacts$rashomon_importance
    if (!is.null(imp) && nrow(imp) > 0L) {
      imp_wide = data.table::dcast(
        data.table::as.data.table(imp), feature ~ learner_id, value.var = "importance"
      )
      paths$g6_rashomon_imp = .save_tbl(imp_wide, "g6_rashomon_importance")
      rank_long = data.table::melt(
        data.table::copy(imp_wide), id.vars = "feature",
        variable.name = "learner", value.name = "importance"
      )
      rank_long[, rank := data.table::frank(-importance, ties.method = "average"), by = "learner"]
      paths$g6_rashomon_ranks = .save_tbl(
        data.table::dcast(rank_long, feature ~ learner, value.var = "rank"),
        "g6_rashomon_ranks"
      )
    }
    sa = g6$artifacts$shift_assessment
    if (!is.null(sa) && !is.null(sa$transport)) {
      paths$g6_transport = .save_tbl(
        data.table::as.data.table(sa$transport), "g6_transport"
      )
    }
    paths$g6_explanation_mult = .save_tbl(
      g6$artifacts$explanation_multiplicity, "g6_explanation_multiplicity"
    )
  }

  # ---- G7A: Subgroup audit -------------------------------------------------
  g7a = .autoiml_get_gate_result(res, "G7A")
  if (!is.null(g7a)) {
    paths$g7a_subgroup = .save_tbl(g7a$artifacts$subgroup, "g7a_subgroup_audit")
  }

  # ---- Plots: all standard auto$plot() types --------------------------------
  exclude_plot_types = unique(as.character(exclude_plot_types))

  .save_plt = function(type, name, w = fig_width, h = fig_height, ...) {
    if (type %in% exclude_plot_types) {
      base = file.path(fig_dir, paste0(prefix, "_", name))
      unlink(c(paste0(base, ".pdf"), paste0(base, ".png")), force = TRUE)
      return(NULL)
    }
    p = tryCatch(auto$plot(type, ...), error = function(e) NULL)
    if (is.null(p)) {
      return(NULL)
    }
    path = file.path(fig_dir, paste0(prefix, "_", name))
    save_analysis_plot(p, path, width = w, height = h)
    path
  }

  paths$fig_overview = .save_plt("overview", "fig_overview", w = 13, h = 16)
  paths$fig_gate_strip = .save_plt("gate_strip", "fig_gate_strip", w = 10, h = 1.5)
  paths$fig_g1_scores = .save_plt("g1_scores", "fig_g1_scores", w = 8, h = 4)
  paths$fig_g2_hstats = .save_plt("g2_hstats", "fig_g2_hstats", w = 8, h = 5)
  paths$fig_g2_ale2d = .save_plt("g2_ale_2d", "fig_g2_ale2d", w = 7, h = 5)
  paths$fig_g2_gadget = .save_plt("g2_gadget", "fig_g2_gadget", w = 10, h = 7)
  paths$fig_g2_gadget_tree = .save_plt("g2_gadget_tree", "fig_g2_gadget_tree", w = 9, h = 4.5)
  paths$fig_g2_pint = .save_plt("g2_pint", "fig_g2_pint", w = 8, h = 5)
  paths$fig_g3_cal = .save_plt("g3_calibration", "fig_g3_calibration", w = 6, h = 5)
  paths$fig_g3_dca = .save_plt("g3_dca", "fig_g3_dca", w = 8, h = 5)
  paths$fig_g5_stab = .save_plt("g5_stability", "fig_g5_stability", w = 8, h = 6)
  paths$fig_g6_heatmap = .save_plt("g6_rank_heatmap", "fig_g6_rank_heatmap", w = 8, h = 5)
  paths$fig_g6_perf = .save_plt("g6_performance", "fig_g6_performance", w = 7, h = 5)
  paths$fig_g6_mult = .save_plt("g6_pred_multiplicity", "fig_g6_pred_multiplicity", w = 7, h = 5)
  paths$fig_g6_loco = .save_plt("g6_loco", "fig_g6_loco", w = 11, h = 5)
  paths$fig_g7a = .save_plt("g7a_subgroups", "fig_g7a_subgroups", w = 11, h = 5)

  # Top-feature ALE/effect plot (auto-selects feature from G2 recommendation)
  top_feat = tryCatch({
    rec = res$gate_results$G2$artifacts$recommendation
    tf = rec$top_features %??% character()
    if (is.list(tf)) tf = unlist(tf, use.names = FALSE)
    as.character(tf[1L])
  }, error = function(e) NULL)
  if (!is.null(top_feat) && nzchar(top_feat)) {
    paths$fig_g2_effect = .save_plt("g2_effect", "fig_g2_effect",
      w = fig_width, h = fig_height, feature = top_feat)
  }

  # ---- Extra tables and figures provided by caller -------------------------
  for (nm in names(extra_tables)) {
    spec = extra_tables[[nm]]
    save_analysis_table(spec$dt, spec$path, caption = spec$caption %??% NULL,
      label = spec$label %??% NULL, digits = spec$digits %??% 3L,
      top_n = spec$top_n %??% NULL)
    paths[[paste0("extra_table_", nm)]] = spec$path
  }

  for (nm in names(extra_figures)) {
    spec = extra_figures[[nm]]
    save_analysis_plot(spec$p, spec$path,
      width = spec$width %??% fig_width,
      height = spec$height %??% fig_height,
      title = spec$title %??% NULL)
    paths[[paste0("extra_figure_", nm)]] = spec$path
  }

  invisible(Filter(Negate(is.null), paths))
}
