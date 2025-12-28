# FILE: R/config_profiles.R
#
# Smart, dataset-dependent defaults for computational budgets and gate configs.
#
# High-level idea:
# - profile = "high_resolution" (default): more stable / less noisy diagnostics
# - profile = "fast": smaller samples and grids; suitable for iteration
#
# User values in ctx always override defaults.

# @keywords internal
NULL

.autoiml_default_profile = function(profile = c("high_resolution", "fast")) {
  match.arg(profile)
}

.autoiml_default_config = function(task, learner, profile = c("high_resolution", "fast")) {
  profile = .autoiml_default_profile(profile)

  n = as.integer(task$nrow)
  p = as.integer(length(task$feature_names))

  ft = tryCatch(task$feature_types, error = function(e) NULL)
  p_num = if (!is.null(ft) && all(c("id", "type") %in% names(ft))) {
    sum(ft$type %in% c("numeric", "integer"))
  } else {
    p
  }

  clamp = function(x, lo, hi) {
    x = as.integer(round(x))
    max(as.integer(lo), min(as.integer(hi), x))
  }

  # ---- Profile-specific budgets -----------------------------------------
  if (profile == "fast") {
    sample_n = min(n, clamp(0.10 * n, 200L, 1000L))
    grid_n = clamp(10 + log10(n + 1) * 2, 10L, 16L)
    ale_bins = clamp(grid_n, 10L, 16L)
    ice_keep_n = min(sample_n, 30L)
    max_features = min(p_num, 5L)
    hstat_max_features = min(p_num, 4L)
    hstat_grid_n = 6L

    stability_B = clamp(10 + log10(n + 1) * 2, 12L, 20L)
    stability_max_features = min(p, 10L)

    shap_sample_size = 60L
    shap_background_n = min(n, 200L)

    multiplicity_enabled = FALSE
    multiplicity_max_alt = 3L
  } else {
    sample_n = min(n, clamp(0.30 * n, 400L, 5000L))
    grid_n = clamp(18 + log10(n + 1) * 3, 18L, 30L)
    ale_bins = clamp(grid_n * 0.8, 12L, 25L)
    ice_keep_n = min(sample_n, 100L)
    max_features = min(p_num, clamp(sqrt(max(1, p_num)), 5L, 12L))
    hstat_max_features = min(p_num, clamp(sqrt(max(1, p_num)) + 2, 4L, 8L))
    hstat_grid_n = 8L

    stability_B = clamp(15 + log10(n + 1) * 3, 18L, 40L)
    stability_max_features = min(p, 15L)

    shap_sample_size = 200L
    shap_background_n = min(n, 1000L)

    multiplicity_enabled = TRUE
    multiplicity_max_alt = 5L
  }

  # ---- Gate 2: Structure ------------------------------------------------
  structure = list(
    sample_n = sample_n,
    max_features = max_features,
    ice_keep_n = ice_keep_n,

    grid_n = grid_n,
    grid_type = "equidist",
    ice_center = "mean",

    ale_bins = ale_bins,

    cor_threshold = 0.70,

    hstat_max_features = hstat_max_features,
    hstat_grid_n = hstat_grid_n,
    hstat_threshold = 0.20,

    regionalize = (profile != "fast"),
    gadget_max_depth = if (profile == "fast") 3L else 4L,
    gadget_min_bucket = max(25L, floor(sample_n / 8L)),
    gadget_gamma = 0.00,
    gadget_top_k = 2L
  )

  # ---- Gate 5: Stability ------------------------------------------------
  stability = list(
    B = stability_B,
    max_features = stability_max_features,
    grouping = NULL
  )

  # ---- SHAP defaults ----------------------------------------------------
  shap = list(
    sample_size = as.integer(shap_sample_size),
    background_n = as.integer(shap_background_n)
  )

  # ---- Gate 6 defaults --------------------------------------------------
  multiplicity = list(
    enabled = isTRUE(multiplicity_enabled),
    max_alt_learners = as.integer(multiplicity_max_alt),
    # Rashomon selection rule: "1se" is conservative and data-driven.
    rashomon_rule = "1se",
    importance_n = min(n, if (profile == "fast") 300L else 800L),
    importance_max_features = min(p, if (profile == "fast") 10L else 15L)
  )

  list(
    structure = structure,
    stability = stability,
    shap = shap,
    multiplicity = multiplicity
  )
}

.autoiml_default_alt_learners = function(task, learner, max_n = 5L) {
  stopifnot(inherits(learner, "Learner"))
  max_n = as.integer(max_n)
  if (max_n < 1L) {
    return(list())
  }

  out = list()
  out[[1L]] = learner$clone(deep = TRUE)

  base_predict_type = learner$predict_type %||% "response"

  # Keep dependencies light (stats / rpart / nnet); extend via ctx$alt_learners.
  cand = if (inherits(task, "TaskClassif")) {
    c("classif.log_reg", "classif.multinom", "classif.rpart", "classif.featureless")
  } else if (inherits(task, "TaskRegr")) {
    c("regr.lm", "regr.rpart", "regr.featureless")
  } else {
    character(0)
  }

  ids = unique(cand)
  for (id in ids) {
    if (length(out) >= max_n) break

    l = tryCatch(mlr3::lrn(id), error = function(e) NULL)
    if (is.null(l)) next

    # Align predict_type as much as possible
    if (inherits(task, "TaskClassif")) {
      if (base_predict_type == "prob" && "prob" %in% l$predict_types) {
        l$predict_type = "prob"
      } else {
        l$predict_type = l$predict_types[1L]
      }
    } else {
      l$predict_type = l$predict_types[1L]
    }

    out[[length(out) + 1L]] = l
  }

  out
}

.autoiml_apply_config_defaults = function(ctx, profile = c("high_resolution", "fast")) {
  profile = match.arg(profile)

  ctx$profile = profile

  defaults = .autoiml_default_config(ctx$task, ctx$learner, profile = profile)

  ctx$structure = utils::modifyList(defaults$structure, ctx$structure %||% list())
  ctx$stability = utils::modifyList(defaults$stability, ctx$stability %||% list())
  ctx$shap = utils::modifyList(defaults$shap, ctx$shap %||% list())
  ctx$multiplicity = utils::modifyList(defaults$multiplicity, ctx$multiplicity %||% list())

  # Gate 6: populate default alternative learners only when enabled
  if (isTRUE(ctx$multiplicity$enabled) && (is.null(ctx$alt_learners) || length(ctx$alt_learners) == 0L)) {
    max_n = ctx$multiplicity$max_alt_learners %||% 5L
    ctx$alt_learners = .autoiml_default_alt_learners(ctx$task, ctx$learner, max_n = max_n)
  }

  invisible(ctx)
}
