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
    gadget_top_k = 2L,

    # Optional: off-manifold support screening for marginal what-if semantics (Gate 2)
    support_check = list(
      enabled = NULL,
      sample_n = min(200L, sample_n),
      ratio_threshold = 1.5,
      k = 1L
    )
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

.autoiml_default_alt_learners = function(task, base_learner, max_n = 6L) {
  # Task-type aware defaults (avoid known incompatibilities, e.g. binary-only log_reg on multiclass).
  is_classif = inherits(task, "TaskClassif")
  is_regr = inherits(task, "TaskRegr")

  nclass = if (is_classif) length(task$class_names) else NA_integer_
  is_binary = is_classif && nclass == 2L
  is_multiclass = is_classif && nclass > 2L

  learner_ids = character(0)

  if (is_regr) {
    learner_ids = c(
      "regr.featureless",
      "regr.lm",
      "regr.rpart",
      "regr.ranger",
      "regr.xgboost",
      "regr.svm"
    )
  } else if (is_binary) {
    learner_ids = c(
      "classif.featureless",
      "classif.log_reg",
      "classif.rpart",
      "classif.ranger",
      "classif.xgboost",
      "classif.svm",
      "classif.naive_bayes"
    )
  } else if (is_multiclass) {
    learner_ids = c(
      "classif.featureless",
      "classif.multinom",
      "classif.rpart",
      "classif.ranger",
      "classif.xgboost",
      "classif.svm",
      "classif.naive_bayes"
    )
  } else {
    return(list())
  }

  out = list()

  # Exclude the base learner id if it appears in the alternatives
  base_id = base_learner$id %||% ""
  learner_ids = setdiff(learner_ids, base_id)

  for (id in learner_ids) {
    lrn = mlr3::lrn(id)

    if (is_classif && "prob" %in% lrn$predict_types) {
      lrn$predict_type = "prob"
    }

    # Handle preprocessing (e.g., missing values) consistently
    gl = as_learner(mlr3pipelines::ppl("robustify", learner = lrn, task = task) %>>% lrn)
    # capitalized aselearner id as gl id
    gl$id = toupper(gsub("_", " ", data.table::last(strsplit(id, ".", fixed = TRUE)[[1L]])))
    out[[id]] = gl
  }

  if (length(out) == 0L) {
    return(list())
  }

  max_n = as.integer(max_n)
  if (length(out) > max_n) {
    out = out[seq_len(max_n)]
  }

  unname(out)
}

.autoiml_apply_config_defaults = function(ctx, profile = c("high_resolution", "fast")) {
  profile = match.arg(profile)

  ctx$profile = profile

  defaults = .autoiml_default_config(ctx$task, ctx$learner, profile = profile)

  ctx$structure = utils::modifyList(defaults$structure, ctx$structure %||% list())
  ctx$stability = utils::modifyList(defaults$stability, ctx$stability %||% list())
  ctx$shap = utils::modifyList(defaults$shap, ctx$shap %||% list())
  ctx$multiplicity = utils::modifyList(defaults$multiplicity, ctx$multiplicity %||% list())

  # ---- Gate 0 / claim semantics defaults -----------------------------
  # Conservative defaults: associational (on-manifold / descriptive) semantics unless explicitly set.
  purpose = ctx$purpose %||% "exploratory"
  purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))

  default_claims = switch(
    purpose,
    "exploratory" = list(global = TRUE, local = FALSE, decision = FALSE),
    "global_insight" = list(global = TRUE, local = FALSE, decision = FALSE),
    "decision_support" = list(global = TRUE, local = TRUE, decision = TRUE),
    "deployment" = list(global = TRUE, local = TRUE, decision = TRUE)
  )

  default_claim = list(
    claims = default_claims,
    semantics = "associational",
    stakes = switch(
      purpose,
      "exploratory" = "low",
      "global_insight" = "medium",
      "decision_support" = "high",
      "deployment" = "high"
    ),
    audience = "technical",
    decision_spec = list(thresholds = (ctx$calibration$thresholds %||% seq(0.01, 0.99, by = 0.01))),
    actionability = list(mutable_features = NULL, immutable_features = NULL)
  )

  ctx$claim = utils::modifyList(default_claim, ctx$claim %||% list())

  # Gate 6: populate default alternative learners only when enabled
  if (isTRUE(ctx$multiplicity$enabled) && (is.null(ctx$alt_learners) || length(ctx$alt_learners) == 0L)) {
    max_n = ctx$multiplicity$max_alt_learners %||% 5L
    ctx$alt_learners = .autoiml_default_alt_learners(ctx$task, ctx$learner, max_n = max_n)
  }

  invisible(ctx)
}
