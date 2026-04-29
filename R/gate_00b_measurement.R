# FILE: R/gate_00b_measurement.R

#' @title Gate 0B: Measurement / Construct Readiness
#'
#' @description
#' Screens whether measurement/construct considerations are sufficiently addressed
#' \strong{before} modeling and explanation.
#'
#' This gate is deliberately conservative: many psychometric properties (reliability,
#' invariance/comparability) cannot be inferred from the modeling dataset alone.
#' Instead, users can provide a minimal measurement specification via `ctx$measurement`
#' (or `config = list(measurement = list(...))`).
#'
#' In high-stakes decision/deployment settings, missing critical measurement evidence
#' (measurement level, reliability, and subgroup comparability when subgroup analyses
#' are in scope) is treated as a failure.
#'
#' @section Automatic Diagnostics:
#' \itemize{
#'   \item Summarize missingness rates (feature-level) and flag high missingness
#'   \item Detect whether the task provides subgroup variables (via `ctx$sensitive_features`)
#' }
#'
#' @section User Input Required:
#' \itemize{
#'   \item Reliability evidence (e.g., omega/alpha for scale scores, test-retest)
#'   \item Invariance/comparability evidence across relevant groups
#'   \item Whether predictors are composites/scale scores vs. single items vs. latent scores
#' }
#'
#' @name Gate0BMeasurement
#' @keywords internal
NULL

Gate0BMeasurement = R6::R6Class(
  "Gate0BMeasurement",
  inherit = Gate,
  public = list(
    initialize = function() {
      super$initialize(
        id = "G0B",
        name = "Measurement readiness",
        pdr = "P"
      )
    },

    run = function(ctx) {
      task = ctx$task

      claim = .autoiml_as_list(ctx$claim)
      purpose = claim$purpose %??% ctx$purpose %??% "exploratory"
      purpose = .autoiml_validate_purpose(purpose)

      stakes = tolower(as.character(claim$stakes %??% "medium")[1L])
      if (!stakes %in% c("low", "medium", "high")) stakes = "medium"
      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))

      m = .autoiml_as_list(ctx$measurement)
      .autoiml_assert_known_names(
        m,
        c(
          "level", "reliability", "invariance", "construct_map",
          "missingness_warn", "missingness_plan", "scoring_pipeline"
        ),
        "ctx$measurement"
      )

      has_content = function(x) {
        if (is.null(x)) {
          return(FALSE)
        }
        if (is.list(x)) {
          vals = unlist(x, use.names = FALSE)
          vals = vals[!is.na(vals)]
          if (length(vals) == 0L) {
            return(length(x) > 0L)
          }
          return(any(nzchar(trimws(as.character(vals)))))
        }
        vals = as.character(x)
        vals = vals[!is.na(vals)]
        any(nzchar(trimws(vals)))
      }

      # user-provided metadata
      pv_cfg = .autoiml_as_list(ctx$plausible_values)
      has_pv_tasks = {
        pv_tasks = pv_cfg$pv_tasks %??% list()
        if (inherits(pv_tasks, "Task")) {
          TRUE
        } else {
          is.list(pv_tasks) && length(pv_tasks) > 0L
        }
      }

      level_user = .autoiml_normalize_measurement_level(m$level, default = NA_character_)
      level_source = if (!is.na(level_user)) "user" else if (isTRUE(has_pv_tasks)) "analysis" else "missing"
      level = if (!is.na(level_user)) level_user else if (isTRUE(has_pv_tasks)) "plausible_values" else "unknown"

      has_reliability = isTRUE(has_content(m$reliability))
      has_invariance = isTRUE(has_content(m$invariance))
      has_construct_map = isTRUE(has_content(m$construct_map))

      # automatic missingness summary
      feats = task$feature_names
      dat = tryCatch(task$data(cols = feats), error = function(e) NULL)

      miss_max = NA_real_
      miss_mean = NA_real_
      miss_tbl = NULL

      if (!is.null(dat) && length(feats) > 0L) {
        miss = vapply(feats, function(f) mean(is.na(dat[[f]])), numeric(1L))
        miss_tbl = data.table::data.table(
          feature = feats,
          missing_rate = as.numeric(miss)
        )[order(-missing_rate)]
        miss_max = max(miss, na.rm = TRUE)
        miss_mean = mean(miss, na.rm = TRUE)
      }

      miss_warn = as.numeric(m$missingness_warn %??% 0.20)
      miss_flag = isTRUE(is.finite(miss_max) && miss_max >= miss_warn)

      # detect whether subgroup vars exist
      sensitive = ctx$sensitive_features %??% task$col_roles$stratum %??% character()
      sensitive = unique(as.character(sensitive))
      sensitive = sensitive[nzchar(sensitive)]
      has_groups = length(sensitive) > 0L

      # gate decision
      status = "pass"
      msgs = character()
      critical_missing = character()
      user_has_missingness_plan = isTRUE(has_content(m$missingness_plan))
      user_has_scoring_pipeline = isTRUE(has_content(m$scoring_pipeline))

      if (identical(level_source, "analysis")) {
        msgs = c(msgs, "Inferred measurement level='plausible_values' from ctx$plausible_values$pv_tasks.")
      }

      if (identical(level, "unknown")) {
        if (isTRUE(high_stakes)) {
          status = "fail"
          critical_missing = c(critical_missing, "measurement_level")
          msgs = c(msgs, "Measurement level is not specified (item/scale/factor_score/plausible_values); this is required for high-stakes interpretation claims.")
        } else if (purpose != "exploratory") {
          status = "warn"
          msgs = c(msgs, "Measurement level not specified (item/scale/factor_score/plausible_values). In psychological applications, interpretation and transportability depend on construct definition and measurement quality.")
        }
      }

      reliability_required = level %in% c("scale", "factor_score")
      invariance_required = isTRUE(has_groups) && level %in% c("scale", "factor_score", "plausible_values")

      if (!isTRUE(user_has_missingness_plan)) {
        m$missingness_plan = if (isTRUE(is.finite(miss_max) && miss_max == 0)) {
          "No feature missingness was detected in the analyzed task data."
        } else if (is.finite(miss_max) && is.finite(miss_mean)) {
          sprintf(
            "Feature missingness is present in the analyzed task data (max %.1f%%, mean %.1f%%). Interpret handling together with the fitted resampling pipeline used in Gate 1.",
            100 * miss_max,
            100 * miss_mean
          )
        } else {
          "Read missingness handling from the analyzed task data and the fitted resampling pipeline used in Gate 1."
        }
      }

      if (!isTRUE(user_has_scoring_pipeline)) {
        m$scoring_pipeline = if (identical(level, "plausible_values") || isTRUE(has_pv_tasks)) {
          "Outcome uncertainty is represented through the supplied plausible-value tasks, and Gate 1 pools predictive metrics across those tasks."
        } else if (inherits(task, "TaskClassif")) {
          "Use the observed task target and feature representation supplied to the fitted learner pipeline in Gate 1."
        } else if (inherits(task, "TaskRegr")) {
          "Use the observed numeric task target and feature representation supplied to the fitted learner pipeline in Gate 1."
        } else {
          "Use the task target and feature representation supplied to the fitted learner pipeline in Gate 1."
        }
      }

      has_missingness_plan = isTRUE(has_content(m$missingness_plan))
      has_scoring_pipeline = isTRUE(has_content(m$scoring_pipeline))
      derived_fields = character()
      if (level_source == "analysis") {
        derived_fields = c(derived_fields, "level")
      }
      if (!isTRUE(user_has_missingness_plan)) {
        derived_fields = c(derived_fields, "missingness_plan")
      }
      if (!isTRUE(user_has_scoring_pipeline)) {
        derived_fields = c(derived_fields, "scoring_pipeline")
      }
      if (length(derived_fields) > 0L) {
        msgs = c(
          msgs,
          sprintf(
            "Derived %s from the analyzed task and fitted pipeline. Override ctx$measurement if you need a narrower measurement description.",
            paste(unique(derived_fields), collapse = ", ")
          )
        )
      }

      if (isTRUE(reliability_required) && !isTRUE(has_reliability)) {
        if (isTRUE(high_stakes)) {
          status = "fail"
          critical_missing = c(critical_missing, "reliability")
          msgs = c(msgs, "No reliability evidence provided in ctx$measurement (e.g., omega/alpha, test-retest, interrater); this is required for high-stakes use.")
        } else if (purpose != "exploratory") {
          status = "warn"
          msgs = c(msgs, "No reliability evidence provided in ctx$measurement (e.g., omega/alpha, test-retest, interrater). Consider documenting available evidence or limitations.")
        }
      }

      if (isTRUE(invariance_required) && !isTRUE(has_invariance)) {
        if (isTRUE(high_stakes)) {
          status = "fail"
          critical_missing = c(critical_missing, "invariance")
          msgs = c(msgs, "Subgroups declared (ctx$sensitive_features / Task stratum roles) but no invariance/comparability evidence provided (ctx$measurement$invariance); this is required for high-stakes subgroup claims.")
        } else if (purpose != "exploratory") {
          status = "warn"
          msgs = c(msgs, "Subgroups declared (ctx$sensitive_features / Task stratum roles) but no invariance/comparability evidence provided (ctx$measurement$invariance). Consider testing measurement invariance or justifying comparability assumptions.")
        }
      }

      if (isTRUE(miss_flag)) {
        if (identical(status, "pass")) {
          status = "warn"
        }
        msgs = c(msgs, sprintf("High missingness detected (max missing rate %.1f%% >= %.1f%%). Document missingness mechanism assumptions and handling (imputation/modeling).", 100 * miss_max, 100 * miss_warn))
      }

      if (identical(status, "fail") && length(critical_missing) > 0L) {
        summary = paste0(
          "Measurement readiness failed for high-stakes use: missing critical evidence (",
          paste(unique(critical_missing), collapse = ", "),
          ")."
        )
      } else {
        summary = "Measurement readiness screened (psychometric evidence is user-supplied when needed, and pipeline notes may be analysis-derived)."
      }

      metrics = data.table::data.table(
        measurement_level = level,
        measurement_level_source = level_source,
        reliability_required = isTRUE(reliability_required),
        has_reliability = isTRUE(has_reliability),
        invariance_required = isTRUE(invariance_required),
        has_invariance = isTRUE(has_invariance),
        has_construct_map = isTRUE(has_construct_map),
        has_missingness_plan = isTRUE(has_missingness_plan),
        has_scoring_pipeline = isTRUE(has_scoring_pipeline),
        has_groups = isTRUE(has_groups),
        n_group_vars = length(sensitive),
        high_stakes = isTRUE(high_stakes),
        critical_missing_n = length(unique(critical_missing)),
        missing_rate_max = miss_max,
        missing_rate_mean = miss_mean,
        missingness_warn = miss_warn
      )

      GateResult$new(
        gate_id = self$id,
        gate_name = self$name,
        pdr = self$pdr,
        status = status,
        summary = summary,
        metrics = metrics,
        artifacts = list(
          measurement = m,
          missingness = miss_tbl,
          group_vars = sensitive,
          derived_fields = unique(derived_fields),
          critical_missing = unique(critical_missing)
        ),
        messages = msgs
      )
    }
  )
)
