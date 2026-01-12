# FILE: R/gate_00b_measurement.R
#' Gate 0B: Measurement / construct readiness
#'
#' Screen whether measurement/construct considerations are sufficiently addressed
#' **before** modeling and explanation.
#'
#' This gate is deliberately conservative: many psychometric properties (reliability,
#' invariance/comparability) cannot be inferred from the modeling dataset alone.
#' Instead, users can provide a minimal measurement specification via `ctx$measurement`
#' (or `config = list(measurement = list(...))`).
#'
#' What the gate can do automatically:
#' - summarize missingness rates (feature-level) and flag high missingness
#' - detect whether the task provides subgroup variables (via `ctx$sensitive_features`)
#'
#' What requires user input:
#' - reliability evidence (e.g., omega/alpha for scale scores, test-retest)
#' - invariance/comparability evidence across relevant groups
#' - whether predictors are composites/scale scores vs. single items vs. latent scores
#'
#' @keywords internal
#' @noRd
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

      claim = ctx$claim %||% list()
      purpose = claim$purpose %||% ctx$purpose %||% "exploratory"
      purpose = match.arg(purpose, c("exploratory", "global_insight", "decision_support", "deployment"))

      stakes = tolower(as.character(claim$stakes %||% "medium")[1L])
      if (!stakes %in% c("low", "medium", "high")) stakes = "medium"
      high_stakes = isTRUE(stakes == "high" || purpose %in% c("decision_support", "deployment"))

      m = ctx$measurement %||% list()
      if (is.environment(m)) m <- as.list(m)
      if (!is.list(m)) m <- list()

      # user-provided metadata
      level = m$level %||% "unknown"
      level = tolower(as.character(level)[1L])
      level = gsub("[[:space:]-]", "_", level)

      has_reliability = !is.null(m$reliability)
      has_invariance = !is.null(m$invariance)
      has_construct_map = !is.null(m$construct_map)

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

      miss_warn = as.numeric(m$missingness_warn %||% 0.20)
      miss_flag = isTRUE(is.finite(miss_max) && miss_max >= miss_warn)

      # detect whether subgroup vars exist
      sensitive = ctx$sensitive_features %||% task$col_roles$stratum %||% character()
      sensitive = unique(as.character(sensitive))
      sensitive = sensitive[nzchar(sensitive)]
      has_groups = length(sensitive) > 0L

      # gate decision
      status = "pass"
      msgs = character()

      if (!level %in% c("item", "scale", "composite", "latent", "unknown")) {
        level = "unknown"
      }
      if (identical(level, "unknown")) {
        if (isTRUE(high_stakes) || purpose != "exploratory") {
          status = "warn"
          msgs = c(msgs, "Measurement level not specified (item/scale/composite/latent). In psychological applications, interpretation and transportability depend on construct definition and measurement quality.")
        }
      }

      if (!isTRUE(has_reliability) && (isTRUE(high_stakes) || purpose != "exploratory")) {
        status = "warn"
        msgs = c(msgs, "No reliability evidence provided in ctx$measurement (e.g., omega/alpha, test-retest, interrater). Consider documenting available evidence or limitations.")
      }

      if (isTRUE(has_groups) && !isTRUE(has_invariance) && (isTRUE(high_stakes) || purpose != "exploratory")) {
        status = "warn"
        msgs = c(msgs, "Subgroups declared (ctx$sensitive_features / Task stratum roles) but no invariance/comparability evidence provided (ctx$measurement$invariance). Consider testing measurement invariance or justifying comparability assumptions.")
      }

      if (isTRUE(miss_flag)) {
        status = "warn"
        msgs = c(msgs, sprintf("High missingness detected (max missing rate %.1f%% >= %.1f%%). Document missingness mechanism assumptions and handling (imputation/modeling).", 100 * miss_max, 100 * miss_warn))
      }

      summary = "Measurement readiness screened (requires user-supplied psychometric evidence; missingness summarized)."

      metrics = data.table::data.table(
        measurement_level = level,
        has_reliability = isTRUE(has_reliability),
        has_invariance = isTRUE(has_invariance),
        has_construct_map = isTRUE(has_construct_map),
        has_groups = isTRUE(has_groups),
        n_group_vars = length(sensitive),
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
          group_vars = sensitive
        ),
        messages = msgs
      )
    }
  )
)
