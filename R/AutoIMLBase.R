# FILE: R/AutoIMLBase.R

#' AutoIML: Gate-based AutoML/IML workflow
#'
#' `AutoIML` orchestrates a gate-based interpretability workflow (G1–G7) for
#' [`mlr3`][mlr3::mlr3-package] tasks and learners. It produces a reproducible
#' audit trail and data-/task-dependent interpretability artifacts.
#'
#' Run the workflow with `$run()`; inspect results with `$overview()` and
#' `$report_card()`; retrieve non-plot tables via `$tables()`; and generate
#' ggplot2 outputs via `$plot(type=...)`.
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @seealso [autoiml()] for a functional wrapper.
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("penguins")
#' learner = lrn("classif.rpart", predict_type = "prob")
#' resampling = rsmp("cv", folds = 5)
#' auto = AutoIML$new(task = task, learner = learner, resampling = resampling,
#'   purpose = "global_insight", quick_start = FALSE, seed = 123)
#' result = auto$run(verbose = TRUE)
#' auto$report_card()
#' auto$tables("g2")
#' # auto$plot("g2_effect", feature = "body_mass", class_label = "Adelie")
AutoIML = R6::R6Class(
  "AutoIML",
  public = list(
    #' @field task (`mlr3::Task`)
    #' The task to be analyzed.
    task = NULL,

    #' @field learner (`mlr3::Learner`)
    #' The learner used to fit the predictive model.
    learner = NULL,

    #' @field resampling (`mlr3::Resampling`)
    #' Resampling strategy used for performance estimation (Gate 1) and stability checks.
    resampling = NULL,

    #' @field purpose (`character(1)`)
    #' Purpose of the analysis. Controls defaults and the overall claim scope.
    purpose = NULL,

    #' @field quick_start (`logical(1)`)
    #' If `TRUE`, use a lighter configuration (fewer or cheaper artifacts).
    quick_start = NULL,

    #' @field seed (`integer(1)`)
    #' Random seed used for stochastic components.
    seed = NULL,

    #' @field ctx (`list`)
    #' Mutable context/configuration passed to gates. You may set `auto$ctx$...` before calling `$run()`.
    ctx = NULL,

    #' @field gates (`list`)
    #' List of gate objects to execute (G1..G7).
    gates = NULL,

    #' @field result (`AutoIMLResult`)
    #' Latest result produced by `$run()`.
    result = NULL,

    #' @description
    #' Create a new `AutoIML` runner.
    #'
    #' @param task (`mlr3::Task`)
    #'   Task to analyze.
    #' @param learner (`mlr3::Learner`)
    #'   Learner used to fit the predictive model.
    #' @param resampling (`mlr3::Resampling`)
    #'   Resampling strategy used for Gate 1 and related diagnostics.
    #' @param purpose (`character(1)`)
    #'   Intended use. Influences default thresholds and which gates are executed.
    #' @param quick_start (`logical(1)`)
    #'   If `TRUE`, reduce compute (fewer artifacts, smaller samples).
    #' @param seed (`integer(1)`)
    #'   Random seed for stochastic steps.
    #' @param config (`list`)
    #'   Optional configuration list merged into `ctx`.
    #' @return A new `AutoIML` object.
    initialize = function(task, learner, resampling,
      purpose = c("exploratory", "global_insight", "decision_support"),
      quick_start = TRUE,
      seed = 1L, config = list()) {
      checkmate::assert_class(task, "Task")
      checkmate::assert_class(learner, "Learner")
      checkmate::assert_class(resampling, "Resampling")
      purpose = match.arg(purpose)
      checkmate::assert_flag(quick_start)
      checkmate::assert_int(seed, lower = 0)

      self$task = task
      self$learner = learner
      self$resampling = resampling
      self$purpose = purpose
      self$quick_start = quick_start
      self$seed = seed

      # Context is an environment to allow in-place configuration:
      # auto$ctx$structure$sample_n <- 200
      self$ctx = new.env(parent = emptyenv())
      self$ctx$task = task
      self$ctx$learner = learner
      self$ctx$resampling = resampling
      self$ctx$purpose = purpose
      self$ctx$quick_start = quick_start
      self$ctx$seed = seed

      # Dedicated per-gate configuration lists
      self$ctx$structure = list()
      self$ctx$calibration = list()
      self$ctx$faithfulness = list()
      self$ctx$stability = list()
      self$ctx$multiplicity = list()
      self$ctx$human = list()

      # Apply user-supplied config list into ctx (shallow merge per top-level key)
      if (is.list(config) && length(config) > 0) {
        for (nm in names(config)) {
          if (is.null(nm) || nm == "") next
          val = config[[nm]]
          if (is.list(val) && is.list(self$ctx[[nm]])) {
            self$ctx[[nm]] = utils::modifyList(self$ctx[[nm]], val)
          } else {
            self$ctx[[nm]] = val
          }
        }
      }

      # Runtime log (filled during run)
      self$ctx$run_log = data.table::data.table(
        gate_id = character(),
        gate_name = character(),
        status = character(),
        elapsed_sec = numeric(),
        summary = character()
      )

      self$gates = private$default_gates(purpose = purpose, quick_start = quick_start)
    },

    # ---- execution -------------------------------------------------------

    #' @description
    #' Execute all configured gates and return an [AutoIMLResult].
    #'
    #' The resulting object contains the per-gate [GateResult] objects,
    #' timing information, and all computed artifacts. The latest result is also
    #' stored in the `$result` field.
    #'
    #' @param verbose (`logical(1)`)
    #'   If `TRUE`, prints the report card at the end of the run.
    #' @return ([AutoIMLResult]) Result object containing all gate outputs.
    run = function(verbose = FALSE) {
      ctx = self$ctx
      ctx$task = self$task
      ctx$learner = self$learner
      ctx$resampling = self$resampling
      ctx$purpose = self$purpose
      ctx$quick_start = self$quick_start
      ctx$seed = self$seed

      # Strict config validation (early fail)
      .autoiml_validate_ctx(ctx)

      gate_results = list()
      timings = numeric(0)

      t0 = proc.time()[3]

      for (i in seq_along(self$gates)) {

        gate_obj = self$gates[[i]]
        gstart = proc.time()[3]

        gr = tryCatch(
          gate_obj$run(ctx),
          error = function(e) {
            GateResult$new(
              gate_id = gate_obj$id,
              gate_name = gate_obj$name,
              pdr = gate_obj$pdr,
              status = "error",
              summary = paste0("Gate failed with error: ", conditionMessage(e)),
              metrics = NULL,
              artifacts = list(),
              messages = character()
            )
          }
        )

        gate_results[[i]] = gr
        names(gate_results)[[i]] = gate_obj$id

        gend = proc.time()[3]
        timings[[gate_obj$id]] = gend - gstart
      }

      irl = irl_from_gates(gate_results)
      claim_scope = private$claim_scope(irl, self$purpose)

      report = report_card(AutoIMLResult$new(
        task_id = self$task$id,
        learner_id = self$learner$id,
        purpose = self$purpose,
        quick_start = self$quick_start,
        irl = irl,
        claim_scope = claim_scope,
        gate_results = gate_results,
        report = data.table::data.table(), # placeholder; overwritten below
        timings = timings,
        extras = list()
      ))

      res = AutoIMLResult$new(
        task_id = self$task$id,
        learner_id = self$learner$id,
        purpose = self$purpose,
        quick_start = self$quick_start,
        irl = irl,
        claim_scope = claim_scope,
        gate_results = gate_results,
        report = report,
        timings = timings,
        extras = list(ctx = ctx)
      )

      self$result = res

      if (isTRUE(verbose)) {
        cat("\n=== AutoIML run complete ===\n")
        print(res$report)
        cat("\nUse `$overview()` for a richer audit trail and `$plot(type=...)` for visual outputs.\n")
      }

      res
    },

    # ---- tables (non-plot) ----------------------------------------------

    #' @description
    #' Return the report card for the most recent `$run()` call.
    #'
    #' This is the same table as [report_card()] but scoped to the object.
    #'
    #' @return A `data.table` with one row per gate.
    report_card = function() {
      if (is.null(self$result)) stop("No result available: call $run() first.", call. = FALSE)
      self$result$report
    },

    #' @description
    #' Return key tables produced by the workflow.
    #'
    #' @param which (`character(1)`)
    #'   Which tables to return. Currently supports `"all"` and gate-specific options.
    #' @param ... Additional arguments forwarded to table helpers.
    #' @return A named list of tables (usually `data.table`s).
    tables = function(which = c("g2"), ...) {
      if (is.null(self$result)) stop("No result available: call $run() first.", call. = FALSE)
      which = match.arg(which)
      if (which == "g2") {
        return(gate2_tables(self$result, ...))
      }
      stop("Unknown table group: ", which, call. = FALSE)
    },

    #' @description
    #' Print a structured overview of the latest run (audit trail).
    #'
    #' @return Invisibly returns `self`.
    overview = function() {
      if (is.null(self$result)) stop("No result available: call $run() first.", call. = FALSE)

      res = self$result
      cat("\n=== AutoIML Overview ===\n")
      cat("Task: ", res$task_id, " | Learner: ", res$learner_id, " | Resampling: ", self$resampling$id, "\n", sep = "")
      cat("Purpose: ", res$purpose, " | quick_start: ", res$quick_start, " | seed: ", self$seed, "\n", sep = "")
      cat("IRL: ", res$irl, "\n", sep = "")
      cat("Claim scope: ", res$claim_scope, "\n\n", sep = "")

      print(res$report)

      cat("\nArtifacts per gate (keys only):\n")
      for (gid in names(res$gate_results)) {
        gr = res$gate_results[[gid]]
        ks = names(gr$artifacts)
        if (length(ks) == 0L) ks = "<none>"
        cat("- ", gid, " (", gr$status, "): ", paste(ks, collapse = ", "), "\n", sep = "")
      }

      cat("\nTiming (sec):\n")
      for (gid in names(res$timings)) {
        cat("- ", gid, ": ", sprintf("%.3f", res$timings[[gid]]), "\n", sep = "")
      }
      cat("Total: ", sprintf("%.3f", sum(unlist(res$timings))), "\n", sep = "")

      invisible(res)
    },

    # ---- plots -----------------------------------------------------------

    #' @description
    #' Create ggplot2-based visual outputs from the latest run.
    #'
    #' @param type (`character(1)`)
    #'   Plot type identifier. Supported values include:
    #'
    #'   - `"g1_scores"`: Gate 1 CV score trend.
    #'   - `"g2_effect"`: Gate 2 global effect (PDP/ALE) with optional ICE overlay.
    #'   - `"g2_ice_spread"`: Gate 2 ICE spread summary.
    #'   - `"g2_hstats"`: Gate 2 interaction screening (H-statistics).
    #'   - `"g2_gadget"`: Gate 2 regionalization summary (if available).
    #'   - `"shap_local"`: Local SHAP/Shapley breakdown for a selected row.
    #' @param ... Additional arguments forwarded to the selected plot helper.
    #' @return A `ggplot` object or a `patchwork` composition.
    plot = function(type, ...) {
      if (is.null(self$result)) stop("No result yet; call $run() first.", call. = FALSE)

      type = as.character(type)[1L]
      if (!nzchar(type)) stop("`type` must be a non-empty string.", call. = FALSE)

      switch(
        type,
        "g1_scores" = .autoiml_plot_g1_scores(self$result, ...),
        "g2_ice_spread" = .autoiml_plot_g2_ice_spread(self$result, ...),
        "g2_effect" = .autoiml_plot_g2_effect(self$result, ...),
        "g2_hstats" = .autoiml_plot_g2_hstats(self$result, ...),
        "g2_gadget" = .autoiml_plot_g2_gadget(self$result, ...),
        "shap_local" = .autoiml_plot_shap_local(self, ...),
        "shap_importance" = .autoiml_plot_shap_importance(self, ...),
        "shap_beeswarm" = .autoiml_plot_shap_beeswarm(self, ...),
        stop("Unknown plot type: ", type, call. = FALSE)
      )
    },

    # ---- SHAP / Shapley --------------------------------------------------

    #' @description
    #' Compute local Shapley values (SHAP) for one observation.
    #'
    #' Internally uses a Shapley value decomposition (via the `iml` package),
    #' wrapped to work on `mlr3` learners.
    #'
    #' @param row_id (`integer(1)` | `character(1)`)
    #'   Row id of the observation to explain (must exist in `task`).
    #' @param class_label (`character(1)` | `NULL`)
    #'   For classification tasks, the class to explain. Defaults to the first
    #'   class level if `NULL`. Ignored for regression.
    #' @param sample_size (`integer(1)`)
    #'   Number of Monte Carlo samples used for the Shapley estimation.
    #' @param background_n (`integer(1)`)
    #'   Number of background observations used for the reference distribution.
    #' @param seed (`integer(1)` | `NULL`)
    #'   Optional seed for reproducibility.
    #' @return A `data.table` with feature-level Shapley contributions.
    shap = function(row_id, class_label = NULL, sample_size = 100L, background_n = 200L, seed = NULL) {
      ctx = self$ctx
      task = ctx$task
      model = ctx$final_model
      if (is.null(model)) stop("No trained final model found. Run $run() first.", call. = FALSE)

      feats = task$feature_names

      x_interest = task$data(rows = row_id, cols = feats)
      bg_n = min(as.integer(background_n), task$nrow)
      bg_rows = sample(task$row_ids, size = bg_n)
      background = task$data(rows = bg_rows, cols = feats)

      cls = if (!is.null(class_label)) as.character(class_label) else NULL
      seed_use = seed %||% (ctx$seed %||% 1L)

      .autoiml_shapley_iml(
        task = task,
        model = model,
        x_interest = x_interest,
        background = background,
        sample_size = as.integer(sample_size),
        seed = as.integer(seed_use),
        class_labels = cls
      )
    }
  ),

  private = list(
    default_gates = function(purpose, quick_start) {
      # NOTE: quick_start is implemented by selecting fewer gates.
      gates = list(
        Gate1Validity$new(),
        Gate2Structure$new(),
        Gate3Calibration$new(),
        Gate4Faithfulness$new(),
        Gate5Stability$new(),
        Gate6Multiplicity$new(),
        Gate7HumanFairness$new()
      )

      if (isTRUE(quick_start)) {
        # Keep the runtime predictable: run preflight + core structure + minimal decision checks.
        gates = gates[1:4]
      }

      gates
    },

    derive_irl = function(gate_results) {
      # Simple heuristic mapping from gate statuses to an IML Readiness Level label.
      sts = vapply(gate_results, function(gr) gr$status, character(1))
      if (any(sts == "error")) {
        return("IRL-0")
      }
      if (any(sts == "warn")) {
        return("IRL-1")
      }
      "IRL-2"
    },

    claim_scope = function(irl, purpose) {
      if (irl == "IRL-0") {
        return("Exploratory only: use for pipeline auditing and hypothesis generation; avoid substantive global/local claims.")
      }
      if (purpose == "global_insight") {
        if (irl == "IRL-1") {
          return("Cautious global insight: describe global model structure on observed support; avoid case-level guidance and causal language.")
        }
        return("Global insight: report effects/importance with diagnostics and uncertainty; still avoid causal language.")
      }
      if (purpose == "decision_support") {
        if (irl == "IRL-1") {
          return("Constrained decision support: require calibration/utility checks; use explanations as decision aids with conservative thresholds.")
        }
        return("Decision support: calibrated, utility-aware reporting; explanations may be used per case with documented limits.")
      }
      "Exploratory: emphasize diagnostics and limitations."
    }
  )
)
