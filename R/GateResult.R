# FILE: R/GateResult.R

#' @title GateResult Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Result object returned by a diagnostic gate (G1--G7).
#'
#' A [GateResult] contains a standardized status (`pass`, `warn`, `fail`, `skip`,
#' `error`), a short natural-language summary, optional numeric metrics, and
#' optional artifacts (tables and intermediate objects) used for reproducibility
#' and plotting.
#'
#' @section Fields:
#' * `gate_id` :: `character(1)`\cr
#'   Short gate identifier, e.g. `"G2"`.
#' * `gate_name` :: `character(1)`\cr
#'   Human-readable gate name.
#' * `pdr` :: `character(1)`\cr
#'   Stage label used by the framework (e.g. `"P"`, `"D"`, `"R"`).
#' * `status` :: `character(1)`\cr
#'   One of `pass`, `warn`, `fail`, `skip`, `error`.
#' * `summary` :: `character(1)`\cr
#'   Short summary of what was found and the implied recommendations.
#' * `metrics` :: [data.table::data.table] | `NULL`\cr
#'   Optional gate-specific numeric metrics.
#' * `artifacts` :: `list()` | `NULL`\cr
#'   Optional gate-specific artifacts (tables, models, etc.).
#' * `messages` :: `character()` | `NULL`\cr
#'   Optional additional messages and caveats.
#'
#' @export
GateResult = R6::R6Class(
  "GateResult",
  public = list(
    #' @field gate_id (`character(1)`)
    #' Gate identifier (e.g. `"G2"`).
    gate_id = NULL,

    #' @field gate_name (`character(1)`)
    #' Human-readable gate name.
    gate_name = NULL,

    #' @field pdr (`character(1)`)
    #' Stage label (e.g. `"P"`, `"D"`, `"R"`).
    pdr = NULL,

    #' @field status (`character(1)`)
    #' One of `pass`, `warn`, `fail`, `skip`, `error`.
    status = NULL,

    #' @field summary (`character(1)`)
    #' Short human-readable summary.
    summary = NULL,

    #' @field metrics (`data.table::data.table` | `NULL`)
    #' Optional gate-specific numeric metrics.
    metrics = NULL,

    #' @field artifacts (`list()` | `NULL`)
    #' Optional gate-specific artifacts (tables, intermediate objects, etc.).
    artifacts = NULL,

    #' @field messages (`character()` | `NULL`)
    #' Optional additional messages / caveats.
    messages = NULL,

    #' @description
    #' Create a new `GateResult`.
    #'
    #' @param gate_id (`character(1)`) Gate identifier.
    #' @param gate_name (`character(1)`) Human-readable gate name.
    #' @param pdr (`character(1)`) Stage label.
    #' @param status (`character(1)`) Gate status.
    #' @param summary (`character(1)`) Short summary.
    #' @param metrics (`data.table::data.table` | `NULL`) Optional metrics.
    #' @param artifacts (`list()` | `NULL`) Optional artifacts.
    #' @param messages (`character()` | `NULL`) Optional messages.
    initialize = function(gate_id, gate_name, pdr, status, summary,
      metrics = NULL, artifacts = NULL, messages = NULL) {
      self$gate_id = gate_id
      self$gate_name = gate_name
      self$pdr = pdr
      self$status = status
      self$summary = summary
      self$metrics = metrics
      self$artifacts = artifacts
      self$messages = messages
    },

    #' Print a short summary of the gate result.
    #'
    #' @param ... Additional arguments, ignored.
    #' @return Invisibly returns `self`.
    print = function(...) {
      cat(sprintf("<GateResult %s: %s>\n", self$gate_id, self$status))
      cat(sprintf("  %s\n", self$gate_name))
      if (!is.null(self$summary)) cat(sprintf("  Summary: %s\n", self$summary))
      invisible(self)
    }
  )
)
