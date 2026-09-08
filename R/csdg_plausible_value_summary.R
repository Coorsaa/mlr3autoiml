#' Summarize estimates across plausible values on their value scale
#'
#' Aggregates a scalar estimate separately within each requested group before assigning any ranks.
#' The resulting spread is descriptive between-plausible-value variation and is not automatically a sampling
#' variance or a Rubin-pooled uncertainty estimate.
#'
#' @param data A data frame containing one estimate per plausible value and grouping key.
#' @param plausible_value Name of the plausible-value identifier column.
#' @param estimate Name of the numeric estimate column.
#' @param by Optional character vector of grouping columns.
#' @param direction Whether larger or smaller pooled estimates receive the leading rank.
#'
#' @return A list containing validated input values, value-scale summaries, and estimand metadata.
#' @export
csdg_plausible_value_summary = function(
    data,
    plausible_value = "plausible_value",
    estimate = "estimate",
    by = NULL,
    direction = c("decreasing", "increasing")) {
  assert_data_frame(data, min.rows = 2L)
  assert_string(plausible_value, min.chars = 1L)
  assert_string(estimate, min.chars = 1L)
  assert_character(by, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
  direction = match.arg(direction)
  plausible_value_column = plausible_value
  estimate_column = estimate
  columns = c(plausible_value, estimate, by)
  missing = setdiff(columns, names(data))
  if (length(missing)) {
    .csdg_stop("`data` is missing: %s.", paste(missing, collapse = ", "))
  }
  values = .as_dt(data)[, .SD, .SDcols = columns]
  if (anyDuplicated(values, by = c(by, plausible_value))) {
    .csdg_stop("Each grouping key must contain exactly one row per plausible value.")
  }
  assert_numeric(values[[estimate]], any.missing = FALSE, finite = TRUE, .var.name = estimate)
  if (anyNA(values[[plausible_value]])) {
    .csdg_stop("`%s` must not contain missing values.", plausible_value)
  }
  expected_pv = sort(unique(as.character(values[[plausible_value]])))
  if (length(expected_pv) < 2L) {
    .csdg_stop("At least two plausible values are required.")
  }
  coverage = if (length(by)) {
    values[, .(
      plausible_values = list(sort(unique(as.character(.SD[[plausible_value_column]]))))
    ), by = by, .SDcols = plausible_value_column]
  } else {
    data.table(plausible_values = list(expected_pv))
  }
  if (any(!vapply(coverage$plausible_values, identical, logical(1L), expected_pv))) {
    .csdg_stop("Every grouping key must contain the same complete plausible-value set.")
  }
  summary = values[, .(
    n_plausible_values = .N,
    mean_estimate = mean(.SD[[estimate_column]]),
    standard_deviation_across_plausible_values = sd(.SD[[estimate_column]]),
    minimum_estimate = min(.SD[[estimate_column]]),
    median_estimate = median(.SD[[estimate_column]]),
    maximum_estimate = max(.SD[[estimate_column]])
  ), by = by, .SDcols = estimate_column]
  if (length(by)) {
    summary[, rank_after_value_scale_aggregation := frank(
      if (identical(direction, "decreasing")) -mean_estimate else mean_estimate,
      ties.method = "average"
    )]
    setorderv(summary, c("rank_after_value_scale_aggregation", by))
  }
  list(
    values = values[],
    summary = summary[],
    estimand = list(
      aggregation_order = "Aggregate estimates on their value scale before ranking.",
      direction = direction,
      plausible_values = expected_pv,
      uncertainty = paste(
        "The standard deviation and range describe variation across supplied plausible values.",
        "They are not, by themselves, a sampling interval or a Rubin-pooled variance."
      )
    )
  )
}
