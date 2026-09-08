.importance_agreement_table = function(x, name) {
  assert_data_frame(x, min.rows = 2L, .var.name = name)
  data = as.data.table(x)
  required = c("feature_group", "importance")
  missing = setdiff(required, names(data))
  if (length(missing)) {
    .csdg_stop("`%s` is missing: %s.", name, paste(missing, collapse = ", "))
  }
  assert_character(data$feature_group, any.missing = FALSE, unique = TRUE)
  assert_numeric(data$importance, any.missing = FALSE, finite = TRUE)
  data[, .(feature_group, importance = as.numeric(importance))]
}

.importance_pair_table = function(values, tolerance) {
  pairs = utils::combn(seq_len(nrow(values)), 2L)
  first = pairs[1L, ]
  second = pairs[2L, ]
  reference_difference = values$reference_importance[first] - values$reference_importance[second]
  comparison_difference = values$comparison_importance[first] - values$comparison_importance[second]
  data.table(
    feature_1 = values$feature_group[first],
    feature_2 = values$feature_group[second],
    reference_difference = reference_difference,
    comparison_difference = comparison_difference,
    reference_tie = abs(reference_difference) <= tolerance,
    comparison_tie = abs(comparison_difference) <= tolerance,
    exact_reversal = reference_difference * comparison_difference < 0
  )[, practical_reversal := exact_reversal & !reference_tie & !comparison_tie]
}

#' Compare permutation-importance explanations without treating every exact rank as equally meaningful
#'
#' Aligns two complete importance vectors, reports exact pair-order reversals, practical-tie sensitivity,
#' top-k overlap, direct importance differences, and an importance-weighted reversal fraction.
#' Groups listed in `exclude_groups` are retained in the direct-difference output but excluded from rank comparisons;
#' this is useful when a joint perturbation overlaps its component-variable perturbations.
#'
#' @param reference Data frame with unique `feature_group` and numeric `importance` columns for the reference model.
#' @param comparison Data frame with the same columns for the comparison model.
#' @param top_k Positive integer used for set overlap.
#' @param practical_tolerances Nonnegative absolute importance differences treated as practical ties.
#' @param exclude_groups Optional feature groups excluded from rank-based summaries.
#'
#' @return A list containing aligned direct differences, pair-level comparisons, tolerance-sensitivity summaries,
#'   top-k overlap, and explicit metric definitions.
#' @export
csdg_importance_agreement = function(
    reference,
    comparison,
    top_k = 5L,
    practical_tolerances = c(0, 0.01, 0.05),
    exclude_groups = NULL) {
  assert_int(top_k, lower = 1L)
  assert_numeric(
    practical_tolerances,
    lower = 0,
    any.missing = FALSE,
    finite = TRUE,
    min.len = 1L,
    unique = TRUE
  )
  assert_character(exclude_groups, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
  reference = .importance_agreement_table(reference, "reference")
  comparison = .importance_agreement_table(comparison, "comparison")
  if (!setequal(reference$feature_group, comparison$feature_group)) {
    .csdg_stop("`reference` and `comparison` must contain the same feature groups.")
  }
  values = merge(
    reference,
    comparison,
    by = "feature_group",
    suffixes = c("_reference", "_comparison"),
    sort = FALSE
  )
  setnames(
    values,
    c("importance_reference", "importance_comparison"),
    c("reference_importance", "comparison_importance")
  )
  values[, `:=`(
    difference = comparison_importance - reference_importance,
    absolute_difference = abs(comparison_importance - reference_importance),
    excluded_from_rank_comparison = feature_group %in% (exclude_groups %||% character())
  )]
  ranked = values[excluded_from_rank_comparison == FALSE]
  if (nrow(ranked) < 2L) {
    .csdg_stop("At least two nonexcluded feature groups are required.")
  }
  ranked[, `:=`(
    reference_rank = frank(-reference_importance, ties.method = "average"),
    comparison_rank = frank(-comparison_importance, ties.method = "average")
  )]
  effective_top_k = min(top_k, nrow(ranked))
  reference_top = ranked[reference_rank <= effective_top_k, feature_group]
  comparison_top = ranked[comparison_rank <= effective_top_k, feature_group]
  top_union = union(reference_top, comparison_top)
  top_k_summary = data.table(
    requested_top_k = top_k,
    effective_top_k = effective_top_k,
    intersection = length(intersect(reference_top, comparison_top)),
    union = length(top_union),
    jaccard = length(intersect(reference_top, comparison_top)) / length(top_union)
  )

  tolerance_sensitivity = rbindlist(lapply(sort(practical_tolerances), function(tolerance) {
    pairs = .importance_pair_table(ranked, tolerance)
    weights = abs(pairs$reference_difference) + abs(pairs$comparison_difference)
    comparable = !pairs$reference_tie & !pairs$comparison_tie
    weighted_denominator = sum(weights[comparable])
    data.table(
      practical_tolerance = tolerance,
      n_pairs = nrow(pairs),
      n_comparable_pairs = sum(comparable),
      n_exact_reversals = sum(pairs$exact_reversal),
      exact_reversal_fraction = mean(pairs$exact_reversal),
      n_practical_reversals = sum(pairs$practical_reversal),
      practical_reversal_fraction = if (sum(comparable)) {
        sum(pairs$practical_reversal) / sum(comparable)
      } else {
        NA_real_
      },
      importance_weighted_reversal_fraction = if (weighted_denominator > 0) {
        sum(weights[pairs$practical_reversal]) / weighted_denominator
      } else {
        NA_real_
      },
      kendall_tau_b = suppressWarnings(cor(
        ranked$reference_importance,
        ranked$comparison_importance,
        method = "kendall"
      ))
    )
  }))
  pairwise = .importance_pair_table(ranked, min(practical_tolerances))
  list(
    direct_differences = values[],
    pairwise = pairwise[],
    tolerance_sensitivity = tolerance_sensitivity[],
    top_k = top_k_summary[],
    definitions = data.table(
      metric = c(
        "exact_reversal_fraction",
        "practical_reversal_fraction",
        "importance_weighted_reversal_fraction",
        "top_k_jaccard"
      ),
      definition = c(
        "Share of all unordered feature pairs whose strict importance order changes sign",
        "Share of nontied feature pairs whose importance order changes sign at the declared absolute tolerance",
        "Reversal-weight share using the sum of absolute pairwise importance separations",
        "Intersection divided by union of the two top-k feature sets"
      )
    ),
    limitations = c(
      "Importance agreement is conditional on the loss, assessment rows, perturbation design, and feature grouping.",
      "Practical-tie tolerances require a substantive or decision-linked justification.",
      "Overlapping joint and component perturbations should not be treated as additive or mutually exclusive ranks."
    )
  )
}
