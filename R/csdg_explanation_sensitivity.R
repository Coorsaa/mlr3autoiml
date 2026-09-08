#' Quantify explanation sensitivity across near-equivalent models
#'
#' Compares the performance and complete feature-group ranking of each accepted model with an explicitly selected
#' focal model.
#' The calculation is descriptive: it does not construct confidence intervals or perform hypothesis tests.
#'
#' `importance_ranks` is a stable tabular interface so that ranks from any compatible importance method can be used.
#' The `ranks` table returned by [csdg_rashomon_agreement()] can be passed directly.
#'
#' @param rashomon A `CSDGRashomon` object returned by [csdg_rashomon()],
#'   or its exported candidates table with constant `primary_measure`, `direction`, `best_score`,
#'   `acceptance_reference_score`, and `acceptance_limit` columns.
#' @param importance_ranks A data frame with one row per learner and feature group and columns `learner_name`,
#'   `feature_group`, and `rank`.
#'   Within each learner, `rank` must be a complete average-rank ordering; fractional ranks therefore represent ties.
#' @param focal_learner Name of the accepted learner used as the comparison reference.
#' @param top_k Number of top-ranked feature groups used for Jaccard similarity.
#'   Ties at the cutoff are resolved by feature-group name to keep the selected set deterministic.
#' @return A data table with one row per accepted learner.
#'   Raw performance difference is the candidate score minus the focal score.
#'   Direction-adjusted performance difference is positive when the candidate performs worse than the focal model.
#'   Two tolerance fractions distinguish distance from the best score from deterioration relative to the focal score.
#'   Kendall's tau-b, strict reversals among all feature-group pairs, and top-k Jaccard similarity compare each
#'   candidate ranking with the focal ranking.
#' @examples
#' candidates = data.frame(
#'   learner_name = c("focal", "alternative"),
#'   accepted = c(TRUE, TRUE),
#'   mean_score = c(1.00, 1.05),
#'   primary_measure = "regr.rmse",
#'   direction = "minimize",
#'   best_score = 1.00,
#'   acceptance_limit = 1.10,
#'   distance_from_best = c(0, 0.05)
#' )
#' ranks = data.frame(
#'   learner_name = rep(c("focal", "alternative"), each = 3L),
#'   feature_group = rep(c("a", "b", "c"), 2L),
#'   rank = c(1, 2, 3, 1, 3, 2)
#' )
#' csdg_explanation_sensitivity(candidates, ranks, focal_learner = "focal", top_k = 2L)
#' @export
csdg_explanation_sensitivity = function(
    rashomon,
    importance_ranks,
    focal_learner,
    top_k = 10L) {
  assert_multi_class(rashomon, c("CSDGRashomon", "data.frame"))
  assert_data_frame(importance_ranks, min.rows = 1L)
  assert_string(focal_learner, min.chars = 1L)
  assert_int(top_k, lower = 1L)

  if (inherits(rashomon, "CSDGRashomon")) {
    if (!is.data.frame(rashomon$candidates)) {
      .csdg_stop("`rashomon$candidates` must be a data frame.")
    }
    candidates = as.data.table(rashomon$candidates)
    primary_measure = rashomon$primary_measure
    direction = rashomon$direction
    best_score = rashomon$best_score
    acceptance_reference_score = rashomon$acceptance_reference_score %||% rashomon$best_score
    acceptance_reference_learner = rashomon$acceptance_reference_learner %||% NA_character_
    acceptance_limit = rashomon$acceptance_limit
  } else {
    required_export_columns = c(
      "primary_measure", "direction", "best_score", "acceptance_limit"
    )
    missing_export_columns = setdiff(required_export_columns, names(rashomon))
    if (length(missing_export_columns)) {
      .csdg_stop(
        "An exported `rashomon` table is missing required columns: %s.",
        paste(missing_export_columns, collapse = ", ")
      )
    }
    candidates = as.data.table(rashomon)
    metadata = lapply(required_export_columns, function(column) unique(candidates[[column]]))
    names(metadata) = required_export_columns
    inconsistent_metadata = names(metadata)[vapply(metadata, length, integer(1L)) != 1L]
    if (length(inconsistent_metadata)) {
      .csdg_stop(
        "Exported `rashomon` metadata must be constant; inconsistent columns: %s.",
        paste(inconsistent_metadata, collapse = ", ")
      )
    }
    primary_measure = metadata$primary_measure[[1L]]
    direction = metadata$direction[[1L]]
    best_score = metadata$best_score[[1L]]
    acceptance_limit = metadata$acceptance_limit[[1L]]
    acceptance_reference_score = if ("acceptance_reference_score" %in% names(candidates)) {
      unique(candidates$acceptance_reference_score)[[1L]]
    } else {
      best_score
    }
    acceptance_reference_learner = if ("acceptance_reference_learner" %in% names(candidates)) {
      unique(candidates$acceptance_reference_learner)[[1L]]
    } else {
      NA_character_
    }
  }
  assert_choice(direction, c("minimize", "maximize"), .var.name = "rashomon$direction")
  assert_number(best_score, finite = TRUE, .var.name = "rashomon$best_score")
  assert_number(
    acceptance_reference_score,
    finite = TRUE,
    .var.name = "rashomon$acceptance_reference_score"
  )
  assert_number(
    acceptance_limit,
    finite = TRUE,
    .var.name = "rashomon$acceptance_limit"
  )
  assert_string(primary_measure, min.chars = 1L, .var.name = "rashomon$primary_measure")
  required_candidate_columns = c("learner_name", "accepted", "mean_score")
  missing_candidate_columns = setdiff(required_candidate_columns, names(candidates))
  if (length(missing_candidate_columns)) {
    .csdg_stop(
      "`rashomon` candidates are missing required columns: %s.",
      paste(missing_candidate_columns, collapse = ", ")
    )
  }
  assert_character(
    candidates$learner_name,
    any.missing = FALSE,
    min.len = 1L,
    min.chars = 1L
  )
  assert_logical(candidates$accepted, any.missing = FALSE, min.len = 1L)
  assert_numeric(candidates$mean_score, any.missing = FALSE, finite = TRUE)
  if (anyDuplicated(candidates$learner_name)) {
    .csdg_stop("`rashomon$candidates$learner_name` must be unique.")
  }
  score_tolerance = 1e-10 * max(1, abs(candidates$mean_score), abs(best_score), abs(acceptance_limit))
  expected_best = if (identical(direction, "minimize")) {
    min(candidates$mean_score)
  } else {
    max(candidates$mean_score)
  }
  limit_has_correct_direction = if (identical(direction, "minimize")) {
    acceptance_limit >= acceptance_reference_score - score_tolerance
  } else {
    acceptance_limit <= acceptance_reference_score + score_tolerance
  }
  acceptance_mismatch = if (identical(direction, "minimize")) {
    (candidates$accepted & candidates$mean_score > acceptance_limit + score_tolerance) |
      (!candidates$accepted & candidates$mean_score < acceptance_limit - score_tolerance)
  } else {
    (candidates$accepted & candidates$mean_score < acceptance_limit - score_tolerance) |
      (!candidates$accepted & candidates$mean_score > acceptance_limit + score_tolerance)
  }
  if (abs(best_score - expected_best) > score_tolerance) {
    .csdg_stop("`rashomon$best_score` is not the direction-specific best candidate score.")
  }
  if (!limit_has_correct_direction) {
    .csdg_stop("`rashomon$acceptance_limit` lies on the wrong side of the acceptance reference score.")
  }
  if (any(acceptance_mismatch)) {
    .csdg_stop("`rashomon$candidates$accepted` is inconsistent with the prespecified acceptance limit.")
  }
  calculated_candidate_distance = if (identical(direction, "minimize")) {
    candidates$mean_score - best_score
  } else {
    best_score - candidates$mean_score
  }
  if ("distance_from_best" %in% names(candidates)) {
    assert_numeric(candidates$distance_from_best, any.missing = FALSE, finite = TRUE)
    if (any(candidates$distance_from_best < -score_tolerance) ||
        any(abs(candidates$distance_from_best - calculated_candidate_distance) > score_tolerance)) {
      .csdg_stop("`rashomon$candidates$distance_from_best` is inconsistent with the candidate scores.")
    }
  }
  accepted_candidates = candidates[candidates$accepted]
  if (!nrow(accepted_candidates)) {
    .csdg_stop("`rashomon` contains no accepted learners.")
  }
  if (!focal_learner %in% accepted_candidates$learner_name) {
    .csdg_stop("`focal_learner` must name an accepted learner in `rashomon`.")
  }

  required_rank_columns = c("learner_name", "feature_group", "rank")
  missing_rank_columns = setdiff(required_rank_columns, names(importance_ranks))
  if (length(missing_rank_columns)) {
    .csdg_stop(
      "`importance_ranks` is missing required columns: %s.",
      paste(missing_rank_columns, collapse = ", ")
    )
  }
  ranks = as.data.table(importance_ranks)
  assert_character(ranks$learner_name, any.missing = FALSE, min.len = 1L, min.chars = 1L)
  ranks = ranks[ranks$learner_name %in% accepted_candidates$learner_name]
  if (!nrow(ranks)) {
    .csdg_stop("`importance_ranks` contains no rows for accepted learners.")
  }
  assert_character(ranks$feature_group, any.missing = FALSE, min.len = 1L, min.chars = 1L)
  assert_numeric(ranks$rank, lower = 1, any.missing = FALSE, finite = TRUE)
  rank_keys = ranks[, c("learner_name", "feature_group"), with = FALSE]
  if (anyDuplicated(rank_keys)) {
    .csdg_stop("`importance_ranks` must contain one row per learner and feature group.")
  }

  missing_rank_learners = setdiff(accepted_candidates$learner_name, unique(ranks$learner_name))
  if (length(missing_rank_learners)) {
    .csdg_stop(
      "Missing importance ranks for accepted learners: %s.",
      paste(missing_rank_learners, collapse = ", ")
    )
  }

  focal_ranks = ranks[ranks$learner_name == focal_learner]
  focal_features = sort(focal_ranks$feature_group)
  n_features = length(focal_features)
  if (!n_features) {
    .csdg_stop("`importance_ranks` contains no feature groups for the focal learner.")
  }
  focal_rank = focal_ranks$rank[match(focal_features, focal_ranks$feature_group)]

  inconsistent_learners = accepted_candidates$learner_name[
    !vapply(accepted_candidates$learner_name, function(current_learner) {
      learner_features = ranks$feature_group[ranks$learner_name == current_learner]
      identical(sort(learner_features), focal_features)
    }, logical(1))
  ]
  if (length(inconsistent_learners)) {
    .csdg_stop(
      "Every accepted learner must rank the same feature groups as the focal learner; inconsistent learners: %s.",
      paste(inconsistent_learners, collapse = ", ")
    )
  }
  rank_validity = ranks[, .(
    valid_bounds = all(rank <= .N),
    valid_average_rank = isTRUE(all.equal(
      as.numeric(base::rank(rank, ties.method = "average")),
      as.numeric(rank),
      tolerance = 1e-12
    ))
  ), by = learner_name]
  invalid_rank_learners = rank_validity[!valid_bounds | !valid_average_rank, learner_name]
  if (length(invalid_rank_learners)) {
    .csdg_stop(
      paste(
        "Each learner's ranks must be a complete average-rank ordering bounded by its number of feature groups;",
        "invalid learners: %s."
      ),
      paste(invalid_rank_learners, collapse = ", ")
    )
  }

  prespecified_tolerance = abs(acceptance_limit - acceptance_reference_score)
  focal_score = accepted_candidates$mean_score[
    match(focal_learner, accepted_candidates$learner_name)
  ]
  effective_top_k = min(top_k, n_features)
  focal_top = head(
    focal_features[order(focal_rank, focal_features)],
    effective_top_k
  )

  result_rows = lapply(seq_len(nrow(accepted_candidates)), function(index) {
    current_learner = accepted_candidates$learner_name[[index]]
    candidate_score = accepted_candidates$mean_score[[index]]
    candidate_ranks = ranks[ranks$learner_name == current_learner]
    candidate_rank = candidate_ranks$rank[match(focal_features, candidate_ranks$feature_group)]

    raw_difference = candidate_score - focal_score
    direction_adjusted_difference = if (identical(direction, "minimize")) {
      raw_difference
    } else {
      -raw_difference
    }
    calculated_distance_from_best = if (identical(direction, "minimize")) {
      candidate_score - best_score
    } else {
      best_score - candidate_score
    }
    distance_from_best = if ("distance_from_best" %in% names(accepted_candidates)) {
      accepted_candidates$distance_from_best[[index]]
    } else {
      calculated_distance_from_best
    }
    if (abs(distance_from_best) <= score_tolerance) {
      distance_from_best = 0
    }
    tolerance_fraction_from_best = if (prespecified_tolerance > 0) {
      min(1, distance_from_best / prespecified_tolerance)
    } else if (abs(distance_from_best) <= score_tolerance) {
      0
    } else {
      NA_real_
    }
    tolerance_fraction_from_focal = if (prespecified_tolerance > 0) {
      min(1, max(0, direction_adjusted_difference) / prespecified_tolerance)
    } else if (direction_adjusted_difference <= score_tolerance) {
      0
    } else {
      NA_real_
    }

    has_kendall_variation = n_features >= 2L &&
      length(unique(focal_rank)) >= 2L &&
      length(unique(candidate_rank)) >= 2L
    kendall_tau = if (has_kendall_variation) {
      cor(focal_rank, candidate_rank, method = "kendall")
    } else {
      NA_real_
    }

    if (n_features >= 2L) {
      feature_pairs = combn(seq_len(n_features), 2L)
      focal_pair_differences = focal_rank[feature_pairs[1L, ]] - focal_rank[feature_pairs[2L, ]]
      candidate_pair_differences = candidate_rank[feature_pairs[1L, ]] - candidate_rank[feature_pairs[2L, ]]
      reversed_pairs = focal_pair_differences * candidate_pair_differences < 0
      tied_pairs = focal_pair_differences == 0 | candidate_pair_differences == 0
      n_pairs = ncol(feature_pairs)
      n_reversed_pairs = sum(reversed_pairs)
      n_tied_pairs = sum(tied_pairs)
      reversal_rate = n_reversed_pairs / n_pairs
    } else {
      n_pairs = 0L
      n_reversed_pairs = 0L
      n_tied_pairs = 0L
      reversal_rate = NA_real_
    }

    candidate_top = head(
      focal_features[order(candidate_rank, focal_features)],
      effective_top_k
    )
    top_intersection = length(intersect(focal_top, candidate_top))
    top_union = length(union(focal_top, candidate_top))

    data.table(
      learner_name = current_learner,
      focal_learner = focal_learner,
      primary_measure = primary_measure,
      direction = direction,
      mean_score = candidate_score,
      focal_mean_score = focal_score,
      acceptance_reference_learner = acceptance_reference_learner,
      acceptance_reference_score = acceptance_reference_score,
      raw_performance_difference_from_focal = raw_difference,
      direction_adjusted_performance_difference_from_focal = direction_adjusted_difference,
      distance_from_best = distance_from_best,
      prespecified_tolerance = prespecified_tolerance,
      fraction_of_prespecified_tolerance = tolerance_fraction_from_best,
      fraction_of_prespecified_tolerance_from_focal = tolerance_fraction_from_focal,
      n_ranked_feature_groups = n_features,
      kendall_tau_vs_focal = kendall_tau,
      n_feature_group_pairs = n_pairs,
      n_reversed_feature_group_pairs = n_reversed_pairs,
      n_tied_feature_group_pairs = n_tied_pairs,
      full_pair_reversal_rate_vs_focal = reversal_rate,
      top_k = effective_top_k,
      top_k_intersection = top_intersection,
      top_k_union = top_union,
      top_k_jaccard_vs_focal = top_intersection / top_union,
      rank_tie_semantics = paste(
        "Kendall tau-b and strict pair reversals retain ties;",
        "top-k cutoff ties are resolved by feature-group name."
      ),
      uncertainty_scope = paste(
        "Descriptive sensitivity across prespecified near-equivalent models;",
        "no confidence interval or hypothesis test."
      )
    )
  })

  rbindlist(result_rows)
}
