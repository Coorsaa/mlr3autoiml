make_explanation_sensitivity_rashomon = function(direction = "minimize") {
  if (identical(direction, "minimize")) {
    scores = c(100, 102, 104, 108)
    best_score = 100
    acceptance_limit = 105
  } else {
    scores = c(.90, .88, .86, .80)
    best_score = .90
    acceptance_limit = .85
  }
  structure(
    list(
      candidates = data.table::data.table(
        learner_name = c("focal", "one_reversal", "reverse", "rejected"),
        accepted = c(TRUE, TRUE, TRUE, FALSE),
        mean_score = scores
      ),
      primary_measure = if (identical(direction, "minimize")) "regr.rmse" else "classif.auc",
      direction = direction,
      best_score = best_score,
      acceptance_limit = acceptance_limit
    ),
    class = c("CSDGRashomon", "list")
  )
}

make_explanation_sensitivity_ranks = function() {
  data.table::data.table(
    learner_name = rep(c("focal", "one_reversal", "reverse"), each = 4L),
    feature_group = rep(c("a", "b", "c", "d"), 3L),
    rank = c(
      1, 2, 3, 4,
      1, 3, 2, 4,
      4, 3, 2, 1
    )
  )
}

test_that("explanation sensitivity separates performance and ranking metrics", {
  out = csdg_explanation_sensitivity(
    make_explanation_sensitivity_rashomon(),
    make_explanation_sensitivity_ranks(),
    focal_learner = "focal",
    top_k = 2L
  )

  expect_s3_class(out, "data.table")
  expect_equal(out$raw_performance_difference_from_focal, c(0, 2, 4))
  expect_equal(out$direction_adjusted_performance_difference_from_focal, c(0, 2, 4))
  expect_equal(out$fraction_of_prespecified_tolerance, c(0, .4, .8))
  expect_equal(out$kendall_tau_vs_focal, c(1, 2 / 3, -1))
  expect_equal(out$n_feature_group_pairs, rep(6L, 3L))
  expect_equal(out$n_reversed_feature_group_pairs, c(0L, 1L, 6L))
  expect_equal(out$full_pair_reversal_rate_vs_focal, c(0, 1 / 6, 1))
  expect_equal(out$top_k_intersection, c(2L, 1L, 0L))
  expect_equal(out$top_k_union, c(2L, 3L, 4L))
  expect_equal(out$top_k_jaccard_vs_focal, c(1, 1 / 3, 0))
  expect_match(out$uncertainty_scope, "no confidence interval", fixed = TRUE)
})

test_that("raw and direction-adjusted differences remain distinct for maximized measures", {
  out = csdg_explanation_sensitivity(
    make_explanation_sensitivity_rashomon("maximize"),
    make_explanation_sensitivity_ranks(),
    focal_learner = "focal",
    top_k = 10L
  )

  expect_equal(out$raw_performance_difference_from_focal, c(0, -.02, -.04))
  expect_equal(out$direction_adjusted_performance_difference_from_focal, c(0, .02, .04))
  expect_equal(out$fraction_of_prespecified_tolerance, c(0, .4, .8))
  expect_equal(out$top_k, rep(4L, 3L))
  expect_equal(out$top_k_jaccard_vs_focal, rep(1, 3L))
})

test_that("full-pair reversal rate keeps ties in the declared denominator", {
  rashomon = make_explanation_sensitivity_rashomon()
  rashomon$candidates = rashomon$candidates[learner_name %in% c("focal", "one_reversal")]
  ranks = data.table::data.table(
    learner_name = rep(c("focal", "one_reversal"), each = 3L),
    feature_group = rep(c("a", "b", "c"), 2L),
    rank = c(1, 2.5, 2.5, 2.5, 1, 2.5)
  )

  out = csdg_explanation_sensitivity(rashomon, ranks, focal_learner = "focal", top_k = 2L)
  comparison = out[learner_name == "one_reversal"]
  expect_equal(comparison$n_feature_group_pairs, 3L)
  expect_equal(comparison$n_reversed_feature_group_pairs, 1L)
  expect_equal(comparison$n_tied_feature_group_pairs, 2L)
  expect_equal(comparison$full_pair_reversal_rate_vs_focal, 1 / 3)
})

test_that("explanation sensitivity validates rank coverage and focal membership", {
  rashomon = make_explanation_sensitivity_rashomon()
  ranks = make_explanation_sensitivity_ranks()

  expect_error(
    csdg_explanation_sensitivity(rashomon, ranks, focal_learner = "rejected"),
    "accepted learner"
  )
  expect_error(
    csdg_explanation_sensitivity(
      rashomon,
      ranks[learner_name != "reverse"],
      focal_learner = "focal"
    ),
    "Missing importance ranks"
  )
  expect_error(
    csdg_explanation_sensitivity(
      rashomon,
      ranks[!(learner_name == "reverse" & feature_group == "d")],
      focal_learner = "focal"
    ),
    "same feature groups"
  )
  expect_error(
    csdg_explanation_sensitivity(rashomon, rbind(ranks, ranks[1L]), focal_learner = "focal"),
    "one row per learner"
  )
  invalid_ranks = data.table::copy(ranks)
  invalid_ranks[learner_name == "reverse", rank := 100]
  expect_error(
    csdg_explanation_sensitivity(rashomon, invalid_ranks, focal_learner = "focal"),
    "complete average-rank ordering"
  )
})

test_that("zero-width Rashomon tolerances produce a defined zero fraction", {
  rashomon = make_explanation_sensitivity_rashomon()
  rashomon$candidates = rashomon$candidates[learner_name %in% c("focal", "one_reversal")]
  rashomon$candidates[, mean_score := 100]
  rashomon$acceptance_limit = 100

  out = csdg_explanation_sensitivity(
    rashomon,
    make_explanation_sensitivity_ranks(),
    focal_learner = "focal"
  )
  expect_equal(out$fraction_of_prespecified_tolerance, c(0, 0))
})

test_that("exported candidate tables reproduce object inputs", {
  rashomon = make_explanation_sensitivity_rashomon()
  exported = data.table::copy(rashomon$candidates)
  exported[, `:=`(
    primary_measure = rashomon$primary_measure,
    direction = rashomon$direction,
    best_score = rashomon$best_score,
    acceptance_limit = rashomon$acceptance_limit,
    distance_from_best = mean_score - rashomon$best_score
  )]

  from_object = csdg_explanation_sensitivity(
    rashomon,
    make_explanation_sensitivity_ranks(),
    focal_learner = "one_reversal",
    top_k = 2L
  )
  from_export = csdg_explanation_sensitivity(
    exported,
    make_explanation_sensitivity_ranks(),
    focal_learner = "one_reversal",
    top_k = 2L
  )

  expect_equal(from_export, from_object)
  expect_equal(from_export$fraction_of_prespecified_tolerance, c(0, .4, .8))
  expect_equal(from_export$fraction_of_prespecified_tolerance_from_focal, c(0, 0, .4))
})

test_that("explanation sensitivity rejects inconsistent Rashomon metadata", {
  ranks = make_explanation_sensitivity_ranks()

  bad_best = make_explanation_sensitivity_rashomon()
  bad_best$best_score = 99
  expect_error(csdg_explanation_sensitivity(bad_best, ranks, "focal"), "best candidate score")

  bad_limit = make_explanation_sensitivity_rashomon()
  bad_limit$acceptance_limit = 99
  expect_error(csdg_explanation_sensitivity(bad_limit, ranks, "focal"), "wrong side")

  bad_acceptance = make_explanation_sensitivity_rashomon()
  bad_acceptance$candidates[learner_name == "reverse", accepted := FALSE]
  expect_error(csdg_explanation_sensitivity(bad_acceptance, ranks, "focal"), "accepted.*inconsistent")

  bad_distance = make_explanation_sensitivity_rashomon()
  bad_distance$candidates[, distance_from_best := mean_score - bad_distance$best_score]
  bad_distance$candidates[learner_name == "one_reversal", distance_from_best := 3]
  expect_error(csdg_explanation_sensitivity(bad_distance, ranks, "focal"), "distance_from_best")
})
