test_that("improvement, minimum importance, and fold criterion follow the article", {
  imp = make_importance_fixture()
  expect_equal(imp$improvement$improvement, 0.4)
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp)
  expect_equal(chk$criteria$tau[["m1"]], 0.004)
  expect_equal(chk$criteria$min_difference[["m1"]], 0.004)
  expect_identical(chk$criteria$need, 4L)
  expect_identical(csdg_check(claim_relies_mainly(imp), imp, stability = 0.75)$criteria$need, 3L)
  expect_identical(chk$criteria$sources$min_importance, "article convention")
  expect_identical(chk$criteria$sources$stability, "article convention")
  expect_identical(chk$criteria$sources$factor, "article convention")
  expect_identical(chk$criteria$sources$interval_rule, "article recommendation")
  expect_identical(chk$criteria$fixed, format(Sys.Date()))
  expect_identical(csdg_check(claim_relies_mainly(imp), imp, stability = 0.75)$criteria$sources$stability,
    "set by the analyst")
  expect_match(chk$records[[which(chk$table$check == "minimum_importance")]]$criterion_source,
    "minimum importance 1%: article convention")
})

test_that("relies_mainly content: averages, ratios, margins, and corrected interval", {
  imp = make_importance_fixture()
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp)
  content = chk$details$content$m1
  expect_identical(content$largest_other, "c")
  expect_equal(unname(content$averages), c(0.30, 0.20, 0.05))
  expect_equal(unname(content$ratios), c(6, 4))
  m = content$margins
  expect_equal(m$estimate, c(0.20, 0.10))
  expect_equal(m$se[[1L]], 0)
  expect_equal(m$se[[2L]], sqrt((1 / 4 + 1 / 3) * 0.0008 / 3), tolerance = 1e-10)
  expect_equal(m$se[[2L]], 0.012472, tolerance = 1e-4)
  expect_equal(m$lower[[2L]], 0.10 - stats::qt(0.975, 3) * m$se[[2L]])
  expect_equal(c(m$lower[[2L]], m$upper[[2L]]), c(0.0603, 0.1397), tolerance = 1e-3)
  expect_equal(m$mc_threshold[[1L]], stats::qt(0.975, 2) * sqrt(4 * (0.01^2 / 3 + 4 * 0.01^2 / 3)) / 4)
  expect_true(all(m$beyond_mc))
  expect_identical(chk$table[check == "content", status], "supported")
  expect_pass_through(chk)
})

# Ten folds with n_test / n_train = 1/9 and permutation values within 1e-5 of the fold values in `centers`.
ten_fold_fixture = function(centers) {
  fits = make_importance_fixture()$fits
  fits$K = 10L
  fits$n = 1000L
  fits$n_train = rep(900L, 10L)
  fits$n_test = rep(100L, 10L)
  fits$resampling_label = "10-fold cross-validation"
  spread = matrix(1e-5, 10L, ncol(centers), dimnames = dimnames(centers))
  losses = data.table::data.table(learner = "m1", iteration = 1:10, loss = 0.6, loss_baseline = 1)
  .csdg_new_importance(fits, loss = "mse", repetitions = 3L, batch_size = 1L, seed = 2L, groups = NULL,
    strata_features = NULL, strata_definition = NULL, pfi = list(m1 = fixture_pfi(centers, spread)),
    conditional = NULL, losses = losses)
}

test_that("a ranking claim needs every comparison with an unnamed predictor, not only the largest other", {
  # d has a smaller average than c, the largest other predictor, but varies across folds.
  centers = cbind(a = rep(1.1, 10L), b = rep(1, 10L), c = rep(0.445, 10L), d = c(rep(0.49, 9L), 0))
  imp = ten_fold_fixture(centers)
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2, marginal_only = TRUE), imp, judgments = g0b_judged())
  content = chk$details$content$m1
  expect_identical(content$largest_other, "c")
  expect_identical(content$comparisons[versus == "c", status], c("supported", "supported"))
  expect_identical(content$comparisons[versus == "d", status], c("open", "open"))
  bd = content$comparisons[predictor == "b" & versus == "d"]
  values = centers[, "b"] - 2 * centers[, "d"]
  expect_equal(bd$lower, mean(values) - stats::qt(0.975, 9) * sqrt((1 / 10 + 1 / 9) * stats::var(values)))
  expect_equal(c(bd$estimate, bd$lower, bd$upper), c(0.118, -0.204, 0.440), tolerance = 1e-3)
  expect_identical(content$margins$versus, c("d", "d"))
  expect_identical(chk$details$stability$m1$n_holds, 10L)
  row = chk$table[check == "content"]
  expect_identical(row$status, "open")
  expect_match(row$observation,
    "weakest of the 4 comparisons with other predictors \\(2 open\\): b minus 2 x d, 0.118 \\[-0.204, 0.440\\]")
  expect_match(row$observation, "The margin of b over d is inconclusive: the lower bound -0.204")
  res = csdg_assess(chk)
  expect_identical(res$assessment, "unresolved")
  expect_identical(res$decisive$check, "content")
  expect_match(res$report, "Content \\(G2\\) is open: .*the margin of b over d is inconclusive \\(lower bound -0.204")
  expect_pass_through(chk)
  new_imp = ten_fold_fixture(centers)
  new_imp$fits$data_hash = "new_data"
  expect_identical(csdg_confirm(chk, new_imp)$assessment, "unresolved")

  centers[, "c"] = 0.9
  centers[, "d"] = c(rep(0.95, 9L), 0)
  imp = ten_fold_fixture(centers)
  chk = csdg_check(claim_top_k(imp, k = 2, marginal_only = TRUE), imp, judgments = g0b_judged())
  content = chk$details$content$m1
  expect_identical(content$largest_other, "c")
  expect_identical(content$comparisons[versus == "c", status], c("supported", "supported"))
  expect_identical(content$comparisons[versus == "d", status], c("open", "open"))
  expect_identical(chk$table[check == "stability", status], "supported")
  expect_identical(csdg_assess(chk)$assessment, "unresolved")
})

test_that("a ranking claim is met if every comparison with an unnamed predictor is supported", {
  centers = cbind(a = rep(1.1, 10L), b = rep(1, 10L), c = rep(0.445, 10L), d = rep(c(0.36, 0.46), 5L))
  imp = ten_fold_fixture(centers)
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp, judgments = g0b_judged())
  content = chk$details$content$m1
  expect_identical(nrow(content$comparisons), 4L)
  expect_true(all(content$comparisons$status == "supported"))
  expect_identical(content$largest_other, "c")
  expect_identical(content$margins$versus, c("d", "d"))
  bd = content$comparisons[predictor == "b" & versus == "d"]
  values = centers[, "b"] - 2 * centers[, "d"]
  expect_equal(bd$lower, mean(values) - stats::qt(0.975, 9) * sqrt((1 / 10 + 1 / 9) * stats::var(values)))
  expect_match(chk$table[check == "content", observation],
    "weakest of the 4 comparisons with other predictors: b minus 2 x d, 0.180 \\[0.0704, 0.290\\]")
  expect_identical(chk$table[check == "content", status], "supported")
  expect_identical(csdg_assess(chk)$assessment, "met")
})

test_that("another learner as context names a weakest comparison that is not with the largest other predictor", {
  m2 = fixture_centers()
  m2[, "b"] = 0.20
  m2[, "d"] = c(0.09, 0, 0.09, 0)
  imp = make_importance_fixture(centers = list(m1 = fixture_centers(), m2 = m2))
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp, judgments = g0b_judged())
  row = chk$table[check == "models"]
  expect_identical(row$learner, "m2")
  expect_identical(chk$details$models$m2$content$status, "open")
  expect_identical(row$observation, paste("learner m2: the result is inconclusive (a 0.300 and b 0.200 versus 0.0500",
    "for c; weakest comparison: b minus 2 x d, 0.110 [-0.143, 0.363])."))
  expect_identical(csdg_assess(chk)$assessment, "met")
})

test_that("Monte Carlo rule of a fold difference reuses csdg_pfi_mc_difference()", {
  imp = make_importance_fixture()
  raw = .csdg_raw(imp, "m1", "a")
  raw_c = .csdg_raw(imp, "m1", "c")
  mc = csdg_pfi_mc_difference(raw[1L, ], 2 * raw_c[1L, ])
  expect_equal(mc$se, sqrt(0.01^2 / 3 + 4 * 0.01^2 / 3))
  expect_equal(mc$se, 0.012910, tolerance = 1e-5)
  expect_equal(mc$threshold, 4.302653 * 0.012910, tolerance = 1e-4)
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp)
  expect_equal(chk$details$stability$m1$folds$se[[1L]], mc$se)
})

test_that("minimum importance is open within and contradicted beyond Monte Carlo error", {
  centers = fixture_centers()
  centers[, "b"] = 0.002
  imp = make_importance_fixture(centers = list(m1 = centers))
  chk = suppressWarnings(csdg_check(claim_relies_mainly(imp, k = 2, predictors = c("a", "b")), imp))
  tab = chk$details$minimum_importance$m1$table
  expect_equal(tab[predictor == "b", mc_se], sqrt(4 * 0.01^2 / 3) / 4)
  expect_equal(tab[predictor == "b", mc_threshold], 0.01242, tolerance = 1e-3)
  expect_identical(chk$table[check == "minimum_importance", status], "open")
  spread = fixture_spread()
  spread[, "b"] = 0.0001
  imp = make_importance_fixture(centers = list(m1 = centers), spread = list(m1 = spread))
  chk = suppressWarnings(csdg_check(claim_relies_mainly(imp, k = 2, predictors = c("a", "b")), imp))
  expect_identical(chk$table[check == "minimum_importance", status], "contradicted")
  expect_pass_through(chk)
})

test_that("a learner without improvement leaves the minimum importance open", {
  imp = make_importance_fixture(loss = 1.1)
  claim = claim_relies_mainly(imp, k = 2)
  expect_warning(csdg_check(claim, imp), "does not improve")
  chk = suppressWarnings(csdg_check(claim, imp))
  expect_identical(chk$table[check == "minimum_importance", status], "open")
  expect_identical(csdg_assess(chk)$assessment, "unresolved")
})

test_that("order claims: fold counts, undecided folds, and Holm reversal", {
  imp = make_importance_fixture()
  chk = csdg_check(claim_order(imp, "a", "b"), imp)
  expect_identical(chk$details$stability$m1$n_holds, 4L)
  expect_identical(chk$table[check == "stability", status], "supported")

  imp = make_importance_fixture(centers = list(m1 = with_fold3(c(a = 0.20, b = 0.21))))
  chk = csdg_check(claim_order(imp, "a", "b"), imp, stability = 0.75)
  folds = chk$details$stability$m1$folds
  expect_equal(folds$d[[3L]], -0.01)
  expect_equal(folds$se[[3L]], 0.008165, tolerance = 1e-4)
  expect_equal(folds$threshold[[3L]], 4.302653 * 0.008165, tolerance = 1e-4)
  expect_identical(folds$class[[3L]], "undecided")
  expect_identical(chk$table[check == "stability", status], "supported")
  chk = csdg_check(claim_order(imp, "a", "b"), imp, stability = 1)
  expect_identical(chk$table[check == "stability", status], "open")

  spread = fixture_spread()
  spread[3L, c("a", "b")] = 0.001
  imp = make_importance_fixture(centers = list(m1 = with_fold3(c(a = 0.10, b = 0.30))), spread = list(m1 = spread))
  chk = suppressWarnings(csdg_check(claim_order(imp, "a", "b"), imp, stability = 1))
  folds = chk$details$stability$m1$folds
  expect_equal(folds$threshold[[3L]], stats::qt(1 - 0.05 / (2 * 4), 2) * sqrt(2 * 0.001^2 / 3))
  expect_equal(folds$threshold[[3L]], 8.860200 * 0.000816, tolerance = 1e-3)
  expect_identical(folds$class[[3L]], "fails")
  expect_identical(chk$table[check == "stability", status], "contradicted")
  expect_pass_through(chk)

  centers = fixture_centers()
  centers[, "a"] = c(0.90, 0.10, 0.10, 0.10)
  centers[, "b"] = c(0.10, 0.14, 0.13, 0.12)
  imp = make_importance_fixture(centers = list(m1 = centers), spread = list(m1 = fixture_spread(0.001)))
  folds = csdg_check(claim_order(imp, "a", "b"), imp, stability = 1)$details$stability$m1$folds
  expect_equal(folds$threshold[2:4], stats::qt(1 - 0.05 / (2 * 4:2), 2) * sqrt(2 * 0.001^2 / 3))
  expect_identical(folds$class, c("holds", "fails", "fails", "fails"))
})

test_that("a failing fold below the smallest relevant difference is undecided", {
  spread = fixture_spread()
  spread[3L, c("a", "b")] = 0.0001
  imp = make_importance_fixture(centers = list(m1 = with_fold3(c(a = 0.199, b = 0.2005))),
    spread = list(m1 = spread))
  chk = csdg_check(claim_order(imp, "a", "b"), imp)
  folds = chk$details$stability$m1$folds
  expect_true(folds$beyond[[3L]])
  expect_identical(folds$class[[3L]], "undecided")
})

test_that("selection stability reports neighboring cutoffs", {
  imp = make_importance_fixture()
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp)
  st = chk$details$stability$m1
  expect_identical(st$n_holds, 4L)
  expect_identical(st$neighbors$cutoff, c(1L, 3L))
  expect_identical(st$neighbors$folds, c(0L, 4L))
  chk = csdg_check(claim_top_k(imp, k = 2), imp)
  expect_identical(chk$details$stability$m1$n_holds, 4L)
  expect_equal(chk$details$content$m1$margins[predictor == "b", estimate], 0.15)
})

test_that("the corrected interval leaves a property open but never supports it", {
  imp = make_importance_fixture()
  claim = claim_order(imp, "b", "c")
  chk = csdg_check(claim, imp, min_difference = 0.2, judgments = g0b_judged())
  m = chk$details$content$m1$margins
  expect_equal(m$lower, 0.15 - stats::qt(0.975, 3) * sqrt(7 / 12 * stats::var(c(0.15, 0.13, 0.17, 0.15))))
  expect_identical(chk$table[check == "content", status], "open")
  expect_identical(csdg_assess(chk)$assessment, "unresolved")
  chk = csdg_check(claim, imp, min_difference = 0.2, interval_rule = "report_only", judgments = g0b_judged())
  expect_identical(chk$table[check == "content", status], "supported")
  expect_identical(csdg_assess(chk)$assessment, "met")
})

test_that("conditional PFI that reverses the result contradicts the procedure", {
  imp = make_importance_fixture(conditional = list(b = list(centers = rep(0.001, 4L), spread = 0.0001)))
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp)
  comparisons = chk$details$procedure$m1$comparisons
  expect_identical(comparisons[comparison == "minimum_importance", outcome], "reversed")
  expect_identical(chk$table[check == "procedure", status], "contradicted")
  expect_identical(csdg_assess(chk)$assessment, "not_met")
  expect_pass_through(chk)
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2, marginal_only = TRUE), imp)
  expect_identical(chk$table[check == "procedure", status], "supported")

  imp = make_importance_fixture(conditional = list(b = list(centers = rep(0.095, 4L), spread = 0.01)))
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp)
  comparisons = chk$details$procedure$m1$comparisons
  expect_identical(comparisons[comparison == "minimum_importance", outcome], "holds")
  rows = comparisons[comparison == "content" & predictor == "b"]
  expect_identical(rows$versus, c("c", "d"))
  expect_identical(rows[versus == "d", outcome], "holds")
  row = rows[versus == "c"]
  expect_equal(row$estimate, -0.005)
  expect_equal(row$threshold, stats::qt(0.975, 2) * 0.006455, tolerance = 1e-3)
  expect_identical(row$outcome, "inconclusive")
  expect_identical(chk$table[check == "procedure", status], "open")
  observation = chk$table[check == "procedure", observation]
  expect_match(observation,
    "b 0.0950 versus 0.0500 for c, the weakest of 2 comparisons \\(the result is inconclusive, within Monte Carlo")
  expect_match(observation, "Whether the result holds under the other perturbation is open")
})

test_that("grouped PFI that reverses the result contradicts the procedure", {
  groups = list(
    cd = list(members = c("c", "d"), centers = rep(0.16, 4L)),
    ac = list(members = c("a", "c"), centers = rep(0.40, 4L))
  )
  imp = make_importance_fixture(groups = groups)
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp)
  comparisons = chk$details$procedure$m1$comparisons
  expect_identical(unique(comparisons$versus), "cd")
  expect_identical(comparisons[predictor == "b", outcome], "reversed")
  expect_identical(chk$table[check == "procedure", status], "contradicted")
  expect_pass_through(chk)
})

test_that("grouped PFI is compared with every group without named predictors", {
  centers = fixture_centers()
  centers[, "b"] = 0.10
  groups = list(
    bc = list(members = c("b", "c"), centers = rep(0.12, 4L)),
    cd = list(members = c("c", "d"), centers = c(0.02, 0.18, 0.02, 0.18))
  )
  imp = make_importance_fixture(centers = list(m1 = centers), groups = groups)
  chk = csdg_check(claim_relies_mainly(imp, k = 1, factor = 2), imp, judgments = g0b_judged())
  expect_identical(chk$table[check == "content", status], "supported")
  comparisons = chk$details$procedure$m1$comparisons
  expect_identical(comparisons$versus, c("bc", "cd"))
  expect_identical(comparisons$outcome, c("holds", "inconclusive"))
  expect_identical(chk$table[check == "procedure", status], "open")
  expect_match(chk$table[check == "procedure", observation],
    "a 0.300 versus 0.100 for cd \\(c, d\\), the weakest of 2 groups without named predictors \\(the result is")
  expect_identical(csdg_assess(chk)$assessment, "unresolved")
})

# Hand-computed values for the symmetric rule. With K = 4, n_test / n_train = 1/3, and fold values with deviations
# (+0.08, -0.08, +0.06, -0.06), the corrected se is sqrt((1/4 + 1/3) * 0.02 / 3) = 0.062361 and the half-width
# qt(.975, 3) * se = 0.19846; with deviations (+0.09, -0.09, +0.07, -0.07), se = 0.071102 and half-width 0.22628.
# With spread 0.01 and R = 3, the Monte Carlo threshold of a difference a - b is qt(.975, 2) * sqrt(2e-4 / 3) / 2 =
# 0.017566, and of a - 2b it is qt(.975, 2) * sqrt(5e-4 / 3) / 2 = 0.027774.
open_pair_centers = function() {
  m = fixture_centers()
  m[, "b"] = c(0.24, 0.40, 0.26, 0.38)
  m
}

test_that("a negative margin whose interval includes zero is open, in both orders", {
  imp = make_importance_fixture(centers = list(m1 = open_pair_centers()))
  chk = suppressWarnings(csdg_check(claim_order(imp, "b", "a"), imp))
  m = chk$details$content$m1$margins
  expect_equal(m$estimate, 0.02)
  expect_equal(m$se, sqrt((1 / 4 + 1 / 3) * 0.02 / 3))
  expect_equal(c(m$lower, m$upper), 0.02 + c(-1, 1) * stats::qt(0.975, 3) * 0.062361, tolerance = 1e-4)
  expect_equal(m$mc_threshold, stats::qt(0.975, 2) * sqrt(2e-4 / 3) / 2)
  expect_equal(m$mc_threshold, 0.017566, tolerance = 1e-4)
  expect_identical(m$status, "open")
  expect_identical(m$reason, "interval")
  chk_ab = suppressWarnings(csdg_check(claim_order(imp, "a", "b"), imp, judgments = g0b_judged()))
  m_ab = chk_ab$details$content$m1$margins
  expect_equal(m_ab$estimate, -0.02)
  expect_equal(c(m_ab$lower, m_ab$upper), c(-0.21846, 0.17846), tolerance = 1e-4)
  expect_true(m_ab$beyond_mc)
  expect_identical(chk_ab$table[check == "content", status], "open")
  expect_identical(chk_ab$table[check == "stability", status], "contradicted")
  res_ab = csdg_assess(chk_ab)
  expect_identical(res_ab$assessment, "not_met")
  expect_identical(res_ab$decisive$check, "stability")
  expect_match(res_ab$report, "Also open: content \\(G2\\)")
  expect_match(chk_ab$table[check == "content", observation], "negative but inconclusive: the upper bound 0.178")
  expect_match(chk_ab$table[check == "content", summary],
    "the reversal is inconclusive \\(upper bound 0.178, above -0.0040\\)")
  expect_pass_through(chk_ab)
})

test_that("a relies-mainly margin beyond Monte Carlo error but with an inconclusive interval is open", {
  m = fixture_centers()
  m[, "b"] = c(0.125, 0.215, 0.135, 0.205)
  imp = make_importance_fixture(centers = list(m1 = m))
  chk = csdg_check(claim_relies_mainly(imp, k = 1, factor = 2), imp)
  margin = chk$details$content$m1$margins
  expect_equal(margin$estimate, -0.04)
  expect_equal(margin$mc_threshold, 0.027774, tolerance = 1e-4)
  expect_equal(c(margin$lower, margin$upper), -0.04 + c(-1, 1) * 0.22628, tolerance = 1e-4)
  expect_identical(margin$status, "open")
  expect_identical(chk$table[check == "content", status], "open")
  expect_identical(.csdg_ratio(chk$details$content$m1$ratios), "1.76")
})

test_that("a reversal beyond Monte Carlo error and the interval is contradicted", {
  imp = make_importance_fixture()
  chk = suppressWarnings(csdg_check(claim_order(imp, "b", "a"), imp))
  m = chk$details$content$m1$margins
  expect_equal(c(m$lower, m$upper), -0.10 + c(-1, 1) * stats::qt(0.975, 3) * 0.012472, tolerance = 1e-4)
  expect_identical(m$status, "contradicted")
  chk = suppressWarnings(csdg_check(claim_order(imp, "b", "a"), imp, min_difference = 0.07))
  expect_identical(chk$details$content$m1$margins$status, "open")
  chk = suppressWarnings(csdg_check(claim_order(imp, "b", "a"), imp, min_difference = 0.07,
    interval_rule = "report_only"))
  expect_identical(chk$details$content$m1$margins$status, "contradicted")
})

test_that("G6a uses the same symmetric rule for each covered learner", {
  imp = make_importance_fixture(centers = list(m1 = fixture_centers(), m2 = open_pair_centers()))
  chk = csdg_check(claim_order(imp, "a", "b", learners = c("m1", "m2")), imp, judgments = g0b_judged())
  content = chk$table[gate == "G6a" & check == "content"]
  expect_identical(content$learner, c("m1", "m2"))
  expect_identical(content$status, c("supported", "open"))
  expect_false(any(chk$table$gate == "G2" & chk$table$check == "content"))
  res = csdg_assess(chk)
  expect_identical(res$assessment, "unresolved")
  expect_match(res$report, "Content \\(G6a\\) for learner m2 is open")
  expect_identical(chk$table[gate == "G2" & check == "procedure" & learner == "m2", role], "context")
  expect_pass_through(chk)
})

test_that("the minimum importance is open within Monte Carlo error on either side", {
  centers = fixture_centers()
  centers[, "b"] = 0.010
  imp = make_importance_fixture(centers = list(m1 = centers))
  chk = suppressWarnings(csdg_check(claim_relies_mainly(imp, k = 2, predictors = c("a", "b")), imp))
  tab = chk$details$minimum_importance$m1$table
  expect_equal(tab[predictor == "b", mc_threshold], stats::qt(0.975, 2) * sqrt(4 * 0.01^2 / 3) / 4)
  expect_true(tab[predictor == "b", holds])
  expect_false(tab[predictor == "b", beyond_mc])
  expect_identical(chk$table[check == "minimum_importance", status], "open")
})

test_that("a shortfall of the minimum importance counts only beyond the corrected interval", {
  centers = fixture_centers()
  centers[, "b"] = c(0.0005, 0.0035, 0.0005, 0.0035)
  spread = fixture_spread()
  spread[, "b"] = 0.0001
  imp = make_importance_fixture(centers = list(m1 = centers), spread = list(m1 = spread))
  chk = suppressWarnings(csdg_check(claim_relies_mainly(imp, k = 2, predictors = c("a", "b")), imp))
  tab = chk$details$minimum_importance$m1$table
  expect_true(tab[predictor == "b", beyond_mc])
  expect_equal(tab[predictor == "b", upper], 0.002 + stats::qt(0.975, 3) * sqrt((1 / 4 + 1 / 3) * 3e-6))
  expect_equal(tab[predictor == "b", upper], 0.0062100, tolerance = 1e-4)
  expect_identical(tab[predictor == "b", status], "open")
  expect_match(chk$table[check == "minimum_importance", observation],
    "so the shortfall does not exceed its uncertainty")
  chk = suppressWarnings(csdg_check(claim_relies_mainly(imp, k = 2, predictors = c("a", "b")), imp,
    interval_rule = "report_only"))
  expect_identical(chk$table[check == "minimum_importance", status], "contradicted")
})

test_that("a conditional shortfall within the corrected interval leaves the procedure open", {
  imp = make_importance_fixture(conditional = list(b = list(centers = c(0.0005, 0.0035, 0.0005, 0.0035),
    spread = 0.0001)))
  chk = csdg_check(claim_order(imp, "b", "c"), imp)
  row = chk$details$procedure$m1$comparisons[comparison == "minimum_importance"]
  expect_identical(row$status, "open")
  expect_identical(row$outcome, "inconclusive")
  expect_identical(chk$table[check == "procedure", status], "open")
  expect_match(chk$table[check == "procedure", observation],
    "ithin the supplied strata, b adds 0.0020 \\(below the minimum importance 0.0040, inconclusive given its corrected")
})

test_that("a perturbation counts only if the marginal result holds", {
  conditional = list(a = list(centers = rep(0.30, 4L)), b = list(centers = rep(0.10, 4L)))
  imp = make_importance_fixture(conditional = conditional)
  chk = suppressWarnings(csdg_check(claim_order(imp, "b", "a"), imp))
  comparisons = chk$details$procedure$m1$comparisons
  expect_identical(comparisons[comparison == "content", outcome], "unchanged")
  expect_identical(chk$table[check == "procedure", status], "supported")
  expect_identical(chk$table[check == "content", status], "contradicted")
  expect_match(chk$table[check == "procedure", observation], "as under marginal permutation")
})

test_that("ties at the cutoff do not reproduce a selection", {
  P = c(a = 0.3, b = 0, c = 0, d = 0)
  expect_false(.csdg_fold_ranking(P, c("a", "b"), 1, "top_k")$holds)
  expect_true(.csdg_fold_ranking(P, "a", 1, "top_k")$holds)
  expect_false(.csdg_fold_ranking(P, c("a", "b"), 2, "relies_mainly")$holds)
  m = fixture_centers()
  m[, c("c", "d")] = 0.05
  imp = make_importance_fixture(centers = list(m1 = m))
  expect_warning(claim_top_k(imp, k = 3), "ties at the cutoff")
})

test_that("the dependence screen flags a high R^2 on the other predictors as an unresolved threat to G2", {
  set.seed(4)
  z = stats::rnorm(50)
  dat = data.frame(a = z, b = z + stats::rnorm(50, sd = 0.1), c = stats::rnorm(50), d = stats::rnorm(50), y = z)
  imp = make_importance_fixture()
  imp$fits$task = mlr3::as_task_regr(dat, target = "y")
  r2 = summary(stats::lm(a ~ b + c + d, data = dat))$r.squared
  chk = csdg_check(claim_relies_mainly(imp, k = 2), imp, judgments = g0b_judged())
  screen = chk$details$dependence
  expect_equal(screen[predictor == "a", r2], r2, tolerance = 1e-10)
  expect_identical(screen[predictor == "a", partner], "b")
  expect_equal(screen[predictor == "a", partner_r2], stats::cor(dat$a, dat$b)^2, tolerance = 1e-10)
  row = chk$table[check == "dependence"]
  expect_identical(row$role, "unresolved_threat")
  expect_identical(row$status, "open")
  expect_match(row$observation, sprintf("a: R\\^2 = %s on the other 3 predictors \\(b alone: %s\\)", .csdg_r(r2),
    .csdg_r(stats::cor(dat$a, dat$b)^2)))
  res = csdg_assess(chk)
  expect_identical(res$assessment, "unresolved")
  expect_identical(res$decisive$check, "dependence")
  expect_match(res$report, "Dependence \\(G2\\) is open")
  expect_match(paste(res$hints, collapse = " "), "conditional = list\\(a = \"b\", b = \"a\"\\)")
  expect_pass_through(chk)
  qualified = csdg_check(claim_relies_mainly(imp, k = 2, marginal_only = TRUE), imp, judgments = g0b_judged())
  expect_identical(qualified$table[check == "dependence", role], "context")
  expect_identical(csdg_assess(qualified)$assessment, "met")
  clear = csdg_check(claim_relies_mainly(imp, k = 2), imp, max_r2 = 0.999)
  expect_identical(clear$table[check == "dependence", role], "context")
  expect_match(clear$table[check == "dependence", observation], "no named predictor reaches \\.999")
  expect_identical(clear$criteria$sources$max_r2, "set by the analyst")
})

test_that("the dependence screen detects joint dependence that no pairwise correlation shows", {
  set.seed(7)
  n = 400L
  dat = data.frame(b = stats::rnorm(n), c = stats::rnorm(n), d = stats::rnorm(n))
  dat$a = (dat$b + dat$c) / sqrt(2) + stats::rnorm(n, sd = 0.3)
  dat$y = dat$a + stats::rnorm(n)
  imp = make_importance_fixture()
  imp$fits$task = mlr3::as_task_regr(dat[, c("a", "b", "c", "d", "y")], target = "y")
  expect_lt(max(abs(stats::cor(dat)["a", c("b", "c", "d")])), 0.7)
  chk = csdg_check(claim_relies_mainly(imp, k = 2), imp)
  expect_gt(chk$details$dependence[predictor == "a", r2], 0.85)
  expect_true(chk$details$dependence[predictor == "a", flagged])
  expect_identical(chk$table[check == "dependence", role], "unresolved_threat")
  .csdg_reset_deprecations()
  expect_warning(old <- csdg_check(claim_relies_mainly(imp, k = 2), imp, max_cor = 0.7), "deprecated")
  expect_equal(old$criteria$max_r2, 0.49)
  expect_true(old$details$dependence[predictor == "a", flagged])
})

test_that("the dependence screen encodes factors, honors declared groups, and skips covered predictors", {
  set.seed(8)
  n = 300L
  g = factor(sample(c("x", "y", "z"), n, TRUE))
  dat = data.frame(a = as.numeric(g == "x") + stats::rnorm(n, sd = 0.1), b = g, c = stats::rnorm(n),
    d = stats::rnorm(n))
  dat$y = dat$a + stats::rnorm(n)
  imp = make_importance_fixture()
  imp$fits$task = mlr3::as_task_regr(dat, target = "y")
  chk = csdg_check(claim_relies_mainly(imp, k = 2), imp)
  screen = chk$details$dependence
  expect_gt(screen[predictor == "a", r2], 0.9)
  expect_identical(screen[predictor == "a", partner], "b")
  expect_true(screen[predictor == "b", factor])
  expect_gt(screen[predictor == "b", r2], 0.9)
  declared = make_importance_fixture()
  declared$fits$task = mlr3::as_task_regr(data.frame(a = stats::rnorm(n), b = stats::rnorm(n), c = stats::rnorm(n),
    d = stats::rnorm(n), y = stats::rnorm(n)), target = "y")
  chk = csdg_check(claim_relies_mainly(declared, k = 2), declared, dependent_groups = list(scale = c("a", "c")))
  expect_true(chk$details$dependence[predictor == "a", flagged])
  expect_false(chk$details$dependence[predictor == "b", flagged])
  expect_match(chk$table[check == "dependence", observation], "group of dependent predictors scale")
  expect_error(csdg_check(claim_relies_mainly(declared, k = 2), declared, dependent_groups = list(s = "a")),
    "at least two")
  expect_error(csdg_check(claim_relies_mainly(declared, k = 2), declared, dependent_groups = list(s = c("a", "q"))),
    "Unknown predictors")
  covered = make_importance_fixture(conditional = list(a = list(centers = rep(0.3, 4L)),
    b = list(centers = rep(0.2, 4L))))
  covered$fits$task = imp$fits$task
  chk = csdg_check(claim_relies_mainly(covered, k = 2), covered)
  expect_false(any(chk$details$dependence$flagged))
  expect_identical(chk$table[check == "dependence", role], "context")
  expect_match(chk$table[check == "dependence", observation], "conditional PFI covers a and b")
})

test_that("grouped PFI covers a named predictor only if its group contains the predictors on which it depends", {
  set.seed(9)
  n = 400L
  dat = data.frame(b = stats::rnorm(n), c = stats::rnorm(n), d = stats::rnorm(n))
  dat$a = 0.4 * dat$b + 0.9 * dat$c + stats::rnorm(n, sd = 0.2)
  dat$y = dat$a + stats::rnorm(n)
  task = mlr3::as_task_regr(dat[, c("a", "b", "c", "d", "y")], target = "y")
  incomplete = make_importance_fixture(groups = list(ab = list(members = c("a", "b"), centers = rep(0.45, 4L))))
  incomplete$fits$task = task
  chk = csdg_check(claim_relies_mainly(incomplete, k = 2), incomplete, judgments = g0b_judged())
  screen = chk$details$dependence
  r2_out = summary(stats::lm(a ~ c + d, data = dat))$r.squared
  expect_equal(screen[predictor == "a", r2_outside], r2_out, tolerance = 1e-10)
  expect_gt(r2_out, 0.5)
  expect_identical(screen[predictor == "a", partner_outside], "c")
  expect_identical(screen$group, c("ab", "ab"))
  expect_identical(screen$covered, c(FALSE, TRUE))
  expect_identical(screen$flagged, c(TRUE, FALSE))
  row = chk$table[check == "dependence"]
  expect_identical(row$role, "unresolved_threat")
  expect_match(row$observation, sprintf("and %s on the 2 predictors outside its group ab \\(c alone: %s\\)",
    .csdg_r(r2_out), .csdg_r(screen[predictor == "a", partner_outside_r2])))
  expect_match(row$observation, "no group of grouped PFI contains it together with the predictors on which it depends")
  expect_match(chk$table[check == "procedure", observation], "Grouped PFI of ab \\(a, b\\) 0.450 measures reliance")
  res = csdg_assess(chk)
  expect_identical(res$assessment, "unresolved")
  expect_match(res$hints, "conditional = list\\(a = \"c\", c = \"a\"\\)\\)\\), add the predictors on which a depends")
  expect_pass_through(chk)
  qualified = csdg_check(claim_relies_mainly(incomplete, k = 2, marginal_only = TRUE), incomplete)
  expect_identical(qualified$table[check == "dependence", role], "context")
  complete = make_importance_fixture(groups = list(abc = list(members = c("a", "b", "c"), centers = rep(0.5, 4L))))
  complete$fits$task = task
  chk = csdg_check(claim_relies_mainly(complete, k = 2), complete, judgments = g0b_judged())
  expect_true(all(chk$details$dependence$covered))
  expect_identical(chk$table[check == "dependence", role], "context")
  expect_match(chk$table[check == "dependence", observation],
    "grouped PFI of abc covers a and b \\(R\\^2 = \\.00 and \\.00 on the predictor outside the group\\)")
  expect_identical(csdg_assess(chk)$assessment, "met")
  declared = make_importance_fixture(groups = list(ab = list(members = c("a", "b"), centers = rep(0.45, 4L))))
  declared$fits$task = mlr3::as_task_regr(data.frame(a = stats::rnorm(n), b = stats::rnorm(n), c = stats::rnorm(n),
    d = stats::rnorm(n), y = stats::rnorm(n)), target = "y")
  chk = csdg_check(claim_relies_mainly(declared, k = 2), declared, dependent_groups = list(scale = c("a", "c")))
  expect_identical(chk$details$dependence$flagged, c(TRUE, FALSE))
  expect_match(chk$table[check == "dependence", observation], "declared by the researcher \\(c outside its group ab\\)")
  chk = csdg_check(claim_relies_mainly(declared, k = 2), declared, dependent_groups = list(scale = c("a", "b")))
  expect_false(any(chk$details$dependence$flagged))
})

test_that("print, details, summary, and plot of a check", {
  imp = make_importance_fixture(labels = c(m1 = "XGBoost"))
  chk = csdg_check(claim_relies_mainly(imp, k = 2), imp)
  out = format(chk)
  expect_true(any(grepl("^gate  check", out)))
  expect_true(any(grepl("^G0a   specification", out)))
  expect_true(any(grepl("^G0b   measurement         open", out)))
  expect_true(any(grepl("^G2    minimum importance", out)))
  expect_false(any(grepl("Next:", out)))
  expect_true(any(grepl("Criterion:", format(chk, details = TRUE))))
  expect_named(summary(chk), c("gate", "check", "learner", "role", "status"))
  expect_s3_class(plot(chk), "ggplot")
})
