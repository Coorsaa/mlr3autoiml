swapped_centers = function() {
  m = fixture_centers()
  m[, c("b", "c")] = m[, c("c", "b")]
  m
}

test_that("a claim over all learners requires the result for each (G6a)", {
  imp = make_importance_fixture(centers = list(m1 = fixture_centers(), m2 = swapped_centers()))
  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2, learners = c("m1", "m2")), imp)
  expect_identical(chk$table[gate == "G6a" & learner == "m2" & check == "content", status], "contradicted")
  expect_identical(chk$table[gate == "G6a" & learner == "m1" & check == "content", status], "supported")
  expect_identical(chk$table[gate == "G6a" & learner == "m1" & check == "minimum_importance", status], "supported")
  expect_identical(chk$table[gate == "G5" & learner == "m2", role], "context")
  expect_identical(chk$table[gate == "G2" & learner == "m2", role], "context")
  expect_false(any(chk$table$gate == "G2" & chk$table$check == "content"))
  res = csdg_assess(chk)
  expect_identical(res$assessment, "not_met")
  expect_identical(unique(res$decisive$gate), "G6a")
  expect_match(res$report, "Content \\(G6a\\) for learner m2 is contradicted")
  expect_identical(claim_relies_mainly(imp, learners = names(imp$pfi))$id, "relies_mainly_a_b_all")
  expect_pass_through(chk)

  chk = csdg_check(claim_relies_mainly(imp, k = 2, factor = 2), imp)
  expect_identical(chk$table[gate == "G6a", role], "context")
  expect_match(chk$table[gate == "G6a", observation], "does not hold")
  expect_identical(csdg_assess(chk)$assessment, "met")
})

test_that("the gate plan follows the claim's model scope", {
  imp = make_importance_fixture(centers = list(m1 = fixture_centers(), m2 = fixture_centers()))
  chk = csdg_check(claim_relies_mainly(imp), imp)
  expect_identical(chk$plan[required == TRUE, gate_id], c("G0a", "G0b", "G2", "G5"))
  expect_identical(chk$plan[gate_id == "G1", plan_role], "context")
  chk = csdg_check(claim_relies_mainly(imp, learners = c("m1", "m2")), imp)
  expect_identical(chk$plan[required == TRUE, gate_id], c("G0a", "G0b", "G2", "G5", "G6a"))
})

test_that("constructs need a judgment, and judgments never offset a contradiction", {
  imp = make_importance_fixture()
  claim = claim_relies_mainly(imp, refers_to = "constructs")
  chk = csdg_check(claim, imp)
  expect_identical(chk$table[gate == "G0b", status], "open")
  expect_identical(csdg_assess(chk)$assessment, "unresolved")
  judged = csdg_check(claim, imp, judgments = csdg_judge("G0b", supported(), note = "Reliability .86."))
  expect_identical(judged$table[gate == "G0b", check], "judgment")
  expect_identical(csdg_assess(judged)$assessment, "met")
  expect_pass_through(judged)
  coding = csdg_check(claim, imp, judgments = list(csdg_judge("G0b", supported(), note = "Reliability .86."),
    csdg_judge("G2", contradicted(), note = "coding error")))
  expect_identical(csdg_assess(coding)$assessment, "not_met")
  groups = list(cd = list(members = c("c", "d"), centers = rep(0.16, 4L)))
  imp = make_importance_fixture(groups = groups)
  chk = csdg_check(claim_relies_mainly(imp), imp, judgments = csdg_judge("G2", supported(), note = "reviewed"))
  expect_identical(csdg_assess(chk)$assessment, "not_met")
  expect_warning(csdg_check(claim_relies_mainly(imp), imp, judgments = csdg_judge("G3a", open_status(), "n/a")),
    "context")
  expect_output(print(csdg_judge("G0b", supported(), note = "ok")), "<CSDG judgment> G0b supported: ok")
})

test_that("labels and provenance distinguish exploration from a test on new data", {
  imp = make_importance_fixture()
  claim = claim_relies_mainly(imp)
  expect_identical(claim$record$provenance$origin, "retrospective_exploratory")
  chk = csdg_check(claim, imp)
  res = csdg_assess(chk)
  expect_identical(res$label, "met (exploratory)")
  expect_match(res$report, "established only after a test on new data")
  expect_error(csdg_confirm(chk, imp), "same data")
  new_imp = make_importance_fixture(data_hash = "new_data")
  conf = csdg_confirm(chk, new_imp)
  expect_s3_class(conf, "CSDGConfirmation")
  expect_identical(conf$label, "met (tested on new data)")
  expect_identical(conf$check$claim$record$provenance$origin, "independently_confirmed")
  expect_identical(conf$check$claim$record$provenance$evidence_ids, "fixture:new_data")
  expect_identical(conf$exploratory$label, "met (exploratory)")
  expect_identical(conf$check$criteria$fixed, chk$criteria$fixed)
  other = claim_order(imp, "a", "b")
  expect_error(csdg_confirm(chk, new_imp, claim = other), "claim of `chk`")
  expect_error(csdg_confirm(claim, chk), "csdg_confirm\\(chk, new\\)")
  expect_identical(csdg_confirm(chk, new_imp, claim = claim)$label, "met (tested on new data)")
  expect_output(print(conf), "Exploratory result: met \\(exploratory\\)")
  expect_error(csdg_check(claim, new_imp), "csdg_confirm")
})

test_that("reports name the decisive gate", {
  imp = make_importance_fixture(conditional = list(b = list(centers = rep(0.001, 4L), spread = 0.0001)))
  res = csdg_assess(csdg_check(claim_order(imp, "b", "c"), imp))
  expect_identical(res$assessment, "not_met")
  expect_match(res$report, "Not met \\(exploratory\\): Procedure \\(G2\\) is contradicted: within the supplied strata, b")
  expect_match(res$report, "supported only under marginal permutation")
  row = as.data.table(res)
  expect_named(row, c("claim_id", "statement", "assessment", "label", "origin", "decisive_gates",
    "decisive_checks", "decisive_property", "decision_options", "supported_gates", "contradicted_gates", "open_gates", "report"))
  expect_identical(row$decisive_gates, "G2")
})

test_that("print, format, and as.data.table methods", {
  imp = make_importance_fixture(centers = list(m1 = fixture_centers(), m2 = swapped_centers()),
    labels = c(m1 = "XGBoost", m2 = "ridge logistic regression"))
  claim = claim_relies_mainly(imp, k = 2, factor = 2)
  chk = csdg_check(claim, imp)
  res = csdg_assess(chk)
  expect_snapshot(print(imp))
  expect_snapshot(print(claim))
  expect_snapshot(print(chk))
  expect_snapshot(print(res))
  expect_named(as.data.table(chk), c("gate", "gate_name", "check", "learner", "role", "property", "criterion",
    "observation", "relevance", "status", "summary"))
  expect_named(as.data.table(claim), c("id", "kind", "statement", "origin", "quantity", "model", "procedure",
    "data", "meaning", "use"))
  expect_identical(nrow(as.data.table(imp, level = "fold")), 32L)
  expect_identical(format(chk)[[1L]], "<CSDG check> XGBoost relies mainly on a and b.")
})

test_that("plot() draws the leading predictors per learner", {
  imp = make_importance_fixture(centers = list(m1 = fixture_centers(), m2 = swapped_centers()))
  p = plot(imp)
  expect_s3_class(p, "ggplot")
  built = ggplot2::ggplot_build(p)
  expect_identical(length(unique(built$data[[1L]]$PANEL)), 2L)
  expect_s3_class(plot(imp, scale = "relative", n = 2L), "ggplot")
  expect_s3_class(plot(imp, features = c("c", "d"), min_importance = 0.05), "ggplot")
  layers = vapply(p$layers, function(l) class(l$geom)[[1L]], character(1L))
  expect_true("GeomText" %in% layers)
})

test_that("csdg_export_assessment() writes the records without row-level data", {
  imp = make_importance_fixture()
  res = csdg_assess(csdg_check(claim_relies_mainly(imp), imp))
  dir = csdg_export_assessment(res, withr::local_tempdir())
  expect_true(all(c("claim.json", "criteria.json", "check.csv", "records.json", "gate_plan.csv",
    "assessment.json") %in% list.files(dir)))
  assessment = jsonlite::read_json(file.path(dir, "assessment.json"))
  expect_identical(assessment$assessment, "met")
  conf = csdg_confirm(res$check, make_importance_fixture(data_hash = "new_data"))
  expect_true("exploratory.csv" %in% list.files(csdg_export_assessment(conf, withr::local_tempdir())))
})

test_that("a confirmation checks settings, overlap, and labels", {
  imp = make_importance_fixture(conditional = list(b = list(centers = rep(0.15, 4L))),
    labels = c(m1 = "XGBoost"))
  chk = csdg_check(claim_relies_mainly(imp, k = 2), imp)
  plain = make_importance_fixture(data_hash = "new_data")
  expect_error(csdg_confirm(chk, plain), "conditional PFI of b")
  new_imp = make_importance_fixture(conditional = list(b = list(centers = rep(0.15, 4L))), data_hash = "new_data",
    labels = c(m1 = "xgb"))
  conf = csdg_confirm(chk, new_imp)
  expect_match(conf$check$table[check == "content", property], "XGBoost relies mainly")
  expect_identical(conf$check$claim$selection, "Claim and criteria fixed in the exploratory check.")
  other_reps = new_imp
  other_reps$repetitions = 5L
  expect_warning(csdg_confirm(chk, other_reps), "5 permutations per fold; the check used 3")
  chk$source$row_hashes = c("r1", "r2")
  overlapping = new_imp
  overlapping$fits$row_hashes = c("r9", "r2")
  expect_error(csdg_confirm(chk, overlapping), "1 of the new rows also occurs in the data of the check")
})

test_that("reports suggest the closest claims and list other open properties", {
  imp = make_importance_fixture()
  res = csdg_assess(csdg_check(claim_relies_mainly(imp, k = 1), imp))
  expect_identical(res$assessment, "not_met")
  expect_match(res$hints, "claim_top_k\\(k = 1\\)\\) holds on average and reproduces in 4 of 4 folds")
  expect_match(res$hints, "with k = 2 \\(a, b\\), the selection reproduces in 4 of 4 folds")
  expect_match(res$report, "a has 1.50 times the PFI")
  centers = fixture_centers()
  centers[, "b"] = c(0.125, 0.215, 0.135, 0.205)
  imp = make_importance_fixture(centers = list(m1 = centers))
  res = csdg_assess(csdg_check(claim_relies_mainly(imp, k = 1), imp))
  expect_identical(res$assessment, "not_met")
  expect_match(res$report, "Also open: content \\(G2\\)")
})
