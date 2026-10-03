# Synthetic CSDGImportance objects with known fold and permutation values (no fitting).
# K = 4 folds, R = 3 permutations per fold with values center + c(-spread, 0, spread), features a, b, c, d.
fixture_centers = function() {
  cbind(
    a = c(0.30, 0.30, 0.30, 0.30),
    b = c(0.20, 0.18, 0.22, 0.20),
    c = c(0.05, 0.05, 0.05, 0.05),
    d = c(0.01, 0.00, 0.02, 0.01)
  )
}

fixture_pfi = function(centers, spread, loss = "mse") {
  rows = list()
  for (g in colnames(centers)) {
    for (k in seq_len(nrow(centers))) {
      s = if (is.matrix(spread)) spread[k, g] else spread
      rows[[length(rows) + 1L]] = data.table::data.table(
        iteration = k, repetition = 1L, fold = k, feature_group = g,
        features = attr(centers, "members")[[g]] %||% g, permutation_repetition = 1:3, n_assessment = 100L,
        loss = loss, baseline_loss = 0.6, permuted_loss = 0.6 + centers[k, g] + c(-s, 0, s),
        importance = centers[k, g] + c(-s, 0, s)
      )
    }
  }
  raw = data.table::rbindlist(rows)
  per_iteration = raw[, .(
    importance = mean(importance), monte_carlo_sd = stats::sd(importance),
    monte_carlo_se = stats::sd(importance) / sqrt(.N), n_permutations = .N,
    baseline_loss = mean(baseline_loss), permuted_loss = mean(permuted_loss), n_assessment = max(n_assessment)
  ), by = .(iteration, repetition, fold, feature_group, features, loss)]
  list(raw = raw, per_iteration = per_iteration)
}

fixture_spread = function(value = 0.01) {
  out = matrix(value, 4L, 4L, dimnames = list(NULL, c("a", "b", "c", "d")))
  out
}

# `centers`: named list per learner of 4 x 4 matrices; `groups`: named list group -> list(members, centers, spread);
# `conditional`: named list feature -> list(centers, spread), applied to every learner.
make_importance_fixture = function(centers = list(m1 = fixture_centers()), spread = list(), loss = 0.6,
                                   loss_baseline = 1.0, groups = NULL, conditional = NULL,
                                   labels = NULL, data_hash = "fixture_data") {
  learners = names(centers)
  fits = structure(list(
    task = NULL, task_id = "fixture", task_type = "regr", target = "y", positive = NULL,
    features = c("a", "b", "c", "d"), n = 400L,
    labels = labels %||% stats::setNames(paste("learner", learners), learners),
    resamples = stats::setNames(vector("list", length(learners)), learners), baseline = NULL,
    resampling = NULL, resampling_label = "4-fold cross-validation", K = 4L,
    n_train = rep(300L, 4L), n_test = rep(100L, 4L), seed = 1L, data_hash = data_hash, created = "fixture",
    loss = "mse", measure = NULL, row_hashes = NULL
  ), class = c("CSDGFits", "list"))
  pfi = lapply(learners, function(l) {
    m = centers[[l]]
    s = spread[[l]] %||% fixture_spread()
    members = stats::setNames(as.list(colnames(m)), colnames(m))
    for (g in names(groups)) {
      m = cbind(m, groups[[g]]$centers)
      colnames(m)[ncol(m)] = g
      s = cbind(s, rep(groups[[g]]$spread %||% 0.01, 4L))
      colnames(s)[ncol(s)] = g
      members[[g]] = paste(groups[[g]]$members, collapse = "|")
    }
    attr(m, "members") = members
    fixture_pfi(m, s)
  })
  names(pfi) = learners
  cond = NULL
  if (length(conditional)) {
    cond = lapply(learners, function(l) {
      out = lapply(names(conditional), function(j) {
        m = matrix(conditional[[j]]$centers, ncol = 1L, dimnames = list(NULL, j))
        fixture_pfi(m, conditional[[j]]$spread %||% 0.01)
      })
      stats::setNames(out, names(conditional))
    })
    names(cond) = learners
  }
  losses = data.table::rbindlist(lapply(learners, function(l) {
    data.table::data.table(learner = l, iteration = 1:4, loss = loss, loss_baseline = loss_baseline)
  }))
  group_list = if (length(groups)) lapply(groups, `[[`, "members") else NULL
  .csdg_new_importance(fits, loss = "mse", repetitions = 3L, batch_size = 1L, seed = 2L, groups = group_list,
    strata_features = names(conditional),
    strata_definition = if (length(conditional)) {
      stats::setNames(rep("the supplied strata", length(conditional)), names(conditional))
    },
    pfi = pfi, conditional = cond, losses = losses)
}

with_fold3 = function(values, m = fixture_centers()) {
  for (nm in names(values)) m[3L, nm] = values[[nm]]
  m
}

expect_pass_through = function(chk) {
  adjudication = csdg_adjudicate_claim(chk$records, claim_applicable = TRUE, plan = chk$plan)
  testthat::expect_identical(csdg_assess(chk)$assessment, adjudication$assessment)
  for (record in chk$records) testthat::expect_s3_class(.csdg_revalidate_evidence_record(record),
    "CSDGEvidenceRecord")
}
