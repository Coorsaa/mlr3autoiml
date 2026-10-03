# Text helpers of the analysis-first interface. All generated sentences are built here so that wording and number
# formats are consistent across claims, checks, assessments, and printed output.

.csdg_and = function(x) {
  x = as.character(x)
  n = length(x)
  if (!n) return("")
  if (n == 1L) return(x)
  if (n == 2L) return(paste(x[[1L]], "and", x[[2L]]))
  paste0(paste(x[-n], collapse = ", "), ", and ", x[[n]])
}

.csdg_sentence = function(x) {
  x = as.character(x)
  paste0(toupper(substring(x, 1L, 1L)), substring(x, 2L))
}

.csdg_number_word = function(k) {
  words = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  k = as.integer(k)
  if (k >= 1L && k <= 10L) words[[k]] else as.character(k)
}

# Three significant digits, trailing zeros kept, at most four decimals; exact zero is "0".
.csdg_num = function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    if (!is.finite(v)) return(as.character(v))
    if (v == 0) return("0")
    digits = min(4L, max(0L, 2L - as.integer(floor(log10(abs(v))))))
    v = round(v, digits)
    if (v == 0) v = 0
    sprintf("%.*f", digits, v)
  }, character(1L), USE.NAMES = FALSE)
}

.csdg_pct = function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    p = 100 * v
    if (abs(p) >= 10) sprintf("%.0f%%", p) else sprintf("%.1f%%", p)
  }, character(1L), USE.NAMES = FALSE)
}

.csdg_pct_value = function(x) {
  # Percent of a criterion value, e.g. 0.01 -> "1%".
  p = 100 * x
  if (abs(p - round(p)) < 1e-9) sprintf("%d%%", as.integer(round(p))) else sprintf("%s%%", format(p))
}

# A criterion value as supplied, for example 0.02 (not 0.0200).
.csdg_crit = function(x) {
  formatC(signif(x, 3L), digits = 3L, format = "fg")
}

.csdg_ci = function(estimate, lower, upper) {
  sprintf("%s [%s, %s]", .csdg_num(estimate), .csdg_num(lower), .csdg_num(upper))
}

.csdg_folds = function(n, K) {
  sprintf("%d of %d folds", as.integer(n), as.integer(K))
}

.csdg_count = function(n) {
  format(as.integer(n), big.mark = ",")
}

.csdg_loss_label = function(loss) {
  switch(loss, logloss = "log loss", rmse = "RMSE", mse = "squared error", mae = "absolute error",
    brier = "Brier score", loss)
}

.csdg_loss_units = function(loss) {
  switch(loss, logloss = "log-loss units", rmse = "RMSE units", mse = "squared-error units",
    mae = "absolute-error units", brier = "Brier-score units", "loss units")
}

# Ratios with three significant digits, so that a failing 1.96 is not shown as 2.0.
.csdg_ratio = function(x) .csdg_num(x)

.csdg_r = function(r) {
  sub("^(-?)0\\.", "\\1.", sprintf("%.2f", r))
}

.csdg_task_type_label = function(task_type) {
  if (identical(task_type, "classif")) "binary classification" else "regression"
}

.csdg_wrap = function(text, indent = 0L, exdent = indent, width = getOption("width", 80L)) {
  paste(strwrap(text, width = max(40L, width), indent = indent, exdent = exdent), collapse = "\n")
}

.csdg_learner_label = function(fits, learner) {
  unname(fits$labels[learner])
}

.csdg_learners_text = function(fits, learners) {
  .csdg_and(.csdg_learner_label(fits, learners))
}

.csdg_feature_label = function(claim, feature) {
  labels = claim$labels
  out = as.character(feature)
  if (length(labels)) {
    hit = feature %in% names(labels)
    out[hit] = unname(labels[feature[hit]])
  }
  out
}

.csdg_scale_label = function(task_type) {
  if (identical(task_type, "classif")) "the probability scale" else "the scale of the outcome"
}

# Row texts of a check. Each returns property, criterion, observation, relevance, and summary (one clause for
# reports).

.csdg_lower_first = function(x) paste0(tolower(substring(x, 1L, 1L)), substring(x, 2L))

.csdg_verb = function(n, singular, plural) if (n == 1L) singular else plural

.csdg_squash = function(texts) lapply(texts, function(text) if (is.character(text)) gsub("\\s+", " ", text) else text)

.csdg_interval_criterion = function(criteria, delta, n) {
  level = .csdg_pct_value(criteria$level)
  if (identical(criteria$interval_rule, "report_only")) {
    return(sprintf("%s corrected %s %s reported.", .csdg_verb(n, "its", "their"), level,
      .csdg_verb(n, "interval is", "intervals are")))
  }
  sprintf(paste("%s corrected %s %s at least %s (the smallest relevant difference); a reversal counts only if the",
    "upper bound is at most -%s."), .csdg_verb(n, "the lower bound of its", "the lower bounds of their"), level,
    .csdg_verb(n, "interval is", "intervals are"), .csdg_num(delta), .csdg_num(delta))
}

.csdg_text_specification = function() {
  list(
    property = "The claim and its six scope elements are stated.",
    criterion = "All six scope elements are recorded.",
    observation = paste("Generated from the analysis: quantity, model, procedure, data, meaning (model description),",
      "and use."),
    relevance = "They fix which quantity, learner, and procedure the claim refers to.",
    summary = "the claim and its six scope elements are stated (generated from the analysis)"
  )
}

.csdg_text_measurement = function(claim, fits) {
  named = c(claim$predictors, claim$first, claim$second, claim$feature)
  constructs = identical(claim$refers_to, "constructs")
  list(
    property = "The data cover what the claim names.",
    criterion = if (constructs) {
      "Validity evidence for each named construct (for example reliability and measurement invariance)."
    } else {
      "The claim names analyzed variables, not constructs, and no population beyond the sample."
    },
    observation = if (constructs) {
      paste("The claim names constructs; reliability and validity evidence are not computed by the package",
        "(enter them with csdg_judge()).")
    } else {
      sprintf("The claim names the %s %s of task %s (%s rows); no construct or other population is named.",
        .csdg_verb(length(named), "variable", "variables"), .csdg_and(named), fits$task_id, .csdg_count(fits$n))
    },
    relevance = paste("A variable name needs documentation of how it was measured; a construct also needs validity",
      "evidence, which no explanation supplies."),
    summary = if (constructs) {
      "the claim names constructs, and no validity evidence was entered (csdg_judge())"
    } else {
      "the claim names analyzed variables, not constructs"
    }
  )
}

.csdg_text_performance = function(view, loss, prefix) {
  imp_row = view$improvement
  text = if (imp_row$improvement > 0) {
    sprintf("held-out %s %s versus %s for the model without predictors (improvement %s, %s); %s improve.",
      .csdg_loss_label(loss), .csdg_num(imp_row$loss), .csdg_num(imp_row$loss_baseline),
      .csdg_num(imp_row$improvement), .csdg_pct(imp_row$relative_improvement),
      sprintf("%d of %d folds", imp_row$folds_improved, imp_row$K))
  } else {
    sprintf("held-out %s %s versus %s for the model without predictors: the learner does not improve on it.",
      .csdg_loss_label(loss), .csdg_num(imp_row$loss), .csdg_num(imp_row$loss_baseline))
  }
  observation = if (prefix) paste0(view$label, ": ", text) else .csdg_sentence(text)
  list(property = NA_character_, criterion = NA_character_, observation = observation,
    relevance = paste("A model description holds whatever the model's accuracy; the improvement provides the scale",
      "of the minimum importance."),
    summary = sub("\\.$", "", observation))
}

# Why margins are open, in words; `what` names the difference ("the margin of a", "the difference").
.csdg_open_notes = function(m, delta, what) {
  notes = character()
  reasons = character()
  for (i in seq_len(nrow(m))) {
    row = m[i]
    if (identical(row$reason, "within_mc")) {
      notes = c(notes, sprintf("%s (%s) is within Monte Carlo error (threshold %s).", .csdg_sentence(what[[i]]),
        .csdg_num(row$estimate), .csdg_num(row$mc_threshold)))
      reasons = c(reasons, sprintf("%s is within Monte Carlo error", what[[i]]))
    } else if (identical(row$reason, "interval") && row$estimate >= 0) {
      notes = c(notes, sprintf(paste("The lower bound %s of the corrected interval of %s is below the smallest",
        "relevant difference %s, so the result is not established."), .csdg_num(row$lower), what[[i]],
        .csdg_num(delta)))
      reasons = c(reasons, sprintf("the lower bound %s of the corrected interval is below %s",
        .csdg_num(row$lower), .csdg_num(delta)))
    } else if (identical(row$reason, "interval")) {
      notes = c(notes, sprintf(paste("%s (%s) is negative, but the upper bound %s of its corrected interval is",
        "above -%s, so the reversal is not established."), .csdg_sentence(what[[i]]), .csdg_num(row$estimate),
        .csdg_num(row$upper), .csdg_num(delta)))
      reasons = c(reasons, sprintf("the reversal is not established (upper bound %s, above -%s)",
        .csdg_num(row$upper), .csdg_num(delta)))
    }
  }
  list(notes = notes, reasons = unique(reasons))
}

.csdg_text_content = function(claim, view, content, criteria, fits, origin) {
  lab = function(x) .csdg_feature_label(claim, x)
  L = view$label
  K = view$K
  m = content$margins
  n_train = .csdg_count(round(mean(fits$n_train)))
  relevance = if (identical(origin, "independently_confirmed")) {
    sprintf(paste("The learner average over refits on %d training sets of %s rows is the quantity the claim names;",
      "the claim and its criteria were fixed before these data were analyzed."), K, n_train)
  } else {
    sprintf(paste("The learner average over refits on %d training sets of %s rows is the quantity the claim names;",
      "selected from this average, the estimates are optimistic (winner's curse)."), K, n_train)
  }
  direction = identical(claim$kind, "direction")
  delta = if (direction) criteria$min_change else view$delta
  what = switch(claim$kind, direction = "the change", order = "the difference",
    sprintf("the margin of %s", lab(m$predictor)))
  open = .csdg_open_notes(m, delta, rep_len(what, nrow(m)))
  interval = .csdg_interval_criterion(criteria, delta, nrow(m))
  out = switch(claim$kind,
    relies_mainly = , top_k = .csdg_text_content_ranking(claim, view, content, interval, lab),
    order = {
      a = claim$first
      b = claim$second
      ci = .csdg_ci(m$estimate, m$lower, m$upper)
      list(
        property = sprintf("On average over refits, %s relies more on %s than on %s.", L, lab(a), lab(b)),
        criterion = sprintf("Average over %d folds: PFI of %s above that of %s, beyond Monte Carlo error; %s", K,
          lab(a), lab(b), sub("^the lower bound of its", "the lower bound of the", interval)),
        observation = sprintf("%s %s versus %s %s; difference (PFI of %s minus PFI of %s) %s.", lab(a),
          .csdg_num(view$avg[[a]]), lab(b), .csdg_num(view$avg[[b]]), lab(a), lab(b), ci),
        summary = sprintf("on average, %s %s versus %s %s (difference %s)", lab(a), .csdg_num(view$avg[[a]]),
          lab(b), .csdg_num(view$avg[[b]]), ci)
      )
    },
    direction = {
      x = lab(claim$feature)
      p1 = .csdg_ordinal(claim$probs[[1L]])
      p2 = .csdg_ordinal(claim$probs[[2L]])
      ci = .csdg_ci(m$estimate, m$lower, m$upper)
      target = if (identical(fits$task_type, "classif")) "predicted probability" else "prediction"
      list(
        property = sprintf("On average over refits, the predictions of %s %s with %s by at least %s between its %s and
          %s percentiles.", L, claim$direction, x, .csdg_crit(criteria$min_change), p1, p2),
        criterion = sprintf("Average over %d folds: the change of the %s between the %s and %s percentiles of %s is
          at least %s in the claimed direction, also with the lower bound of its corrected %s interval; a reversal
          counts only if the upper bound is at most -%s.", K, target, p1, p2, x, .csdg_crit(criteria$min_change),
          .csdg_pct_value(criteria$level), .csdg_crit(criteria$min_change)),
        observation = sprintf("Change in the claimed direction (%s) %s on %s between the %s and %s percentiles of %s;
          minimum change %s.", claim$direction, ci, .csdg_scale_label(fits$task_type), p1, p2, x,
          .csdg_crit(criteria$min_change)),
        summary = sprintf("on average, the change in the claimed direction (%s) between the %s and %s percentiles of
          %s is %s", claim$direction, p1, p2, x, ci)
      )
    }
  )
  out = .csdg_squash(out)
  out$observation = paste(c(out$observation, open$notes), collapse = " ")
  if (!identical(content$status, "supported")) {
    criterion_short = switch(claim$kind,
      relies_mainly = sprintf("criterion: at least %s times", format(claim$factor)),
      top_k = "criterion: larger than every other predictor",
      order = "criterion: a positive difference",
      direction = sprintf("criterion: at least %s", .csdg_crit(criteria$min_change))
    )
    out$summary = paste(c(out$summary, criterion_short, open$reasons), collapse = "; ")
  }
  out$relevance = relevance
  out
}

.csdg_text_content_ranking = function(claim, view, content, interval, lab) {
  S = claim$predictors
  n = length(S)
  L = view$label
  K = view$K
  m = content$margins
  lo = content$largest_other
  f = format(claim$factor)
  relies = identical(claim$kind, "relies_mainly")
  values = .csdg_and(paste(lab(S), .csdg_num(view$avg[S])))
  ratios = content$ratios
  ratio_ok = all(is.finite(ratios))
  ratio_text = if (ratio_ok) sprintf(" (%s times)", .csdg_and(.csdg_ratio(ratios))) else ""
  definition = if (relies) sprintf("PFI minus %s x PFI of %s", f, lab(lo)) else sprintf("PFI minus PFI of %s",
    lab(lo))
  margins = .csdg_and(.csdg_ci(m$estimate, m$lower, m$upper))
  per_fold = content$margins_per_fold_max
  same = isTRUE(all.equal(per_fold$estimate, m$estimate, tolerance = 1e-12)) &&
    isTRUE(all.equal(per_fold$lower, m$lower, tolerance = 1e-12))
  fold_text = if (same) "" else sprintf(" Against the largest other predictor of each fold, which can differ between
    folds: %s.", .csdg_and(.csdg_ci(per_fold$estimate, per_fold$lower, per_fold$upper)))
  each = if (n == 1L) sprintf("%s has", lab(S)) else sprintf("each of %s has", .csdg_and(lab(S)))
  if (relies) {
    property = sprintf("On average over refits, %s relies mainly on %s: %s at least %s times the PFI of every other
      predictor.", L, .csdg_and(lab(S)), if (n == 1L) "it has" else "each has", f)
    criterion = sprintf("Average over %d folds: %s at least %s times the PFI of every other predictor, that is, a
      margin (PFI minus %s x PFI of the largest other predictor) of at least 0 beyond Monte Carlo error; %s", K,
      each, f, f, interval)
    summary = if (ratio_ok) {
      sprintf("on average, %s %s %s times the PFI of the largest other predictor, %s", .csdg_and(lab(S)),
        .csdg_verb(n, "has", "have"), .csdg_and(.csdg_ratio(ratios)), lab(lo))
    } else {
      sprintf("on average, %s %s margins of %s over the largest other predictor, %s", .csdg_and(lab(S)),
        .csdg_verb(n, "has", "have"), .csdg_and(.csdg_num(m$estimate)), lab(lo))
    }
  } else {
    property = if (n == 1L) {
      sprintf("On average over refits, %s relies most on %s.", L, lab(S))
    } else {
      sprintf("On average over refits, the %s predictors with the largest PFI of %s are %s.", .csdg_number_word(n),
        L, .csdg_and(lab(S)))
    }
    criterion = sprintf("Average over %d folds: %s %s larger PFI than every other predictor, that is, a margin (PFI
      minus PFI of the largest other predictor) above 0 beyond Monte Carlo error; %s", K, .csdg_and(lab(S)),
      .csdg_verb(n, "has", "have"), interval)
    summary = sprintf("on average, the PFI of %s %s that of the largest other predictor, %s, by %s",
      .csdg_and(lab(S)), .csdg_verb(n, "exceeds", "exceed"), lab(lo), .csdg_and(.csdg_num(m$estimate)))
  }
  list(
    property = property,
    criterion = criterion,
    observation = sprintf("%s versus %s for %s, the largest other predictor%s; margin (%s) %s.%s", values,
      .csdg_num(view$avg[[lo]]), lab(lo), ratio_text, definition, margins, fold_text),
    summary = summary
  )
}

.csdg_text_minimum = function(claim, view, mi, criteria, loss) {
  lab = function(x) .csdg_feature_label(claim, x)
  tab = mi$table
  n = nrow(tab)
  L = view$label
  named = .csdg_and(lab(tab$predictor))
  pct = .csdg_pct_value(criteria$min_importance)
  imp_row = view$improvement
  observation = sprintf("%s of the improvement of %s (%s to %s).",
    .csdg_and(sprintf("%s %s (%s)", lab(tab$predictor), .csdg_num(tab$mean_importance), .csdg_pct(tab$share))),
    .csdg_num(view$I), .csdg_num(imp_row$loss_baseline), .csdg_num(imp_row$loss))
  summary = sprintf("%s %s %s of the improvement (minimum %s)", named, .csdg_verb(n, "has", "have"),
    .csdg_and(.csdg_pct(tab$share)), pct)
  if (mi$no_scale) {
    observation = sprintf(paste("%s does not improve on the model without predictors (held-out %s %s versus %s), so",
      "the minimum importance has no scale."), .csdg_sentence(L), .csdg_loss_label(loss), .csdg_num(imp_row$loss),
      .csdg_num(imp_row$loss_baseline))
    summary = sprintf("%s does not improve on the model without predictors, so the minimum importance has no scale",
      L)
  } else if (any(tab$status != "supported")) {
    within = tab[!tab$beyond_mc]
    if (nrow(within)) {
      observation = paste(observation, sprintf("The mean PFI of %s %s within Monte Carlo error of the minimum
        importance %s.", .csdg_and(lab(within$predictor)), .csdg_verb(nrow(within), "is", "are"),
        .csdg_num(view$tau)))
    }
    short = tab[tab$beyond_mc & tab$status == "open"]
    if (nrow(short)) {
      observation = paste(observation, sprintf(paste("The mean PFI of %s %s below the minimum importance %s, but",
        "the upper %s of the corrected interval (%s) %s it, so the shortfall is not established."),
        .csdg_and(lab(short$predictor)), .csdg_verb(nrow(short), "is", "are"), .csdg_num(view$tau),
        .csdg_verb(nrow(short), "bound", "bounds"), .csdg_and(.csdg_num(short$upper)),
        .csdg_verb(nrow(short), "reaches", "reach")))
    }
    below = tab[tab$status == "contradicted"]
    if (nrow(below)) {
      observation = paste(observation, sprintf("The mean PFI of %s %s below the minimum importance %s beyond Monte
        Carlo error.", .csdg_and(lab(below$predictor)), .csdg_verb(nrow(below), "is", "are"), .csdg_num(view$tau)))
    }
    failing = tab[tab$status != "supported"]
    summary = sprintf("%s %s %s of the improvement, %s the minimum of %s (%s)", .csdg_and(lab(failing$predictor)),
      .csdg_verb(nrow(failing), "has", "have"), .csdg_and(.csdg_pct(failing$share)),
      if (any(failing$status == "contradicted")) "below" else "not clearly above", pct, .csdg_num(view$tau))
  }
  .csdg_squash(list(
    property = sprintf("%s the accuracy of %s.", if (n == 1L) paste(named, "matters relative to") else
      paste("Each of", named, "matters relative to"), L),
    criterion = sprintf("%s a mean PFI of at least %s of the improvement of %s over the model without predictors
      (%s %s), beyond Monte Carlo error; a shortfall counts only if the upper bound of the corrected interval is
      below it.", if (n == 1L) paste(named, "has") else paste("Each of", named, "has"), pct, L, .csdg_num(view$tau),
      .csdg_loss_units(loss)),
    observation = observation,
    relevance = "Ratios and ranks are scale-free; the minimum importance excludes negligible predictors.",
    summary = summary
  ))
}

.csdg_text_stability = function(claim, view, st, criteria, origin) {
  lab = function(x) .csdg_feature_label(claim, x)
  L = view$label
  K = st$K
  need = st$need
  holm = if (need >= K) " (Holm-adjusted)" else ""
  tail_rule = sprintf("a failing fold counts only beyond Monte Carlo error%s and by at least the smallest relevant
    difference (%s).", holm, .csdg_num(view$delta))
  rest = character()
  if (st$n_undecided) {
    rest = c(rest, sprintf("%d %s within Monte Carlo error or below the smallest relevant difference",
      st$n_undecided, .csdg_verb(st$n_undecided, "other is", "others are")))
  }
  if (st$n_fails) {
    rest = c(rest, sprintf("%d %s beyond Monte Carlo error", st$n_fails, .csdg_verb(st$n_fails, "fails", "fail")))
  }
  neighbor_text = if (nrow(st$neighbors)) {
    sprintf(" (%s)", paste(sprintf("cutoff %d: %d of %d", st$neighbors$cutoff, st$neighbors$folds,
      st$neighbors$K), collapse = "; "))
  } else {
    ""
  }
  rest_text = if (length(rest)) paste0("; ", paste(rest, collapse = "; ")) else ""
  kind = claim$kind
  S = claim$predictors
  n = length(S)
  leading = if (n == 1L) "the leading predictor" else sprintf("the %s leading predictors", .csdg_number_word(n))
  out = switch(kind,
    relies_mainly = , top_k = list(
      property = sprintf("The result recurs when %s is refitted on other folds, with the selection repeated.", L),
      criterion = sprintf("Selected anew in each fold, %s %s %s%s in at least %d of %d folds; %s",
        .csdg_and(lab(S)), .csdg_verb(n, "is", "are"), leading, if (identical(kind, "relies_mainly")) {
          sprintf(" with at least %s times the PFI of every other predictor", format(claim$factor))
        } else "", need, K, tail_rule),
      observation = sprintf("Selected anew in each fold: %s reproduce the result%s%s.", .csdg_folds(st$n_holds, K),
        rest_text, neighbor_text),
      summary = sprintf("the result recurs in %s%s", .csdg_folds(st$n_holds, K), rest_text)
    ),
    order = list(
      property = sprintf("The ordering recurs when %s is refitted on other folds.", L),
      criterion = sprintf("%s has the larger PFI in at least %d of %d folds; %s", lab(claim$first), need, K,
        tail_rule),
      observation = sprintf("%s exceeds %s in %s%s.", lab(claim$first), lab(claim$second),
        .csdg_folds(st$n_holds, K), rest_text),
      summary = sprintf("%s exceeds %s in %s%s", lab(claim$first), lab(claim$second), .csdg_folds(st$n_holds, K),
        rest_text)
    ),
    direction = list(
      property = sprintf("The direction recurs when %s is refitted on other folds.", L),
      criterion = sprintf("The change is at least %s in the claimed direction in at least %d of %d folds.",
        .csdg_crit(criteria$min_change), need, K),
      observation = sprintf("The change in the claimed direction is at least %s in %s.",
        .csdg_crit(criteria$min_change), .csdg_folds(st$n_holds, K)),
      summary = sprintf("the change in the claimed direction is at least %s in %s", .csdg_crit(criteria$min_change),
        .csdg_folds(st$n_holds, K))
    )
  )
  quantity = if (identical(kind, "direction")) "ALE" else "PFI"
  out$relevance = if (identical(origin, "independently_confirmed")) {
    sprintf(paste("Refits on other folds and new permutations estimate the same learner %s again; the claim was",
      "fixed before these data were analyzed."), quantity)
  } else {
    sprintf(paste("Refits on other folds and new permutations estimate the same learner %s again; on the data that",
      "suggested the claim, this share remains optimistic."), quantity)
  }
  lapply(out, function(text) gsub("\\s+", " ", text))
}

.csdg_text_procedure = function(claim, imp, view, proc) {
  lab = function(x) .csdg_feature_label(claim, x)
  direction = identical(claim$kind, "direction")
  base = if (direction) {
    sprintf("ALE of %s on %d quantile intervals of each held-out fold, as the scope names; ALE stays within the data
      intervals.", lab(claim$feature), claim$intervals)
  } else {
    sprintf("Held-out marginal PFI with %s, %d permutations per fold in %d folds, as the scope names.",
      .csdg_loss_label(imp$loss), imp$repetitions, view$K)
  }
  comparisons = proc$comparisons
  unit_text = function(unit) {
    if (unit %in% names(imp$groups)) sprintf("%s (%s)", unit, paste(imp$groups[[unit]], collapse = ", ")) else
      lab(unit)
  }
  qualifier = function(row) {
    switch(row$outcome,
      inconclusive = if (!is.na(row$threshold) && abs(row$estimate) <= row$threshold) ", within Monte Carlo error"
        else ", not established by the corrected interval",
      unchanged = ", as under marginal permutation",
      "")
  }
  clause = function(i) {
    row = comparisons[i]
    if (identical(row$comparison, "minimum_importance")) {
      return(sprintf("%s, %s adds %s (%s the minimum importance %s%s)", .csdg_conditional_text(imp, row$predictor),
        lab(row$predictor), .csdg_num(row$value), if (row$value >= row$versus_value) "above" else "below",
        .csdg_num(row$versus_value), qualifier(row)))
    }
    if (identical(claim$kind, "order")) {
      verdict = switch(row$outcome, holds = "the order holds", reversed = "the order reverses",
        inconclusive = "the order is not established", unchanged = "as under marginal permutation")
      return(sprintf("conditional PFI of %s and %s is %s and %s (%s%s)", lab(row$predictor), lab(row$versus),
        .csdg_num(row$value), .csdg_num(row$versus_value), verdict,
        if (identical(row$outcome, "inconclusive")) qualifier(row) else ""))
    }
    verdict = switch(row$outcome, holds = "the result holds", reversed = "the result does not hold",
      inconclusive = "the result is not established", unchanged = "the result fails as under marginal permutation")
    sprintf("with %s PFI, %s %s versus %s for %s, the largest other %s (%s%s)", row$perturbation, lab(row$predictor),
      .csdg_num(row$value), .csdg_num(row$versus_value), unit_text(row$versus),
      if (identical(row$perturbation, "grouped")) "unit" else "predictor", verdict,
      if (identical(row$outcome, "inconclusive")) qualifier(row) else "")
  }
  clauses = vapply(seq_len(nrow(comparisons)), clause, character(1L))
  outcomes = comparisons$outcome %||% character()
  reversed = any(outcomes == "reversed")
  inconclusive = any(outcomes == "inconclusive")
  unchanged = any(outcomes == "unchanged")
  what = if (identical(claim$kind, "order")) "the order" else "the result"
  conclusion = if (reversed && isTRUE(claim$marginal_only)) {
    "The wording names the marginal permutation, so the reversal shows what the qualifier is for."
  } else if (reversed) {
    sprintf("%s is supported only under marginal permutation, which the wording does not name.",
      .csdg_sentence(what))
  } else if (inconclusive && isTRUE(claim$marginal_only)) {
    "The wording names the marginal permutation, so the other perturbations are context."
  } else if (inconclusive) {
    sprintf("Whether %s holds under the other perturbation is not established.", what)
  } else if (length(clauses) && all(outcomes == "unchanged")) {
    "The marginal result itself does not hold, so the perturbation does not bear on the wording."
  } else if (length(clauses)) {
    "The result also holds under these perturbations."
  } else if (!direction && !length(proc$context)) {
    "No grouped or conditional comparison was requested."
  } else {
    character()
  }
  if (unchanged && !all(outcomes == "unchanged")) {
    conclusion = c(conclusion, paste("A comparison that fails as under marginal permutation does not bear on the",
      "wording."))
  }
  observation = paste(c(base, if (length(clauses)) paste0(.csdg_sentence(paste(clauses, collapse = "; ")), "."),
    conclusion, if (length(proc$context)) paste0(.csdg_sentence(paste(proc$context, collapse = "; ")), ".")),
    collapse = " ")
  decisive = if (reversed) clauses[outcomes == "reversed"] else clauses[outcomes == "inconclusive"]
  summary = if (length(decisive)) {
    paste(c(decisive, if (reversed && !isTRUE(claim$marginal_only)) {
      sprintf("%s is supported only under marginal permutation", what)
    } else if (isTRUE(claim$marginal_only)) "the wording names the marginal permutation"), collapse = "; ")
  } else if (length(clauses) && all(outcomes == "unchanged")) {
    "the perturbation does not bear on the wording, because the marginal result does not hold"
  } else if (length(clauses)) {
    "the result also holds under grouped or conditional permutation"
  } else if (direction) {
    "ALE within the data intervals, as the scope names"
  } else {
    "held-out marginal PFI as the scope names; no grouped or conditional comparison was requested"
  }
  .csdg_squash(list(
    property = "The procedure computes the named quantity, and the wording is not broader than the scope.",
    criterion = if (direction) {
      "ALE on quantile intervals of the held-out fold, as named in the scope."
    } else {
      paste("Held-out marginal PFI as named in the scope; if grouped or conditional PFI reverses the result",
        "(beyond Monte Carlo error and the corrected interval), the wording names the marginal permutation.")
    },
    observation = observation,
    relevance = paste("Marginal permutation includes input combinations absent from the data; conditional and grouped",
      "PFI measure reliance on information a predictor does not share and on a group."),
    summary = summary
  ))
}

.csdg_text_correlation = function(claim, pairs, criteria, threat) {
  lab = function(x) .csdg_feature_label(claim, x)
  text = paste(sprintf("%s and %s correlate r = %s", lab(pairs$predictor), lab(pairs$partner), .csdg_r(pairs$r)),
    collapse = "; ")
  text = paste(if (nrow(pairs) == 1L) "the predictors" else "the predictor pairs", text)
  advice = "conditional = list(...) or groups = list(...) in csdg_importance() shows whether this changes the result"
  observation = if (threat) {
    sprintf(paste("%s, and no grouped or conditional PFI was computed for %s; marginal PFI can be redistributed",
      "between correlated predictors, so the wording may be broader than marginal PFI shows (%s)."), .csdg_sentence(text),
      if (nrow(pairs) == 1L) "them" else "these predictors", advice)
  } else {
    sprintf("%s; the wording names the marginal permutation, so the correlation is context.", .csdg_sentence(text))
  }
  list(
    property = if (threat) "No unquantified correlation makes the wording broader than marginal PFI." else
      NA_character_,
    criterion = if (threat) sprintf(paste("No named predictor correlates at least %s (absolute value) with another",
      "predictor without grouped or conditional PFI."), .csdg_crit(criteria$max_cor)) else NA_character_,
    observation = observation,
    relevance = "A strong correlation can redistribute marginal PFI between predictors.",
    summary = if (threat) sprintf("%s, and no grouped or conditional PFI was computed", text) else text,
    source = sprintf("correlation cutoff %s: %s", .csdg_crit(criteria$max_cor), criteria$sources$max_cor)
  )
}

.csdg_text_models_context = function(claim, view, content, mi) {
  lab = function(x) .csdg_feature_label(claim, x)
  statuses = c(content$status, if (!is.null(mi)) mi$status)
  verdict = if (all(statuses == "supported")) {
    "the result also holds"
  } else if (any(statuses == "contradicted")) {
    "the result does not hold"
  } else {
    "the result is not established"
  }
  m = content$margins
  numbers = switch(claim$kind,
    relies_mainly = , top_k = sprintf("%s versus %s for %s", .csdg_and(paste(lab(claim$predictors),
      .csdg_num(view$avg[claim$predictors]))), .csdg_num(view$avg[[content$largest_other]]),
      lab(content$largest_other)),
    order = sprintf("%s %s versus %s %s", lab(claim$first), .csdg_num(view$avg[[claim$first]]), lab(claim$second),
      .csdg_num(view$avg[[claim$second]])),
    direction = sprintf("change %s", .csdg_ci(m$estimate, m$lower, m$upper))
  )
  text = sprintf("%s: %s (%s)", view$label, verdict, numbers)
  list(property = NA_character_, criterion = NA_character_, observation = paste0(text, "."),
    relevance = "Another learner computes another quantity; the claim does not cover it.", summary = text)
}

utils::globalVariables(c(
  # Columns of the analysis-first interface
  "loss_baseline", "improvement", "relative_improvement", "folds_improved", "learner_order__", "type_order__",
  "rank_in_fold", "relative_to_improvement", "positive_folds", "sd_importance", "n_permutations", "holds", "beyond",
  "class", "d", "n_test", "K", "key__", "value__", "label__", "learner_label__", "group__", "ale", "change",
  "mean_change", "feature", "status", "learner", "iteration", "gate", "check", "role"
))
