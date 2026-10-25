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

# A criterion value as supplied, for example 0.02 (not 0.0200); formatC() pads short values such as 0.3 with spaces.
.csdg_crit = function(x) {
  trimws(formatC(signif(x, 3L), digits = 3L, format = "fg"))
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

# A cutoff between 0 and 1 without the leading zero, with two decimals unless it needs three: 0.5 -> ".50".
.csdg_cutoff = function(x) {
  digits = if (abs(100 * x - round(100 * x)) < 1e-9) 2L else 3L
  sub("^0\\.", ".", sprintf("%.*f", digits, x))
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

# Quantity of an importance result: "pfi" for csdg_importance(), "loco" for csdg_loco(); results of versions before
# 0.3.0 carry no quantity and are PFI.
.csdg_quantity = function(imp) imp$quantity %||% "pfi"

.csdg_is_loco = function(imp) identical(.csdg_quantity(imp), "loco")

.csdg_quantity_label = function(imp) if (.csdg_is_loco(imp)) "LOCO" else "PFI"

# Type of the single-predictor values in `fold_pfi` and `summary`.
.csdg_single_type = function(imp) if (.csdg_is_loco(imp)) "loco" else "marginal"

# A direction claim states that the predictions rise or fall (`monotone = TRUE`, the default) or only that they differ
# between the two percentiles. Claims of earlier versions have no `monotone` field; their wording states a rise or fall.
.csdg_is_monotone = function(claim) !identical(claim$monotone, FALSE)

# Word of the claimed direction: "rise" or "fall", or "higher" or "lower" for a claim about the two percentiles.
.csdg_direction_word = function(claim) {
  if (.csdg_is_monotone(claim)) claim$direction else if (identical(claim$direction, "rise")) "higher" else "lower"
}

# Change against the claimed direction between adjacent grid points: a decrease for "rise", an increase for "fall".
.csdg_reversal_word = function(claim, article = FALSE) {
  fall = identical(claim$direction, "fall")
  word = if (fall) "increase" else "decrease"
  if (article) paste(if (fall) "an" else "a", word) else word
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

# The reason why G0b is open until the researcher judges it (see ?csdg_check and ?csdg_judge).
.csdg_g0b_reason = "measurement and data not yet judged; use csdg_judge(\"G0b\", ...)"

.csdg_text_measurement = function(claim, fits) {
  named = c(claim$predictors, claim$first, claim$second, claim$feature)
  constructs = identical(claim$refers_to, "constructs")
  variables = sprintf("%s %s and the outcome %s of task %s (%s rows)", .csdg_verb(length(named), "variable",
    "variables"), .csdg_and(named), fits$target, fits$task_id, .csdg_count(fits$n))
  list(
    property = "The data cover what the claim names.",
    criterion = if (constructs) {
      sprintf(paste("Judged by the researcher: validity evidence for each construct the claim names (for example",
        "reliability and measurement invariance), and how the %s were measured, coded, and sampled."), variables)
    } else {
      sprintf(paste("Judged by the researcher: how the %s were measured, coded, and sampled, so that they cover what",
        "the claim names."), variables)
    },
    observation = paste0(.csdg_sentence(.csdg_g0b_reason), ".", if (constructs) {
      " The claim names constructs, so the judgment needs validity evidence for them."
    } else {
      ""
    }),
    relevance = paste("The package computes the explanation but cannot see how the data were collected: a variable",
      "needs documentation of how it was measured and coded, and a construct also needs validity evidence."),
    summary = if (constructs) {
      paste0(.csdg_g0b_reason, "; the claim names constructs, which need validity evidence")
    } else {
      .csdg_g0b_reason
    }
  )
}

.csdg_text_performance = function(view, loss, prefix) {
  imp_row = view$improvement
  text = if (imp_row$improvement > 0) {
    sprintf(paste("held-out %s %s versus %s for the model without predictors (improvement %s, a reduction by %s);",
      "%s improve."),
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

# Why margins are open, in words; `what` names the difference ("the margin of a over c", "the difference"), and
# `threshold` names `delta` ("the smallest relevant difference", or "the minimum change" for direction claims). With
# `name_reasons = TRUE`, the short reasons also name the difference, for claims with several comparisons.
.csdg_open_notes = function(m, delta, what, threshold = "the smallest relevant difference", name_reasons = FALSE) {
  notes = character()
  reasons = character()
  for (i in seq_len(nrow(m))) {
    row = m[i]
    if (identical(row$reason, "within_mc")) {
      notes = c(notes, sprintf("%s (%s) is within Monte Carlo error (threshold %s).", .csdg_sentence(what[[i]]),
        .csdg_num(row$estimate), .csdg_num(row$mc_threshold)))
      reasons = c(reasons, sprintf("%s is within Monte Carlo error", what[[i]]))
    } else if (identical(row$reason, "interval") && row$estimate >= 0) {
      notes = c(notes, sprintf("%s is inconclusive: the lower bound %s of its corrected interval is below %s %s.",
        .csdg_sentence(what[[i]]), .csdg_num(row$lower), threshold, .csdg_num(delta)))
      reasons = c(reasons, if (name_reasons) {
        sprintf("%s is inconclusive (lower bound %s of its corrected interval, below %s)", what[[i]],
          .csdg_num(row$lower), .csdg_num(delta))
      } else {
        sprintf("the lower bound %s of the corrected interval is below %s", .csdg_num(row$lower), .csdg_num(delta))
      })
    } else if (identical(row$reason, "interval")) {
      notes = c(notes, sprintf(paste("%s (%s) is negative but inconclusive: the upper bound %s of its corrected",
        "interval is above -%s."), .csdg_sentence(what[[i]]), .csdg_num(row$estimate), .csdg_num(row$upper),
        .csdg_num(delta)))
      reasons = c(reasons, if (name_reasons) {
        sprintf("%s is negative but inconclusive (upper bound %s of its corrected interval, above -%s)", what[[i]],
          .csdg_num(row$upper), .csdg_num(delta))
      } else {
        sprintf("the reversal is inconclusive (upper bound %s, above -%s)", .csdg_num(row$upper), .csdg_num(delta))
      })
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
  ranking = claim$kind %in% c("relies_mainly", "top_k")
  delta = if (direction) criteria$min_change else view$delta
  # For a ranking claim, `m` holds the weakest comparison of each named predictor with another predictor.
  what = switch(claim$kind, direction = "the change", order = "the difference",
    sprintf("the margin of %s over %s", lab(m$predictor), lab(m$versus)))
  open = .csdg_open_notes(m, delta, rep_len(what, nrow(m)),
    threshold = if (direction) "the minimum change" else "the smallest relevant difference", name_reasons = ranking)
  interval = .csdg_interval_criterion(criteria, delta, nrow(content$comparisons %||% m))
  q = view$q %||% "PFI"
  out = switch(claim$kind,
    relies_mainly = , top_k = .csdg_text_content_ranking(claim, view, content, interval, lab),
    order = {
      a = claim$first
      b = claim$second
      ci = .csdg_ci(m$estimate, m$lower, m$upper)
      list(
        property = sprintf("On average over refits, %s relies more on %s than on %s%s.", L, lab(a), lab(b),
          if (identical(q, "LOCO")) " (LOCO)" else ""),
        criterion = sprintf("Average over %d folds: %s of %s above that of %s%s; %s", K, q, lab(a), lab(b),
          if (isTRUE(view$mc)) ", beyond Monte Carlo error" else "",
          sub("^the lower bound of its", "the lower bound of the", interval)),
        observation = sprintf("%s %s versus %s %s; difference (%s of %s minus %s of %s) %s.", lab(a),
          .csdg_num(view$avg[[a]]), lab(b), .csdg_num(view$avg[[b]]), q, lab(a), q, lab(b), ci),
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
      word = .csdg_direction_word(claim)
      monotone = .csdg_is_monotone(claim)
      minimum = .csdg_crit(criteria$min_change)
      list(
        property = if (monotone) {
          sprintf("On average over refits, the predictions of %s %s with %s by at least %s between its %s and %s
            percentiles.", L, word, x, minimum, p1, p2)
        } else {
          sprintf("On average over refits, the predictions of %s are %s at the %s than at the %s percentile of %s, by
            at least %s.", L, word, p2, p1, x, minimum)
        },
        criterion = sprintf("Average over %d folds: the change of the %s between the %s and %s percentiles of %s is
          at least %s in the claimed direction, also with the lower bound of its corrected %s interval; a reversal
          counts only if the upper bound is at most -%s.%s", K, target, p1, p2, x, minimum,
          .csdg_pct_value(criteria$level), minimum, if (monotone) {
            sprintf(" Whether %s between adjacent grid points exceeds %s is checked in each fold (stability, G5).",
              .csdg_reversal_word(claim, article = TRUE), minimum)
          } else {
            ""
          }),
        observation = sprintf("Change in the claimed direction (%s) %s on %s between the %s and %s percentiles of %s;
          minimum change %s.", word, ci, .csdg_scale_label(fits$task_type), p1, p2, x, minimum),
        summary = sprintf("on average, the change in the claimed direction (%s) between the %s and %s percentiles of
          %s is %s", word, p1, p2, x, ci)
      )
    }
  )
  out = .csdg_squash(out)
  out$observation = paste(c(out$observation, open$notes), collapse = " ")
  if (!identical(content$status, "supported")) {
    criterion_short = switch(claim$kind,
      relies_mainly = sprintf("criterion: at least %s times every other predictor", format(claim$factor)),
      top_k = "criterion: larger than every other predictor",
      order = "criterion: a positive difference",
      direction = sprintf("criterion: at least %s", .csdg_crit(criteria$min_change))
    )
    out$summary = paste(c(out$summary, criterion_short, open$reasons), collapse = "; ")
  }
  out$relevance = relevance
  out
}

# A comparison of a ranking claim: "b minus 2 x d" for a claim that relies mainly on b, "b minus d" otherwise.
.csdg_comparison_label = function(claim, predictor, versus) {
  lab = function(x) .csdg_feature_label(claim, x)
  if (identical(claim$kind, "relies_mainly")) {
    sprintf("%s minus %s x %s", lab(predictor), format(claim$factor), lab(versus))
  } else {
    sprintf("%s minus %s", lab(predictor), lab(versus))
  }
}

.csdg_text_content_ranking = function(claim, view, content, interval, lab) {
  S = claim$predictors
  n = length(S)
  L = view$label
  K = view$K
  lo = content$largest_other
  comparisons = content$comparisons %||% content$margins
  # The margins against the other predictor with the largest average, in the order of the named predictors.
  m = comparisons[comparisons$versus == lo]
  m = m[match(S, m$predictor)]
  n_compared = nrow(comparisons)
  f = format(claim$factor)
  relies = identical(claim$kind, "relies_mainly")
  q = view$q %||% "PFI"
  beyond = if (isTRUE(view$mc)) " beyond Monte Carlo error" else ""
  values = .csdg_and(paste(lab(S), .csdg_num(view$avg[S])))
  ratios = content$ratios
  ratio_ok = all(is.finite(ratios))
  ratio_text = if (ratio_ok) sprintf(" (%s times)", .csdg_and(.csdg_ratio(ratios))) else ""
  definition = if (relies) sprintf("%s minus %s x %s of %s", q, f, q, lab(lo)) else sprintf("%s minus %s of %s", q,
    q, lab(lo))
  margins = .csdg_and(.csdg_ci(m$estimate, m$lower, m$upper))
  # With several other predictors, the weakest of all comparisons and the number of comparisons that are not
  # supported.
  weakest_text = if (length(unique(comparisons$versus)) > 1L) {
    w = .csdg_weakest(comparisons)
    counts = vapply(c("contradicted", "open"), function(status) sum(comparisons$status == status), integer(1L))
    counts = counts[counts > 0L]
    sprintf("; weakest of the %d comparisons with other predictors%s: %s, %s", n_compared,
      if (length(counts)) sprintf(" (%s)", .csdg_and(paste(counts, names(counts)))) else "",
      .csdg_comparison_label(claim, w$predictor, w$versus), .csdg_ci(w$estimate, w$lower, w$upper))
  } else {
    ""
  }
  per_fold = content$margins_per_fold_max
  same = isTRUE(all.equal(per_fold$estimate, m$estimate, tolerance = 1e-12)) &&
    isTRUE(all.equal(per_fold$lower, m$lower, tolerance = 1e-12))
  fold_text = if (same) "" else sprintf(" Against the largest other predictor of each fold, which can differ between
    folds: %s.", .csdg_and(.csdg_ci(per_fold$estimate, per_fold$lower, per_fold$upper)))
  each = if (n == 1L) sprintf("%s has", lab(S)) else sprintf("each of %s has", .csdg_and(lab(S)))
  every = if (n_compared == 1L) "in the comparison with the other predictor" else
    sprintf("in each of the %d comparisons with other predictors", n_compared)
  if (relies) {
    property = sprintf("On average over refits, %s relies mainly on %s: %s at least %s times the %s of every other
      predictor.", L, .csdg_and(lab(S)), if (n == 1L) "it has" else "each has", f, q)
    criterion = sprintf("Average over %d folds: %s at least %s times the %s of every other predictor, that is, a
      margin (%s minus %s x %s of the other predictor) of at least 0%s %s; %s", K, each, f, q, q, f, q, beyond, every,
      interval)
    summary = if (ratio_ok) {
      sprintf("on average, %s %s %s times the %s of the largest other predictor, %s", .csdg_and(lab(S)),
        .csdg_verb(n, "has", "have"), .csdg_and(.csdg_ratio(ratios)), q, lab(lo))
    } else {
      sprintf("on average, %s %s margins of %s over the largest other predictor, %s", .csdg_and(lab(S)),
        .csdg_verb(n, "has", "have"), .csdg_and(.csdg_num(m$estimate)), lab(lo))
    }
  } else {
    property = if (n == 1L) {
      sprintf("On average over refits, %s relies most on %s%s.", L, lab(S), if (identical(q, "LOCO")) " (LOCO)" else
        "")
    } else {
      sprintf("On average over refits, the %s predictors with the largest %s of %s are %s.", .csdg_number_word(n), q,
        L, .csdg_and(lab(S)))
    }
    criterion = sprintf("Average over %d folds: %s %s larger %s than every other predictor, that is, a margin (%s
      minus %s of the other predictor) above 0%s %s; %s", K, .csdg_and(lab(S)), .csdg_verb(n, "has", "have"), q,
      q, q, beyond, every, interval)
    summary = sprintf("on average, the %s of %s %s that of the largest other predictor, %s, by %s", q,
      .csdg_and(lab(S)), .csdg_verb(n, "exceeds", "exceed"), lab(lo), .csdg_and(.csdg_num(m$estimate)))
  }
  list(
    property = property,
    criterion = criterion,
    observation = sprintf("%s versus %s for %s, the largest other predictor%s; margin (%s) %s%s.%s", values,
      .csdg_num(view$avg[[lo]]), lab(lo), ratio_text, definition, margins, weakest_text, fold_text),
    summary = summary
  )
}

.csdg_text_minimum = function(claim, view, mi, criteria, loss) {
  lab = function(x) .csdg_feature_label(claim, x)
  q = view$q %||% "PFI"
  tab = mi$table
  n = nrow(tab)
  L = view$label
  named = .csdg_and(lab(tab$predictor))
  pct = .csdg_pct_value(criteria$min_importance)
  imp_row = view$improvement
  # Importance is compared with the minimum in loss units; shares of the improvement are not shown, because PFI
  # values are not parts of the improvement and can exceed it.
  observation = sprintf("%s versus the minimum importance %s (%s of the improvement of %s, from %s to %s).",
    .csdg_and(sprintf("%s %s", lab(tab$predictor), .csdg_num(tab$mean_importance))), .csdg_num(view$tau), pct,
    .csdg_num(view$I), .csdg_num(imp_row$loss_baseline), .csdg_num(imp_row$loss))
  summary = sprintf("%s %s %s, above the minimum importance %s (%s of the improvement)", named,
    .csdg_verb(n, "has", "have"), .csdg_and(.csdg_num(tab$mean_importance)), .csdg_num(view$tau), pct)
  if (mi$no_scale) {
    observation = sprintf(paste("%s does not improve on the model without predictors (held-out %s %s versus %s), so",
      "the minimum importance has no scale."), .csdg_sentence(L), .csdg_loss_label(loss), .csdg_num(imp_row$loss),
      .csdg_num(imp_row$loss_baseline))
    summary = sprintf("%s does not improve on the model without predictors, so the minimum importance has no scale",
      L)
  } else if (any(tab$status != "supported")) {
    within = tab[!tab$beyond_mc]
    if (nrow(within)) {
      observation = paste(observation, sprintf("The mean %s of %s %s within Monte Carlo error of the minimum
        importance %s.", q, .csdg_and(lab(within$predictor)), .csdg_verb(nrow(within), "is", "are"),
        .csdg_num(view$tau)))
    }
    short = tab[tab$beyond_mc & tab$status == "open"]
    if (nrow(short)) {
      observation = paste(observation, sprintf(paste("The mean %s of %s %s below the minimum importance %s, but",
        "the upper %s of the corrected interval (%s) %s it, so the shortfall does not exceed its uncertainty."), q,
        .csdg_and(lab(short$predictor)), .csdg_verb(nrow(short), "is", "are"), .csdg_num(view$tau),
        .csdg_verb(nrow(short), "bound", "bounds"), .csdg_and(.csdg_num(short$upper)),
        .csdg_verb(nrow(short), "reaches", "reach")))
    }
    below = tab[tab$status == "contradicted"]
    if (nrow(below)) {
      observation = paste(observation, sprintf("The mean %s of %s %s below the minimum importance %s%s.", q,
        .csdg_and(lab(below$predictor)), .csdg_verb(nrow(below), "is", "are"), .csdg_num(view$tau),
        if (isTRUE(view$mc)) " beyond Monte Carlo error" else ""))
    }
    failing = tab[tab$status != "supported"]
    summary = sprintf("%s %s %s, %s the minimum importance %s (%s of the improvement)",
      .csdg_and(lab(failing$predictor)), .csdg_verb(nrow(failing), "has", "have"),
      .csdg_and(.csdg_num(failing$mean_importance)),
      if (any(failing$status == "contradicted")) "below" else "not clearly above", .csdg_num(view$tau), pct)
  }
  .csdg_squash(list(
    property = sprintf("%s the accuracy of %s.", if (n == 1L) paste(named, "matters relative to") else
      paste("Each of", named, "matters relative to"), L),
    criterion = sprintf("%s a mean %s of at least %s of the improvement of %s over the model without predictors
      (%s %s)%s; a shortfall counts only if the upper bound of the corrected interval is below it.",
      if (n == 1L) paste(named, "has") else paste("Each of", named, "has"), q, pct, L, .csdg_num(view$tau),
      .csdg_loss_units(loss), if (isTRUE(view$mc)) ", beyond Monte Carlo error" else ""),
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
  q = view$q %||% "PFI"
  tail_rule = if (isTRUE(view$mc)) {
    sprintf("a failing fold counts only beyond Monte Carlo error%s and by at least the smallest relevant
      difference (%s).", holm, .csdg_num(view$delta))
  } else {
    sprintf("a failing fold counts only by at least the smallest relevant difference (%s).", .csdg_num(view$delta))
  }
  rest = character()
  mc = isTRUE(view$mc)
  if (st$n_undecided) {
    rest = c(rest, sprintf("%d %s %sbelow the smallest relevant difference", st$n_undecided,
      .csdg_verb(st$n_undecided, "other is", "others are"), if (mc) "within Monte Carlo error or " else ""))
  }
  if (st$n_fails) {
    rest = c(rest, sprintf("%d %s %s", st$n_fails, .csdg_verb(st$n_fails, "fails", "fail"),
      if (mc) "beyond Monte Carlo error" else "by at least the smallest relevant difference"))
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
  direction_texts = if (identical(kind, "direction")) .csdg_text_stability_direction(claim, L, st, criteria)
  out = switch(kind,
    relies_mainly = , top_k = list(
      property = sprintf("The result recurs when %s is refitted on other folds, with the selection repeated.", L),
      criterion = sprintf("Selected anew in each fold, %s %s %s%s in at least %d of %d folds; %s",
        .csdg_and(lab(S)), .csdg_verb(n, "is", "are"), leading, if (identical(kind, "relies_mainly")) {
          sprintf(" with at least %s times the %s of every other predictor", format(claim$factor), q)
        } else "", need, K, tail_rule),
      observation = sprintf("Selected anew in each fold: %s reproduce the result%s%s.", .csdg_folds(st$n_holds, K),
        rest_text, neighbor_text),
      summary = sprintf("the result recurs in %s%s", .csdg_folds(st$n_holds, K), rest_text)
    ),
    order = list(
      property = sprintf("The ordering recurs when %s is refitted on other folds.", L),
      criterion = sprintf("%s has the larger %s in at least %d of %d folds; %s", lab(claim$first), q, need, K,
        tail_rule),
      observation = sprintf("%s exceeds %s in %s%s.", lab(claim$first), lab(claim$second),
        .csdg_folds(st$n_holds, K), rest_text),
      summary = sprintf("%s exceeds %s in %s%s", lab(claim$first), lab(claim$second), .csdg_folds(st$n_holds, K),
        rest_text)
    ),
    direction = direction_texts
  )
  quantity = if (identical(kind, "direction")) "ALE" else q
  again = sprintf("Refits on other folds%s estimate the same learner %s again", if (identical(quantity, "PFI")) {
    " and new permutations"
  } else {
    ""
  }, quantity)
  out$relevance = if (identical(origin, "independently_confirmed")) {
    paste0(again, "; the claim was fixed before these data were analyzed.")
  } else {
    paste0(again, "; on the data that suggested the claim, this share remains optimistic.")
  }
  lapply(out, function(text) gsub("\\s+", " ", text))
}

# Stability texts of a direction claim. A claim that the predictions rise or fall needs, in each fold, the change and
# no change against the claimed direction beyond the minimum change between adjacent grid points; the largest such
# change is reported. A claim about the two percentiles needs only the change.
.csdg_text_stability_direction = function(claim, L, st, criteria) {
  K = st$K
  need = st$need
  minimum = .csdg_crit(criteria$min_change)
  held = .csdg_folds(st$n_holds, K)
  if (!.csdg_is_monotone(claim)) {
    return(list(
      property = sprintf("The difference between the two percentiles recurs when %s is refitted on other folds.", L),
      criterion = sprintf("The change is at least %s in the claimed direction in at least %d of %d folds.", minimum,
        need, K),
      observation = sprintf("The change in the claimed direction is at least %s in %s.", minimum, held),
      summary = sprintf("the change in the claimed direction is at least %s in %s", minimum, held)
    ))
  }
  folds = st$folds
  word = .csdg_reversal_word(claim)
  p1 = .csdg_ordinal(claim$probs[[1L]])
  p2 = .csdg_ordinal(claim$probs[[2L]])
  largest = max(folds$reversal)
  largest_text = if (largest > 0) {
    sprintf("the largest %s between adjacent grid points is %s (fold %d)", word, .csdg_num(largest),
      folds$iteration[[which.max(folds$reversal)]])
  } else {
    sprintf("the ALE does not %s between adjacent grid points in any fold", word)
  }
  n_short = sum(!folds$change_holds)
  n_reversed = sum(!folds$shape_holds)
  failures = c(
    if (n_short) sprintf("the change is below %s in %d %s", minimum, n_short, .csdg_verb(n_short, "fold", "folds")),
    if (n_reversed) {
      sprintf("%s exceeds %s in %d %s", .csdg_reversal_word(claim, article = TRUE), minimum, n_reversed,
        .csdg_verb(n_reversed, "fold", "folds"))
    }
  )
  list(
    property = sprintf("The %s recurs when %s is refitted on other folds.", claim$direction, L),
    criterion = sprintf(paste("In at least %d of %d folds, the change between the %s and %s percentiles is at least %s",
      "in the claimed direction, and no %s of the ALE between adjacent grid points in this range exceeds %s."), need,
      K, p1, p2, minimum, word, minimum),
    observation = sprintf(paste("The change in the claimed direction is at least %s, with no %s beyond it between",
      "adjacent grid points between the %s and %s percentiles, in %s%s; %s."), minimum, word, p1, p2, held,
      if (length(failures)) paste0("; ", paste(failures, collapse = ", and ")) else "", largest_text),
    summary = sprintf("the change in the claimed direction is at least %s, with no %s beyond it between adjacent grid
      points, in %s (largest %s %s)", minimum, word, held, word, .csdg_num(largest))
  )
}

# The comparisons of the procedure that its observation shows: per perturbation, kind of comparison, and named
# predictor, the weakest (see .csdg_weakest_order()), with the number of comparisons it stands for (`n_compared`).
.csdg_procedure_shown = function(comparisons) {
  if (!nrow(comparisons)) return(comparisons)
  ids = paste(comparisons$perturbation, comparisons$comparison, comparisons$predictor, sep = "\r")
  rbindlist(lapply(unique(ids), function(id) {
    group = comparisons[which(ids == id)]
    out = group[.csdg_weakest_order(group)[[1L]]]
    out[, n_compared := nrow(group)]
  }))
}

.csdg_text_procedure = function(claim, imp, view, proc) {
  lab = function(x) .csdg_feature_label(claim, x)
  direction = identical(claim$kind, "direction")
  loco = !direction && .csdg_is_loco(imp)
  # A LOCO claim names refits without single predictors in its wording, as "Under marginal permutation" names the
  # perturbation of PFI.
  qualified = isTRUE(claim$marginal_only) || loco
  qualifier_text = if (loco) "refits without single predictors (LOCO)" else "the marginal permutation"
  base = if (direction) {
    sprintf("ALE of %s on %d quantile intervals of each held-out fold, as the scope names; ALE stays within the data
      intervals.", lab(claim$feature), claim$intervals)
  } else if (loco) {
    sprintf("LOCO with %s: refits without each predictor in each of %d folds, as the scope names.",
      .csdg_loss_label(imp$loss), view$K)
  } else {
    sprintf("Held-out marginal PFI with %s, %d permutations per fold in %d folds, as the scope names.",
      .csdg_loss_label(imp$loss), imp$repetitions, view$K)
  }
  # One clause per perturbation and named predictor: its weakest comparison, out of `n_compared`.
  comparisons = .csdg_procedure_shown(proc$comparisons)
  unit_text = function(unit) {
    if (unit %in% names(imp$groups)) sprintf("%s (%s)", unit, paste(imp$groups[[unit]], collapse = ", ")) else
      lab(unit)
  }
  as_single = if (loco) "as with single predictors" else "as under marginal permutation"
  # Why a comparison is inconclusive. In the clauses of margins, the verdict already says "inconclusive" (`stated`).
  qualifier = function(row, stated = FALSE) {
    switch(row$outcome,
      inconclusive = if (row$estimate == 0) {
        ", a tie"
      } else if (!is.na(row$threshold) && row$threshold > 0 && abs(row$estimate) <= row$threshold) {
        ", within Monte Carlo error"
      } else if (stated) {
        ", given its corrected interval"
      } else {
        ", inconclusive given its corrected interval"
      },
      unchanged = paste0(", ", as_single),
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
        inconclusive = "the order is inconclusive", unchanged = as_single)
      return(sprintf("conditional PFI of %s and %s is %s and %s (%s%s)", lab(row$predictor), lab(row$versus),
        .csdg_num(row$value), .csdg_num(row$versus_value), verdict,
        if (identical(row$outcome, "inconclusive")) qualifier(row, stated = TRUE) else ""))
    }
    verdict = switch(row$outcome, holds = "the result holds", reversed = "the result does not hold",
      inconclusive = "the result is inconclusive", unchanged = paste("the result fails", as_single))
    grouped = identical(row$perturbation, "grouped")
    n_compared = row$n_compared %||% 1L
    selected = if (n_compared > 1L) {
      sprintf(", the weakest of %d %s", n_compared, if (grouped) "groups without named predictors" else "comparisons")
    } else if (grouped) {
      ", the group without named predictors"
    } else {
      ""
    }
    sprintf("with %s %s, %s %s versus %s for %s%s (%s%s)", row$perturbation, if (loco) "LOCO" else "PFI",
      lab(row$predictor), .csdg_num(row$value), .csdg_num(row$versus_value), unit_text(row$versus), selected, verdict,
      if (identical(row$outcome, "inconclusive")) qualifier(row, stated = TRUE) else "")
  }
  clauses = vapply(seq_len(nrow(comparisons)), clause, character(1L))
  outcomes = comparisons$outcome %||% character()
  reversed = any(outcomes == "reversed")
  inconclusive = any(outcomes == "inconclusive")
  unchanged = any(outcomes == "unchanged")
  what = if (identical(claim$kind, "order")) "the order" else "the result"
  conclusion = if (reversed && qualified) {
    sprintf("The wording names %s, so the reversal shows what the qualifier is for.", qualifier_text)
  } else if (reversed) {
    sprintf("%s is supported only under marginal permutation, which the wording does not name.",
      .csdg_sentence(what))
  } else if (inconclusive && qualified) {
    sprintf("The wording names %s, so the other %s context.", qualifier_text,
      if (loco) "refits are" else "perturbations are")
  } else if (inconclusive) {
    sprintf("Whether %s holds under the other perturbation is open.", what)
  } else if (length(clauses) && all(outcomes == "unchanged")) {
    sprintf("The %s result itself does not hold, so the %s does not bear on the wording.",
      if (loco) "single-predictor" else "marginal", if (loco) "grouped refit" else "perturbation")
  } else if (length(clauses)) {
    sprintf("The result also holds under %s.", if (loco) "grouped refits" else "these perturbations")
  } else if (loco && !length(proc$context)) {
    "No grouped LOCO was requested."
  } else if (!direction && !length(proc$context)) {
    "No grouped or conditional comparison was requested."
  } else {
    character()
  }
  if (unchanged && !all(outcomes == "unchanged")) {
    conclusion = c(conclusion, sprintf("A comparison that fails %s does not bear on the wording.", as_single))
  }
  observation = paste(c(base, if (length(clauses)) paste0(.csdg_sentence(paste(clauses, collapse = "; ")), "."),
    conclusion, if (length(proc$context)) paste0(.csdg_sentence(paste(proc$context, collapse = "; ")), ".")),
    collapse = " ")
  decisive = if (reversed) clauses[outcomes == "reversed"] else clauses[outcomes == "inconclusive"]
  summary = if (length(decisive)) {
    paste(c(decisive, if (reversed && !qualified) {
      sprintf("%s is supported only under marginal permutation", what)
    } else if (qualified) sprintf("the wording names %s", qualifier_text)), collapse = "; ")
  } else if (length(clauses) && all(outcomes == "unchanged")) {
    sprintf("the %s does not bear on the wording, because the %s result does not hold",
      if (loco) "grouped refit" else "perturbation", if (loco) "single-predictor" else "marginal")
  } else if (length(clauses)) {
    if (loco) "the result also holds under grouped refits" else
      "the result also holds under grouped or conditional permutation"
  } else if (direction) {
    "ALE within the data intervals, as the scope names"
  } else if (loco) {
    "LOCO with refits as the scope names; no grouped LOCO was requested"
  } else {
    "held-out marginal PFI as the scope names; no grouped or conditional comparison was requested"
  }
  .csdg_squash(list(
    property = "The procedure computes the named quantity, and the wording is not broader than the scope.",
    criterion = if (direction) {
      "ALE on quantile intervals of the held-out fold, as named in the scope."
    } else if (loco) {
      paste("LOCO with refits without each predictor, as named in the scope; the wording names refits without single",
        "predictors, so grouped LOCO is reported with the procedure but does not contradict it.")
    } else {
      paste("Held-out marginal PFI as named in the scope; if grouped or conditional PFI reverses the result",
        "(beyond Monte Carlo error and the corrected interval), the wording names the marginal permutation.")
    },
    observation = observation,
    relevance = if (loco) {
      paste("LOCO refits the learner without the predictor, so no unrealistic inputs arise; it measures the",
        "information a predictor does not share with the others, and grouped LOCO measures it for a group.")
    } else {
      paste("Marginal permutation includes input combinations absent from the data; conditional and grouped",
        "PFI measure reliance on information a predictor does not share and on a group.")
    },
    summary = summary
  ))
}

.csdg_others_text = function(n) if (n == 1L) "other predictor" else sprintf("other %d predictors", n)

.csdg_outside_text = function(n) if (n == 1L) "the predictor" else sprintf("the %d predictors", n)

# Texts of the dependence screen (G2). `mode` is "threat" (flagged, unresolved threat), "qualified" (flagged, but the
# wording names the marginal permutation), "loco" (a LOCO claim), or "clear" (no named predictor is flagged). Screens
# of earlier versions have no columns on groups.
.csdg_text_dependence = function(claim, screen, criteria, mode) {
  lab = function(x) .csdg_feature_label(claim, x)
  group_of = function(row) row$group %||% ""
  missing_of = function(row) {
    missing = row$declared_missing %||% ""
    if (nzchar(missing)) strsplit(missing, ", ", fixed = TRUE)[[1L]] else character()
  }
  describe = function(rows, short = FALSE) {
    vapply(seq_len(nrow(rows)), function(i) {
      row = rows[i]
      parts = character()
      if (!is.na(row$r2)) {
        parts = c(parts, if (short) {
          sprintf("R^2 = %s%s", .csdg_r(row$r2), if (!is.na(row$partner)) sprintf(" (mainly %s)", lab(row$partner))
            else "")
        } else {
          sprintf("R^2 = %s%s on the %s%s", .csdg_r(row$r2),
            if (isTRUE(row$factor)) " (largest over its levels)" else "", .csdg_others_text(row$n_other),
            if (!is.na(row$partner) && row$n_other > 1L) {
              sprintf(" (%s alone: %s)", lab(row$partner), .csdg_r(row$partner_r2))
            } else {
              ""
            })
        })
        # A group of grouped PFI that contains the predictor but leaves out predictors on which it depends.
        if (nzchar(group_of(row)) && !isTRUE(row$covered) && !is.na(row$r2_outside) && row$n_outside > 0L &&
          row$r2_outside >= criteria$max_r2 - 1e-12) {
          partner = !is.na(row$partner_outside) && row$n_outside > 1L
          parts[[length(parts)]] = paste(parts[[length(parts)]], if (short) {
            sprintf("and %s outside its group %s%s", .csdg_r(row$r2_outside), group_of(row),
              if (!is.na(row$partner_outside)) sprintf(" (mainly %s)", lab(row$partner_outside)) else "")
          } else {
            sprintf("and %s on %s outside its group %s%s", .csdg_r(row$r2_outside), .csdg_outside_text(row$n_outside),
              group_of(row), if (partner) {
                sprintf(" (%s alone: %s)", lab(row$partner_outside), .csdg_r(row$partner_outside_r2))
              } else {
                ""
              })
          })
        }
      }
      if (nzchar(row$declared)) {
        missing = missing_of(row)
        parts = c(parts, sprintf("in the group of dependent predictors %s declared by the researcher%s", row$declared,
          if (length(missing)) {
            sprintf(" (%s outside its group %s)", .csdg_and(lab(missing)), group_of(row))
          } else {
            ""
          }))
      }
      sprintf("%s: %s", lab(row$predictor), paste(parts, collapse = ", and "))
    }, character(1L))
  }
  cutoff = .csdg_cutoff(criteria$max_r2)
  flagged = screen[screen$flagged]
  covered = screen[screen$covered]
  computed = screen[!is.na(screen$r2)]
  r2_text = if (nrow(computed)) {
    sprintf("R^2 on the %s: %s", .csdg_others_text(computed$n_other[[1L]]),
      .csdg_and(sprintf("%s %s", lab(computed$predictor), .csdg_r(computed$r2))))
  } else {
    "R^2 could not be computed"
  }
  by_text = covered$covered_by %||% rep("grouped or conditional PFI", nrow(covered))
  covered_text = if (nrow(covered)) {
    clauses = vapply(unique(by_text), function(label) {
      rows = covered[by_text == label]
      outside = rows[nzchar(rows$group %||% "") & grepl("grouped PFI", label, fixed = TRUE) & rows$n_outside > 0L]
      sprintf("%s %s %s%s", label, if (grepl(" and ", label, fixed = TRUE)) "cover" else "covers",
        .csdg_and(lab(rows$predictor)),
        if (nrow(outside)) {
          sprintf(" (R^2 = %s on %s outside the group)", .csdg_and(.csdg_r(outside$r2_outside)),
            .csdg_outside_text(outside$n_outside[[1L]]))
        } else {
          ""
        })
    }, character(1L))
    paste0("; ", paste(clauses, collapse = "; "))
  } else {
    ""
  }
  uncovered = screen[!screen$covered]
  declared_text = if (length(criteria$dependent_groups)) " or belongs to a declared group" else ""
  clear_tail = if (!nrow(uncovered)) {
    ""
  } else if (nrow(covered)) {
    sprintf("; %s %s %s%s", .csdg_and(lab(uncovered$predictor)), .csdg_verb(nrow(uncovered), "does not reach",
      "do not reach"), cutoff, if (nzchar(declared_text)) " or belong to a declared group" else "")
  } else {
    sprintf("; no named predictor reaches %s%s", cutoff, declared_text)
  }
  advice = "conditional = list(...) or groups = list(...) in csdg_importance() shows whether this changes the result"
  one = nrow(flagged) == 1L
  grouped = any(nzchar(flagged$group %||% ""))
  # Why no comparison covers the flagged predictors: none was computed, or their groups leave out predictors on which
  # they depend.
  uncovered_text = if (!grouped) {
    sprintf("no grouped or conditional PFI was computed for %s", if (one) "it" else "them")
  } else if (one) {
    paste("no conditional PFI was computed for it, and no group of grouped PFI contains it together with the",
      "predictors on which it depends")
  } else {
    paste("no conditional PFI was computed for them, and no group of grouped PFI contains any of them together with",
      "the predictors on which it depends")
  }
  observation = switch(mode,
    threat = sprintf(paste("%s; %s. Marginal PFI can be redistributed between dependent predictors, so the wording",
      "may be broader than marginal PFI shows (%s)."), paste(describe(flagged), collapse = "; "), uncovered_text,
      advice),
    qualified = sprintf("%s; the wording names the marginal permutation, so the dependence is context.",
      paste(describe(flagged), collapse = "; ")),
    loco = sprintf(paste("%s. LOCO refits the learner without the predictor, so dependence lowers the LOCO of a",
      "predictor that another one can replace rather than shifting importance to it; reported as context."), r2_text),
    clear = sprintf("%s%s%s.", r2_text, covered_text, clear_tail)
  )
  declared = length(criteria$dependent_groups) > 0L
  threat = identical(mode, "threat")
  list(
    property = if (threat) "No unquantified dependence makes the wording broader than marginal PFI." else
      NA_character_,
    criterion = if (threat) {
      sprintf(paste("No named predictor has R^2 of at least %s in a linear regression on all other predictors",
        "(factors one-hot encoded)%s, unless conditional PFI was computed for it, or grouped PFI for a group that",
        "contains it%s such that its R^2 on the predictors outside the group is below %s."), cutoff,
        if (declared) " or belongs to a group of dependent predictors declared by the researcher" else "",
        if (declared) " (and the predictors declared dependent with it)" else "", cutoff)
    } else {
      NA_character_
    },
    observation = observation,
    relevance = if (identical(mode, "loco")) {
      paste("LOCO measures the information a predictor does not share with the others, so dependence lowers it",
        "for predictors that others can replace; it does not make the wording broader than LOCO shows.")
    } else {
      paste("Dependence between predictors can redistribute marginal PFI between them; the R^2 of a predictor on",
        "all other predictors also detects dependence on several predictors jointly, and its R^2 on the predictors",
        "outside its group shows whether grouped PFI, which permutes only the members of the group jointly, accounts",
        "for it.")
    },
    summary = switch(mode,
      threat = sprintf("%s, and no grouped or conditional PFI %s",
        paste(describe(flagged, short = TRUE), collapse = "; "),
        if (grouped) sprintf("covers %s", if (one) "it" else "them") else "was computed"),
      qualified = paste(describe(flagged, short = TRUE), collapse = "; "),
      sub("\\.$", "", observation)
    ),
    source = sprintf("R^2 cutoff %s: %s", cutoff, criteria$sources$max_r2)
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
    "the result is inconclusive"
  }
  m = content$margins
  numbers = switch(claim$kind,
    relies_mainly = , top_k = {
      lo = content$largest_other
      text = sprintf("%s versus %s for %s", .csdg_and(paste(lab(claim$predictors),
        .csdg_num(view$avg[claim$predictors]))), .csdg_num(view$avg[[lo]]), lab(lo))
      # A result that does not hold because of another predictor than the largest other one names that comparison.
      w = .csdg_weakest(content$comparisons %||% m)
      if (!identical(content$status, "supported") && !identical(w$versus, lo)) {
        text = sprintf("%s; weakest comparison: %s, %s", text, .csdg_comparison_label(claim, w$predictor, w$versus),
          .csdg_ci(w$estimate, w$lower, w$upper))
      }
      text
    },
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
  "mean_change", "feature", "status", "learner", "iteration", "gate", "check", "role", "flagged", "r2",
  "covered", "declared", "partner", "partner_r2", "loss_full", "loss_reduced", "unit", "max_decrease", "max_increase",
  "n_compared"
))
