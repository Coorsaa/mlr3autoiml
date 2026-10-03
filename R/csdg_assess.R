#' Assess a checked claim
#'
#' @description
#' Fifth step of the analysis-first interface: applies the decision rule of the article to the records of a
#' [csdg_check()] result with [csdg_adjudicate_claim()].
#' A claim is not met if a required property is contradicted, unresolved if one is open, and met otherwise;
#' favorable results never offset a contradicted property.
#' A claim derived from the results is labeled "(exploratory)": it was formulated after the explanation was
#' inspected and is established only after a test on new data with [csdg_confirm()].
#' The report names each decisive property by its check and gate, for example "Content (G2)" or "Procedure (G2)";
#' for a claim that is not met or unresolved, it adds the other open properties and, where the results suggest one,
#' the closest claim that the averages satisfy (exploratory, to be checked before it is reported).
#'
#' @param x A [csdg_check()] result.
#' @param note Optional note stored with the assessment.
#'
#' @return A `CSDGAssessment` list with `claim`, `check`, `assessment` (`"met"`, `"not_met"`, or `"unresolved"`),
#'   `label` (for example `"met (exploratory)"`), `decisive` (the contradicted required rows if any, otherwise the
#'   open ones), `also_open`, `hints`, `decision_options`, `report` (a few sentences), `adjudication` (the
#'   [csdg_adjudicate_claim()] result), and `note`.
#'   `as.data.table()` returns one row for reports.
#' @seealso [csdg_check()], [csdg_confirm()], [csdg_export_assessment()]
#' @examples
#' set.seed(1)
#' n = 200
#' x = data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
#' x$y = 2 * x$a + x$b + rnorm(n)
#' task = mlr3::as_task_regr(x, target = "y", id = "toy")
#' fits = csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3, seed = 1)
#' imp = csdg_importance(fits, repetitions = 5, seed = 2)
#' res = csdg_assess(csdg_check(claim_order(imp, c("a", "b")), imp))
#' res
#' @export
csdg_assess = function(x, note = NULL) {
  assert_class(x, "CSDGCheck", .var.name = "x")
  assert_string(note, min.chars = 1L, null.ok = TRUE, .var.name = "note")
  adjudication = csdg_adjudicate_claim(x$records, claim_applicable = TRUE, plan = x$plan)
  assessment = adjudication$assessment
  suffix = if (identical(x$origin, "independently_confirmed")) "(tested on new data)" else "(exploratory)"
  label = paste(gsub("_", " ", assessment, fixed = TRUE), suffix)
  bearing = x$table[x$table$role %in% c("required_property", "unresolved_threat")]
  decisive = if (identical(assessment, "not_met")) {
    bearing[bearing$status == "contradicted"]
  } else if (identical(assessment, "unresolved")) {
    bearing[bearing$status == "open"]
  } else {
    bearing[0L]
  }
  also_open = if (identical(assessment, "not_met")) bearing[bearing$status == "open"] else bearing[0L]
  hints = if (identical(assessment, "met")) character() else .csdg_hints(x, decisive)
  report = .csdg_report(x, assessment, label, decisive, also_open)
  structure(
    list(
      claim = x$claim,
      check = x,
      assessment = assessment,
      label = label,
      decisive = decisive,
      also_open = also_open,
      hints = hints,
      decision_options = adjudication$decision_options,
      report = report$report,
      report_body = report$body,
      adjudication = adjudication,
      note = note
    ),
    class = c("CSDGAssessment", "list")
  )
}

# "Content (G2)", or "Content (G6a) for ridge regression" for a learner-specific row of a several-learner claim.
.csdg_row_label = function(x, row) {
  label = .csdg_sentence(.csdg_check_label(row$check, row$gate))
  if (length(x$claim$covered) > 1L && !is.na(row$learner)) {
    label = sprintf("%s for %s", label, unname(x$claim$labels_learner[row$learner]) %||% row$learner)
  }
  label
}

# The summary without the learner prefix that several-learner rows carry.
.csdg_row_summary = function(x, row) {
  if (is.na(row$learner)) return(row$summary)
  prefix = paste0(unname(x$claim$labels_learner[row$learner]) %||% row$learner, ": ")
  if (startsWith(row$summary, prefix)) substring(row$summary, nchar(prefix) + 1L) else row$summary
}

.csdg_report_reason = function(x, assessment, decisive) {
  if (identical(assessment, "met")) {
    required = x$table[x$table$role == "required_property"]
    parts = vapply(x$claim$covered, function(l) {
      rows = rbind(required[required$learner %in% l & required$check == "content"],
        required[required$learner %in% l & required$check == "stability"])
      text = paste(vapply(seq_len(nrow(rows)), function(i) .csdg_row_summary(x, rows[i]), character(1L)),
        collapse = "; ")
      if (length(x$claim$covered) > 1L) sprintf("%s: %s", unname(x$claim$labels_learner[l]), text) else text
    }, character(1L))
    return(paste(parts, collapse = ". "))
  }
  word = if (identical(assessment, "not_met")) "contradicted" else "open"
  shown = utils::head(seq_len(nrow(decisive)), 3L)
  parts = vapply(shown, function(i) {
    row = decisive[i]
    label = .csdg_row_label(x, row)
    sprintf("%s%s is %s: %s", if (i > 1L) "also, " else "", if (i > 1L) .csdg_lower_first(label) else label, word,
      .csdg_row_summary(x, row))
  }, character(1L))
  if (nrow(decisive) > 3L) parts = c(parts, sprintf("and %d more", nrow(decisive) - 3L))
  paste(parts, collapse = "; ")
}

.csdg_report = function(x, assessment, label, decisive, also_open) {
  confirmed = identical(x$origin, "independently_confirmed")
  next_text = switch(assessment,
    met = if (confirmed) {
      "The claim and its criteria were fixed before these data were analyzed."
    } else {
      paste("The claim was formulated after the explanation was inspected; it is established only after a test on",
        "new data (csdg_confirm()).")
    },
    not_met = paste("Revise the claim to what the evidence supports, or withhold it; where the claim matters,",
      "report it as not supported."),
    unresolved = paste("Withhold the claim or supply the missing evidence, for example new data; where the claim",
      "matters, report it as not established.")
  )
  open_text = if (nrow(also_open)) {
    sprintf(" Also open: %s.", .csdg_and(unique(vapply(seq_len(nrow(also_open)), function(i) {
      .csdg_lower_first(.csdg_row_label(x, also_open[i]))
    }, character(1L)))))
  } else {
    ""
  }
  reason = .csdg_report_reason(x, assessment, decisive)
  body = sprintf("%s.%s %s", .csdg_sentence(reason), open_text, next_text)
  list(report = sprintf("%s %s: %s.%s %s", x$claim$statement, .csdg_sentence(label), reason, open_text, next_text),
    body = body)
}

# Constructive options for a claim that is not met or unresolved, built from the computed results.
.csdg_hints = function(x, decisive) {
  claim = x$claim
  checks = decisive$check
  l = claim$learner
  lab = function(v) .csdg_feature_label(claim, v)
  hints = character()
  if (claim$kind %in% c("relies_mainly", "top_k") && any(checks %in% c("content", "stability"))) {
    content = x$details$content[[l]]
    P = x$details$fold_pfi[[l]]
    S = claim$predictors
    k = length(S)
    need = x$criteria$need
    if (identical(claim$kind, "relies_mainly") && all(is.finite(content$ratios))) {
      hints = c(hints, sprintf("on average, %s %s %s times the PFI of the largest other predictor (claimed: %s)",
        .csdg_and(lab(S)), if (k == 1L) "has" else "have at least", .csdg_ratio(min(content$ratios)),
        format(claim$factor)))
      if (!is.null(P) && all(content$margins$estimate + (claim$factor - 1) * content$averages[[content$largest_other]]
        > 0)) {
        folds = sum(vapply(seq_len(nrow(P)), function(i) .csdg_fold_ranking(P[i, ], S, 1, "top_k")$holds,
          logical(1L)))
        wording = if (k == 1L) sprintf("relies most on %s", lab(S)) else sprintf("the %s predictors it relies on most",
          .csdg_number_word(k))
        hints = c(hints, sprintf("'%s' (claim_top_k(k = %d)) holds on average and reproduces in %d of %d folds",
          wording, k, folds, nrow(P)))
      }
    }
    st = x$details$stability[[l]]
    if (!is.null(st) && nrow(st$neighbors)) {
      ok = st$neighbors[st$neighbors$folds >= need]
      for (i in seq_len(nrow(ok))) {
        hints = c(hints, sprintf("with k = %d (%s), the selection reproduces in %d of %d folds", ok$cutoff[[i]],
          ok$predictors[[i]], ok$folds[[i]], ok$K[[i]]))
      }
    }
  }
  if (any(checks == "procedure") && !isTRUE(claim$marginal_only)) {
    hints = c(hints, "marginal_only = TRUE limits the wording to marginal permutation")
  }
  if (any(checks == "correlation")) {
    pairs = x$details$correlation
    example = if (!is.null(pairs) && nrow(pairs)) {
      sprintf("csdg_importance(imp, conditional = list(%s = \"%s\", %s = \"%s\"))", pairs$predictor[[1L]],
        pairs$partner[[1L]], pairs$partner[[1L]], pairs$predictor[[1L]])
    } else {
      "csdg_importance(imp, conditional = list(...))"
    }
    hints = c(hints, sprintf("compute conditional PFI (%s) or reword with marginal_only = TRUE", example))
  }
  if (any(checks %in% c("measurement", "judgment") & decisive$gate == "G0b")) {
    hints = c(hints, "enter validity evidence for the named constructs with csdg_judge(\"G0b\", ...)")
  }
  if (!length(hints)) return(character())
  sprintf("Options on these results (exploratory; check before reporting): %s.", paste(hints, collapse = "; "))
}

#' @export
format.CSDGAssessment = function(x, width = getOption("width", 80L), ...) {
  table = x$check$table
  required = table[table$role %in% c("required_property", "unresolved_threat")]
  statuses = paste(vapply(seq_len(nrow(required)), function(i) {
    sprintf("%s %s", .csdg_row_label(x, required[i]), required$status[[i]])
  }, character(1L)), collapse = " | ")
  c(
    .csdg_wrap(sprintf("<CSDG assessment> %s", x$claim$statement), width = width),
    paste0("  ", .csdg_sentence(x$label)),
    if (inherits(x, "CSDGConfirmation")) paste0("  Exploratory result: ", x$exploratory$label, "."),
    .csdg_wrap(statuses, indent = 2L, width = width),
    .csdg_wrap(x$report_body, indent = 2L, width = width),
    if (length(x$hints)) .csdg_wrap(x$hints, indent = 2L, width = width),
    if (!is.null(x$note)) .csdg_wrap(sprintf("Note: %s", x$note), indent = 2L, width = width),
    sprintf("  Decision options: %s", paste(x$decision_options, collapse = " or "))
  )
}

#' @export
print.CSDGAssessment = function(x, width = getOption("width", 80L), ...) {
  cat(format(x, width = width, ...), sep = "\n")
  invisible(x)
}

#' @export
as.data.table.CSDGAssessment = function(x, keep.rownames = FALSE, ...) {
  properties = x$adjudication$properties
  gates = function(status) paste(properties$gate_id[properties$status == status], collapse = ", ")
  data.table(
    claim_id = x$claim$id,
    statement = x$claim$statement,
    assessment = x$assessment,
    label = x$label,
    origin = x$check$origin,
    decisive_gates = paste(unique(x$decisive$gate), collapse = ", "),
    decisive_checks = paste(unique(.csdg_check_label(x$decisive$check, x$decisive$gate)), collapse = ", "),
    decisive_property = if (nrow(x$decisive)) x$decisive$property[[1L]] else NA_character_,
    decision_options = paste(x$decision_options, collapse = ", "),
    supported_gates = gates("supported"),
    contradicted_gates = gates("contradicted"),
    open_gates = gates("open"),
    report = x$report
  )
}

#' Test a claim on new data
#'
#' @description
#' Applies a checked claim, with its criteria fixed, to new data: the named predictors, the order, or the direction
#' are not selected again, the criteria of `chk` are reused (relative criteria such as the minimum importance are
#' recomputed on the new improvement; an absolute smallest relevant difference or minimum change is reused), and
#' the selection rule is still applied in each new fold for the stability property.
#' Judgments entered in `chk` are carried over, and the learner labels are those of the claim.
#' The claim record is rebuilt with `provenance$origin = "independently_confirmed"`, and the assessment is labeled
#' "(tested on new data)".
#'
#' `new` is either a new [mlr3::Task], which is fitted with the learners, resampling, measure, and seeds of the
#' check and analyzed with the same PFI settings (repetitions, groups, conditional PFI), or a [csdg_importance()]
#' result on new data computed by the analyst (for direction claims, also a [csdg_effect()] or [csdg_fit()]
#' result).
#' A result with another loss, other groups, or without the conditional PFI used in the check is an error; other
#' numbers of folds or permutations give a warning.
#'
#' The new data must not share rows with the data of the check: every row of predictors and outcome is hashed, and
#' any overlap is an error.
#' Whether the new data were sampled independently cannot be checked by the package.
#'
#' @param chk The [csdg_check()] result on the data that suggested the claim.
#' @param new A new [mlr3::Task] or a [csdg_importance()] result on new data; see Description.
#' @param claim Optional; the claim of `chk`, checked for identity if given.
#' @param note Optional note stored with the assessment.
#'
#' @return A `CSDGConfirmation` (a `CSDGAssessment`) with the additional element `exploratory`, the one-row summary
#'   of the exploratory assessment.
#' @seealso [csdg_check()], [csdg_assess()]
#' @examples
#' simulate = function(seed) {
#'   set.seed(seed)
#'   n = 200
#'   x = data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
#'   x$y = 2 * x$a + x$b + rnorm(n)
#'   mlr3::as_task_regr(x, target = "y", id = paste0("toy_", seed))
#' }
#' learners = list(tree = mlr3::lrn("regr.rpart"))
#' imp = csdg_importance(csdg_fit(simulate(1), learners, folds = 3, seed = 1), repetitions = 5, seed = 2)
#' chk = csdg_check(claim_order(imp, c("a", "b")), imp)
#' csdg_confirm(chk, simulate(2))
#' @export
csdg_confirm = function(chk, new, claim = NULL, note = NULL) {
  if (inherits(chk, "CSDGDerivedClaim") && inherits(new, "CSDGCheck")) {
    .csdg_stop("The arguments are csdg_confirm(chk, new); the claim is taken from `chk`.")
  }
  assert_class(chk, "CSDGCheck", .var.name = "chk")
  assert_string(note, min.chars = 1L, null.ok = TRUE, .var.name = "note")
  if (!is.null(claim)) {
    assert_class(claim, "CSDGDerivedClaim", .var.name = "claim")
    fixed_fields = c("id", "kind", "covered", "predictors", "k", "factor", "first", "second", "feature",
      "direction", "probs", "intervals", "marginal_only", "refers_to")
    if (!identical(unclass(claim)[fixed_fields], unclass(chk$claim)[fixed_fields])) {
      .csdg_stop("`claim` must be the claim of `chk`.")
    }
  }
  claim = chk$claim
  if (inherits(new, "Task")) new = .csdg_refit_for_confirmation(chk, new)
  new_imp = .csdg_check_input(new, claim)
  if (identical(new_imp$fits$data_hash, chk$source$data_hash)) {
    .csdg_stop("`new` was computed on the same data as the check; this is not a test on new data.")
  }
  overlap = sum(new_imp$fits$row_hashes %in% chk$source$row_hashes)
  if (overlap > 0L) {
    .csdg_stop(paste("%s of the new rows also %s in the data of the check (identical predictors and outcome);",
      "a test on new data needs new rows."), .csdg_count(overlap), if (overlap == 1L) "occurs" else "occur")
  }
  .csdg_confirm_settings(chk, new_imp)
  .csdg_check_compatible(claim, new_imp, new_data = TRUE)
  new_imp$fits$labels[claim$covered] = claim$labels_learner[claim$covered]
  new_claim = .csdg_confirmation_claim(claim, chk, new_imp)
  criteria = chk$criteria[c("min_importance", "stability", "min_difference_supplied", "min_difference_value",
    "min_change", "max_cor", "factor", "level", "interval_rule", "fixed", "sources")]
  new_chk = .csdg_check_impl(new_claim, new_imp, criteria, chk$judgments, chk$note,
    origin = "independently_confirmed")
  out = csdg_assess(new_chk, note = note)
  out$exploratory = as.data.table(csdg_assess(chk))
  class(out) = c("CSDGConfirmation", class(out))
  out
}

.csdg_refit_for_confirmation = function(chk, task) {
  refit = chk$refit
  if (is.null(refit$resampling)) {
    .csdg_stop(paste("The check used an instantiated or grouped resampling, which cannot be applied to a new task;",
      "fit the new data with csdg_fit() and csdg_importance() and pass the result."))
  }
  supplied = setdiff(chk$source$strata_features, names(chk$source$conditional_on))
  if (length(supplied)) {
    .csdg_stop(paste("The check used supplied strata for %s, which cannot be carried to new data; compute",
      "csdg_importance() on the new data and pass the result."), .csdg_and(supplied))
  }
  fits = csdg_fit(task, refit$learners, resampling = refit$resampling, measure = refit$measure,
    labels = refit$labels, seed = if (is.na(refit$seed)) NULL else refit$seed)
  if (identical(chk$claim$kind, "direction")) return(fits)
  src = chk$source
  csdg_importance(fits, repetitions = src$R, groups = src$groups,
    conditional = if (length(src$conditional_on)) src$conditional_on, batch_size = src$batch_size,
    seed = src$seed)
}

.csdg_confirm_settings = function(chk, new_imp) {
  src = chk$source
  if (!identical(new_imp$loss, src$loss)) {
    .csdg_stop("`new` uses the loss %s; the check used %s.", new_imp$loss, src$loss)
  }
  if (!identical(chk$claim$kind, "direction")) {
    same_groups = setequal(names(new_imp$groups), names(src$groups)) &&
      all(vapply(names(src$groups), function(g) setequal(new_imp$groups[[g]], src$groups[[g]]), logical(1L)))
    if (!same_groups) .csdg_stop("`new` has other groups than the check; use the same `groups`.")
    missing_conditional = setdiff(src$strata_features, new_imp$strata_features)
    if (length(missing_conditional)) {
      .csdg_stop(paste("The check compared conditional PFI of %s; `new` has none. Compute it with",
        "csdg_importance(new_imp, conditional = ...) or pass a new task."), .csdg_and(missing_conditional))
    }
    for (feature in intersect(names(src$conditional_on), new_imp$strata_features)) {
      if (!setequal(src$conditional_on[[feature]], new_imp$conditional_on[[feature]] %||% character())) {
        .csdg_stop("Conditional PFI of %s is conditioned on other predictors than in the check.", feature)
      }
    }
    if (!is.na(src$R) && !identical(as.integer(new_imp$repetitions), as.integer(src$R))) {
      .csdg_warn("`new` uses %d permutations per fold; the check used %d.", new_imp$repetitions, src$R)
    }
  }
  if (!identical(as.integer(new_imp$fits$K), as.integer(src$K))) {
    .csdg_warn("`new` uses %d folds; the check used %d, so the stability criterion counts other folds.",
      new_imp$fits$K, src$K)
  }
  invisible(TRUE)
}

.csdg_confirmation_claim = function(claim, chk, new_imp) {
  fits = new_imp$fits
  out = claim
  if (identical(claim$kind, "direction")) {
    out$effect = .csdg_direction_effects(fits, claim$feature, claim$probs, claim$intervals)$changes
  }
  out$scope = .csdg_derived_scope(out, fits, new_imp)
  out$source = list(task_id = fits$task_id, features = fits$features, loss = claim$source$loss, K = fits$K,
    R = new_imp$repetitions, data_hash = fits$data_hash)
  out$origin = "independently_confirmed"
  out$selection = "Claim and criteria fixed in the exploratory check."
  out$record = .csdg_derived_record(out, origin = "independently_confirmed", date = format(Sys.Date()),
    time_basis = "date of the test on new data",
    selection_basis = "claim and criteria fixed in the exploratory check",
    evidence_ids = paste0(fits$task_id, ":", substr(fits$data_hash, 1L, 12L)))
  out
}

#' Export a claim assessment
#'
#' @description
#' Writes the records of a [csdg_assess()] or [csdg_confirm()] result to a directory
#' `<path>/<prefix>_csdg_assessment`: the claim card (`claim.json`), the criteria (`criteria.json`), the check table
#' (`check.csv`), the evidence records (`records.json`), the gate plan (`gate_plan.csv`), the assessment
#' (`assessment.json`), and, for a confirmation, the exploratory assessment (`exploratory.csv`).
#' No models, predictions, or row-level data are written.
#' The directory is replaced atomically.
#'
#' @param x A [csdg_assess()] or [csdg_confirm()] result.
#' @param path Directory in which the export directory is created.
#' @param prefix Name prefix of the export directory; defaults to the claim id.
#'
#' @return The path of the export directory, invisibly.
#' @examples
#' set.seed(1)
#' n = 200
#' x = data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
#' x$y = 2 * x$a + x$b + rnorm(n)
#' task = mlr3::as_task_regr(x, target = "y", id = "toy")
#' imp = csdg_importance(csdg_fit(task, list(tree = mlr3::lrn("regr.rpart")), folds = 3), repetitions = 5)
#' res = csdg_assess(csdg_check(claim_order(imp, "a", "b"), imp))
#' dir = csdg_export_assessment(res, tempdir())
#' list.files(dir)
#' @export
csdg_export_assessment = function(x, path, prefix = NULL) {
  assert_class(x, "CSDGAssessment", .var.name = "x")
  assert_string(path, min.chars = 1L, .var.name = "path")
  assert_string(prefix, min.chars = 1L, null.ok = TRUE, .var.name = "prefix")
  prefix = .safe_name(prefix %||% x$claim$id)
  target = file.path(path, paste0(prefix, "_csdg_assessment"))
  chk = x$check
  criteria = chk$criteria
  criteria$min_difference = as.list(criteria$min_difference)
  criteria$tau = as.list(criteria$tau)
  plan = as.data.table(chk$plan)
  plan[, required_components := as.character(required_components)]
  .atomic_dir(target, function(dir) {
    .write_json(.card_to_list(x$claim$record), file.path(dir, "claim.json"))
    .write_json(criteria, file.path(dir, "criteria.json"))
    .write_csv(chk$table, file.path(dir, "check.csv"))
    .write_json(lapply(chk$records, .card_to_list), file.path(dir, "records.json"))
    .write_csv(plan, file.path(dir, "gate_plan.csv"))
    .write_json(as.list(as.data.table(x)), file.path(dir, "assessment.json"))
    if (inherits(x, "CSDGConfirmation")) .write_csv(x$exploratory, file.path(dir, "exploratory.csv"))
  })
  invisible(target)
}
