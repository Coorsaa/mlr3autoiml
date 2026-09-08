
.cramers_v = function(x, y) {
  ok = !is.na(x) & !is.na(y)
  x = droplevels(as.factor(x[ok]))
  y = droplevels(as.factor(y[ok]))
  tab = table(x, y, useNA = "no")
  n = sum(tab)
  if (n == 0L || nrow(tab) < 2L || ncol(tab) < 2L) return(NA_real_)
  chi = suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  phi2 = as.numeric(chi) / n
  r = nrow(tab)
  k = ncol(tab)
  phi2_corr = max(0, phi2 - ((k - 1) * (r - 1)) / max(n - 1, 1))
  r_corr = r - ((r - 1)^2) / max(n - 1, 1)
  k_corr = k - ((k - 1)^2) / max(n - 1, 1)
  denom = min(k_corr - 1, r_corr - 1)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  sqrt(phi2_corr / denom)
}

.eta_squared = function(numeric_x, categorical_y) {
  ok = is.finite(numeric_x) & !is.na(categorical_y)
  x = numeric_x[ok]
  g = droplevels(as.factor(categorical_y[ok]))
  if (length(x) < 3L || nlevels(g) < 2L) return(NA_real_)
  grand = mean(x)
  means = tapply(x, g, mean)
  ns = table(g)
  ss_between = sum(as.numeric(ns) * (as.numeric(means) - grand)^2)
  ss_total = sum((x - grand)^2)
  if (ss_total <= 0) return(NA_real_)
  ss_between / ss_total
}

.variable_kind = function(x, max_levels) {
  if (is.numeric(x) && length(unique(stats::na.omit(x))) > max_levels) {
    "continuous"
  } else if (is.numeric(x) && length(unique(stats::na.omit(x))) > 10L) {
    "continuous"
  } else {
    "categorical"
  }
}

#' @rdname csdg_diagnostics
#' @export
csdg_dependence = function(data, features = NULL, max_levels = 50L) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  dt = .as_dt(data)
  features = features %||% names(dt)
  checkmate::assert_character(features, any.missing = FALSE, min.len = 1L, unique = TRUE)
  missing = setdiff(features, names(dt))
  if (length(missing)) {
    .csdg_stop("Unknown features: %s.", paste(missing, collapse = ", "))
  }
  checkmate::assert_int(max_levels, lower = 2)
  dt = dt[, ..features]

  support = data.table::rbindlist(lapply(features, function(nm) {
    x = dt[[nm]]
    kind = .variable_kind(x, max_levels)
    nonmiss = x[!is.na(x)]
    data.table::data.table(
      feature = nm,
      storage = class(x)[[1L]],
      kind = kind,
      n = length(x),
      n_missing = sum(is.na(x)),
      missing_fraction = mean(is.na(x)),
      n_unique = data.table::uniqueN(nonmiss),
      minimum = if (is.numeric(x) && length(nonmiss)) min(nonmiss) else NA_real_,
      maximum = if (is.numeric(x) && length(nonmiss)) max(nonmiss) else NA_real_
    )
  }), fill = TRUE)

  pairs = if (length(features) < 2L) list() else utils::combn(features, 2L, simplify = FALSE)
  pairwise = if (!length(pairs)) {
    data.table::data.table(
      feature_1 = character(),
      feature_2 = character(),
      kind_1 = character(),
      kind_2 = character(),
      method = character(),
      association = numeric(),
      n_complete = integer()
    )
  } else data.table::rbindlist(lapply(pairs, function(pair) {
    a = dt[[pair[[1L]]]]
    b = dt[[pair[[2L]]]]
    ka = .variable_kind(a, max_levels)
    kb = .variable_kind(b, max_levels)
    ok = !is.na(a) & !is.na(b)
    n_complete = sum(ok)
    if (ka == "continuous" && kb == "continuous") {
      method = "spearman"
      value = if (n_complete >= 3L) {
        suppressWarnings(stats::cor(a[ok], b[ok], method = "spearman"))
      } else {
        NA_real_
      }
    } else if (ka == "categorical" && kb == "categorical") {
      method = "cramers_v"
      value = .cramers_v(as.factor(a[ok]), as.factor(b[ok]))
    } else {
      method = "eta_squared"
      if (ka == "continuous") {
        value = .eta_squared(as.numeric(a), as.factor(b))
      } else {
        value = .eta_squared(as.numeric(b), as.factor(a))
      }
    }
    data.table::data.table(
      feature_1 = pair[[1L]],
      feature_2 = pair[[2L]],
      kind_1 = ka,
      kind_2 = kb,
      method = method,
      association = as.numeric(value),
      n_complete = n_complete
    )
  }), fill = TRUE)

  list(
    support = support,
    pairwise = pairwise,
    limitations = c(
      "Associations are descriptive and do not identify causal relations.",
      "Eta-squared is directional in coding but is reported as a mixed-type association magnitude.",
      "Observed support cannot establish support in an unobserved target population."
    )
  )
}
