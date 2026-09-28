.local_fidelity_randomness_decomposition = function(values) {
  perturbation_means = values[, .(marginal_mean = mean(value)), by = perturbation_seed_index]
  crossfit_means = values[, .(marginal_mean = mean(value)), by = crossfit_seed_index]
  grand_mean = mean(values$value)
  n_perturbation = nrow(perturbation_means)
  n_crossfit = nrow(crossfit_means)
  perturbation_sum_squares = n_crossfit * sum((perturbation_means$marginal_mean - grand_mean)^2)
  crossfit_sum_squares = n_perturbation * sum((crossfit_means$marginal_mean - grand_mean)^2)
  decomposed = merge(
    values,
    perturbation_means,
    by = "perturbation_seed_index",
    all.x = TRUE,
    sort = FALSE
  )
  setnames(decomposed, "marginal_mean", "perturbation_marginal_mean")
  decomposed = merge(
    decomposed,
    crossfit_means,
    by = "crossfit_seed_index",
    all.x = TRUE,
    sort = FALSE
  )
  setnames(decomposed, "marginal_mean", "crossfit_marginal_mean")
  interaction_residual = decomposed$value - decomposed$perturbation_marginal_mean -
    decomposed$crossfit_marginal_mean + grand_mean
  interaction_sum_squares = sum(interaction_residual^2)
  total_sum_squares = sum((values$value - grand_mean)^2)
  positive_total = total_sum_squares > .Machine$double.eps * max(1, sum(values$value^2))

  data.table(
    n_grid_cells = nrow(values),
    n_perturbation_seeds = n_perturbation,
    n_crossfit_seeds = n_crossfit,
    grid_mean = grand_mean,
    perturbation_marginal_sd = stats::sd(perturbation_means$marginal_mean),
    perturbation_marginal_range = diff(range(perturbation_means$marginal_mean)),
    crossfit_marginal_sd = stats::sd(crossfit_means$marginal_mean),
    crossfit_marginal_range = diff(range(crossfit_means$marginal_mean)),
    interaction_rms = sqrt(mean(interaction_residual^2)),
    full_grid_sd = stats::sd(values$value),
    full_grid_range = diff(range(values$value)),
    perturbation_sum_squares = perturbation_sum_squares,
    crossfit_sum_squares = crossfit_sum_squares,
    interaction_sum_squares = interaction_sum_squares,
    total_sum_squares = total_sum_squares,
    perturbation_sum_squares_fraction = if (positive_total) {
      perturbation_sum_squares / total_sum_squares
    } else {
      NA_real_
    },
    crossfit_sum_squares_fraction = if (positive_total) crossfit_sum_squares / total_sum_squares else NA_real_,
    interaction_sum_squares_fraction = if (positive_total) interaction_sum_squares / total_sum_squares else NA_real_
  )
}

.summarize_local_fidelity_randomness = function(grid, case_map) {
  metric_columns = c(
    "weighted_r2", "weighted_rmse", "weighted_mae", "maximum_absolute_error",
    "target_case_absolute_error"
  )
  identity_columns = c(
    "case_label", "iteration", "repetition", "fold", "perturbation_seed_index",
    "crossfit_seed_index"
  )
  long = melt(
    grid,
    id.vars = identity_columns,
    measure.vars = metric_columns,
    variable.name = "metric",
    value.name = "value"
  )
  long[, metric := as.character(metric)]
  cases = long[, .local_fidelity_randomness_decomposition(.SD), by = .(
    case_label,
    iteration,
    repetition,
    fold,
    metric
  )]
  public_case_map = case_map[, setdiff(names(case_map), "row_id"), with = FALSE]
  cases = merge(
    cases,
    public_case_map,
    by = c("case_label", "iteration", "repetition", "fold"),
    all.x = TRUE,
    sort = FALSE
  )
  setorder(cases, iteration, case_label, metric)
  finite_median = function(x) {
    finite = x[is.finite(x)]
    if (length(finite)) stats::median(finite) else NA_real_
  }
  summary = cases[, .(
    n_cases = .N,
    n_perturbation_seeds = unique(n_perturbation_seeds),
    n_crossfit_seeds = unique(n_crossfit_seeds),
    median_grid_mean = stats::median(grid_mean),
    median_perturbation_marginal_sd = stats::median(perturbation_marginal_sd),
    q25_perturbation_marginal_sd = .local_fidelity_quantile(perturbation_marginal_sd, 0.25),
    q75_perturbation_marginal_sd = .local_fidelity_quantile(perturbation_marginal_sd, 0.75),
    median_crossfit_marginal_sd = stats::median(crossfit_marginal_sd),
    q25_crossfit_marginal_sd = .local_fidelity_quantile(crossfit_marginal_sd, 0.25),
    q75_crossfit_marginal_sd = .local_fidelity_quantile(crossfit_marginal_sd, 0.75),
    median_interaction_rms = stats::median(interaction_rms),
    median_full_grid_sd = stats::median(full_grid_sd),
    median_perturbation_sum_squares_fraction = finite_median(perturbation_sum_squares_fraction),
    median_crossfit_sum_squares_fraction = finite_median(crossfit_sum_squares_fraction),
    median_interaction_sum_squares_fraction = finite_median(interaction_sum_squares_fraction),
    n_zero_variation_cases = sum(!is.finite(perturbation_sum_squares_fraction)),
    uncertainty_semantics = paste(
      "Balanced deterministic decomposition of a complete crossed seed grid;",
      "computational variation only, not a confidence interval or hypothesis test"
    )
  ), by = metric]
  summary[, metric_order := match(metric, metric_columns)]
  setorder(summary, metric_order)
  summary[, metric_order := NULL]
  list(cases = cases[], summary = summary[])
}

#' Separate perturbation and cross-fit variation in a local-fidelity audit
#'
#' Evaluates a complete crossed grid of perturbation seeds and surrogate cross-fit seeds for held-out cases.
#' The balanced grid separates marginal variation attributable to neighborhood generation, marginal variation
#' attributable to surrogate fold assignment, and their interaction without treating any component as sampling or
#' model-training uncertainty.
#' The section "Local surrogate" states exactly what is fitted.
#'
#' @inheritSection csdg_diagnostics Local surrogate
#' @param x A `CSDGResample` with stored fold models.
#' @param cases Unique task row ids that each occur in exactly one assessment split.
#' @param perturbation_seeds Vector or case-by-seed matrix containing at least two distinct perturbation seeds per case.
#' @param crossfit_seeds Vector or case-by-seed matrix containing at least two distinct cross-fit seeds per case.
#' @param kernel_width Positive kernel width.
#' @param n_perturb Number of perturbations generated for each crossed seed cell.
#' @param crossfit_folds Number of deterministic surrogate cross-fitting folds.
#' @param target_scale For classification, either `"response"` for probability or `"link"` for logit scale.
#' @param neighborhood_method Either synthetic independent perturbations or empirical k-nearest-neighbor resampling.
#' @param empirical_neighbors Number of nearest training rows eligible for empirical-neighbor resampling.
#' @param case_labels Optional unique pseudonymous labels in the same order as `cases`.
#' @param case_metadata Optional data frame with one row per case whose columns are copied to case-level outputs.
#'
#' @return A named list containing the complete crossed `grid`, case-by-metric `cases` decomposition, aggregate
#'   `summary`, fold-training `support`, the private `case_map`, and explicit evaluation and limitation metadata.
#' @export
csdg_local_fidelity_randomness_audit = function(
    x,
    cases,
    perturbation_seeds = 20260201L + 0:4,
    crossfit_seeds = 30260201L + 0:4,
    kernel_width = 0.75,
    n_perturb = 500L,
    crossfit_folds = 5L,
    target_scale = c("response", "link"),
    neighborhood_method = c("synthetic", "empirical_knn"),
    empirical_neighbors = n_perturb,
    case_labels = NULL,
    case_metadata = NULL) {
  assert_number(kernel_width, lower = .Machine$double.eps, finite = TRUE)
  assert_int(n_perturb, lower = 50L)
  assert_int(crossfit_folds, lower = 2L, upper = n_perturb)
  assert_int(empirical_neighbors, lower = 2L)
  target_scale = match.arg(target_scale)
  neighborhood_method = match.arg(neighborhood_method)
  if (identical(x$task_type, "regr") && identical(target_scale, "link")) {
    .csdg_stop("`target_scale = \"link\"` is available only for binary classification.")
  }
  if (!is.null(case_metadata)) {
    reserved = intersect(names(case_metadata), c(
      "metric", "value", "randomness_cell", "perturbation_seed_index", "crossfit_seed_index",
      "n_grid_cells", "n_perturbation_seeds", "n_crossfit_seeds", "grid_mean",
      "perturbation_marginal_sd", "perturbation_marginal_range", "crossfit_marginal_sd",
      "crossfit_marginal_range", "interaction_rms", "full_grid_sd", "full_grid_range",
      "perturbation_sum_squares", "crossfit_sum_squares", "interaction_sum_squares",
      "total_sum_squares", "perturbation_sum_squares_fraction", "crossfit_sum_squares_fraction",
      "interaction_sum_squares_fraction"
    ))
    if (length(reserved)) {
      .csdg_stop("`case_metadata` uses randomness-audit reserved columns: %s.", paste(reserved, collapse = ", "))
    }
  }
  case_map = .local_fidelity_prepare_cases(x, cases, case_labels, case_metadata)
  perturbation_seed_matrix = .local_fidelity_seed_matrix(perturbation_seeds, nrow(case_map))
  crossfit_seed_matrix = .local_fidelity_seed_matrix(crossfit_seeds, nrow(case_map))

  had_random_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_random_seed) {
    caller_random_seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_random_seed) {
      assign(".Random.seed", caller_random_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      remove(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  evaluated = lapply(seq_len(nrow(case_map)), function(index) {
    indices = data.table(
      perturbation_seed_index = rep(
        seq_len(ncol(perturbation_seed_matrix)),
        each = ncol(crossfit_seed_matrix)
      ),
      crossfit_seed_index = rep(
        seq_len(ncol(crossfit_seed_matrix)),
        times = ncol(perturbation_seed_matrix)
      )
    )
    perturbation_seed = perturbation_seed_matrix[index, indices$perturbation_seed_index]
    crossfit_seed = crossfit_seed_matrix[index, indices$crossfit_seed_index]
    result = .local_fidelity_case_result(
      x = x,
      case_map = case_map[index],
      seeds = perturbation_seed,
      crossfit_seeds = crossfit_seed,
      kernel_widths = kernel_width,
      primary_kernel_width = kernel_width,
      n_perturb = n_perturb,
      crossfit_folds = crossfit_folds,
      fidelity_threshold = NULL,
      target_scale = target_scale,
      neighborhood_method = neighborhood_method,
      empirical_neighbors = empirical_neighbors
    )
    result$replicates[, `:=`(
      randomness_cell = seq_len(.N),
      perturbation_seed_index = indices$perturbation_seed_index,
      crossfit_seed_index = indices$crossfit_seed_index
    )]
    list(grid = result$replicates, support = result$support)
  })
  grid = rbindlist(lapply(evaluated, `[[`, "grid"))
  support = rbindlist(lapply(evaluated, `[[`, "support"))
  expected_cells = nrow(case_map) * ncol(perturbation_seed_matrix) * ncol(crossfit_seed_matrix)
  keys = grid[, .N, by = .(case_label, perturbation_seed_index, crossfit_seed_index)]
  if (nrow(grid) != expected_cells || nrow(keys) != expected_cells || any(keys$N != 1L) ||
      nrow(support) != nrow(case_map) || anyDuplicated(support$case_label)) {
    .csdg_stop("The case-by-perturbation-seed-by-cross-fit-seed grid is incomplete or duplicated.")
  }
  summaries = .summarize_local_fidelity_randomness(grid, case_map)

  list(
    grid = grid[],
    cases = summaries$cases,
    summary = summaries$summary,
    support = support[],
    case_map = case_map[],
    evaluation = list(
      method = "complete crossed computational-variation decomposition",
      n_perturbation_seeds = ncol(perturbation_seed_matrix),
      n_crossfit_seeds = ncol(crossfit_seed_matrix),
      grid_cells_per_case = ncol(perturbation_seed_matrix) * ncol(crossfit_seed_matrix),
      decomposition = "balanced two-way sums of squares with one deterministic result per crossed seed cell",
      kernel_width = kernel_width,
      n_perturb = n_perturb,
      crossfit_folds = crossfit_folds,
      target_scale = target_scale,
      neighborhood_method = neighborhood_method
    ),
    limitations = c(
      "The decomposition describes computation under the declared seeds, neighborhood, and surrogate design.",
      "Seed effects are fixed deterministic contrasts, not random-effect estimates or confidence intervals.",
      "The interaction component includes nonadditivity between the selected perturbation and cross-fit seed grids.",
      "No component includes predictive-model refitting, population sampling, or model-selection uncertainty.",
      "Results do not generalize beyond the audited held-out cases or establish causal or actionable explanations."
    )
  )
}
