#' @title mlr3autoiml: Claim-Scoped Diagnostic Gates for Interpretable Machine Learning
#'
#' @description
#' `mlr3autoiml` implements Claim-Scoped Diagnostic Gates (CSDG) for [mlr3][mlr3::mlr3-package] tasks and
#' learners.
#'
#' The CSDG workflow prespecifies claims, measurement and preprocessing semantics, explanation semantics, and
#' adequacy criteria before coordinating out-of-fold diagnostic evidence.
#'
#' The package also retains the established [AutoIML] workflow for compatibility with earlier analyses.
#'
#' @seealso
#' \itemize{
#'   \item [csdg_audit()] for the claim-scoped diagnostic workflow.
#'   \item [csdg_claim()] for claim, measurement, explanation, and configuration cards.
#'   \item [AutoIML] for the established AutoIML orchestrator.
#'   \item [autoiml()] for a convenience wrapper.
#'   \item [report_card()] for audit trail summary.
#' }
#'
#' @importFrom data.table as.data.table copy data.table fcase fifelse frank is.data.table melt rbindlist rleid
#'   setcolorder setnames setorder setorderv setattr uniqueN := .N .SD .I
#' @importFrom ggplot2 aes annotate annotation_custom coord_cartesian coord_equal coord_fixed element_blank element_text
#'   expand_limits expansion
#'   facet_grid facet_wrap geom_abline geom_blank geom_col geom_errorbar geom_hline geom_label geom_line geom_point
#'   geom_rect
#'   geom_ribbon
#'   geom_segment
#'   geom_smooth geom_step geom_text geom_tile geom_vline ggplot ggplotGrob labs margin scale_color_identity
#'   scale_color_manual
#'   scale_fill_gradient scale_fill_gradient2 scale_fill_manual scale_linetype_manual scale_shape_manual scale_size_area
#'   scale_x_continuous scale_x_discrete scale_y_continuous scale_y_discrete
#'   guides guide_legend position_nudge theme theme_void
#' @importFrom grid unit
#' @importFrom stats cor quantile qnorm sd var coef glm predict lm median model.matrix setNames reorder
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_atomic assert_character assert_choice assert_class assert_data_frame assert_flag
#'   assert_int assert_integerish assert_list assert_logical assert_multi_class assert_number assert_numeric
#'   assert_string assert_subset assert_true
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom checkmate %??%
#' @importFrom mlr3misc map_dtr
#' @importFrom mlr3measures auc bbrier logloss mbrier
#' @importFrom utils combn head modifyList
#' @importFrom tools toTitleCase
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check notes for data.table non-standard evaluation
utils::globalVariables(c(
  # Common data.table symbols
  ".", ".data", ".N", ".SD", ".I", ".GRP", ".BY", ".EACHI", ":=", "..feature_names", "..features",
  # Column names used across files
  "phi", "abs_phi", "feature", "feature_value", "feature_label", "feature_f",
  "class_label", "row_id", "mean_abs_phi", "value_scaled",
  "start", "end", "sign", "x_lab", "hjust",
  "importance", "learner_id", "measure_id", "mean", "sd", "se", "ci_low", "ci_high",
  "in_rashomon", "rashomon_threshold",
  "ice_sd_mean", "hstat", "grid_n", "sample_n",
  "type", "id", "group", "n", "N", "logloss", "measure", "truth",
  "status", "gate_id", "gate_name", "pdr", "summary",
  "purpose", "quick_start", "semantics", "stakes",
  "claim_global", "claim_local", "claim_decision",
  "missing_rate", "iteration", "value", "rank", "pred_range", "learner",
  "x_mid", "y_mean", "bin", "threshold", "net_benefit", "nb_treat_all", "nb_treat_none",
  "mean_importance", "flag_off_support", "ratio_to_baseline", "region_id",
  "feature1", "feature2", "pair", "x", "y", "yhat", "m", "shap_mode",
  "bounds", "branch_label", "child_frac", "child_half_height", "child_kind",
  "child_order", "depth", "feature_plot", "fill_key", "flag", "gain", "gain_ratio",
  "group_var", "heterogeneity_reduction", "label", "label_main", "label_stats",
  "label_text", "level", "line_dy", "line_label", "local_effect", "loss", "n_leaf",
  "n_parent", "nb_ci_high", "nb_ci_low", "node_kind", "null_quantile",
  "null_quantile.from_null", "null_risk", "observed_risk", "p_value", "parent_depth",
  "parent_frac", "parent_half_height", "path", "path_chr", "permutation",
  "pint_interaction", "root_loss", "rule_left", "rule_right", "semantics_label",
  "split_feature", "split_type", "split_value", "target_features", "terminal_loss",
  "threshold_pct", "total_loss", "x_child", "x_from", "x_label", "x_parent", "x_to",
  "y_child", "y_ci_high", "y_ci_low", "y_from", "y_global", "y_label", "y_parent",
  "y_region", "y_to", "metric_label__",
  # CSDG data.table symbols
  "accepted", "acceptance_label", "acceptance_limit", "association", "association_strength", "audit_label",
  "audit_variable", "baseline_loss",
  "below_minimum_n", "best_score", "calibration_in_the_large", "calibration_intercept", "calibration_slope",
  "bootstrap_cluster", "bootstrap_stratum", "case_id", "case_label", "ci_high.new", "ci_low.new", "coefficient",
  "complete",
  "completed_at", "criterion", "difference", "direction", "estimate", "estimate_1", "estimate_2",
  "estimate_difference", "gate_order__",
  "distance_from_best", "effective_weight", "evidence_scope", "feature_1", "feature_2",
  "feature_1_index", "feature_2_index", "feature_group", "features", "fold", "gate_label", "held_out_group",
  "intersection", "kind", "kind_1", "kind_2", "label_hjust", "label_x", "learner_1", "learner_2",
  "learner_label", "learner_name", "maximum", "maximum_association", "maximum_distance", "mean_score", "method",
  "metric", "minimum", "n_assessment", "n_complete",
  "n_folds", "n_iterations", "n_missing", "n_pv", "n_strata", "n_train", "n_unique", "observed",
  "operator", "passed", "permutation_repetition", "permuted_loss", "plausible_value", "plot_row", "plot_value",
  "positive_fraction", "predicted", "prediction", "probability", "q10", "q10_importance", "q90",
  "q90_importance", "rank", "repetition", "required", "required_components", "r_squared", "score_label",
  "started_at", "status_label",
  "storage", "stratum", "subgroup", "subgroup_1", "subgroup_2", "subgroup_label", "term", "tie",
  "tolerance_absolute",
  "tolerance_relative", "top_k", "trigger", "uncertainty_label", "union", "weighted_r2",
  "value_label", "weighted_rmse",
  # Table-oriented CSDG plotting symbols
  "absolute_ale2d__", "accepted_label", "ale2d", "case_label__", "ci_high__", "ci_high__.new", "ci_low__",
  "ci_low__.new",
  "connector_x__", "cumulative_probability", "estimate__", "estimable__", "feature_label", "feature_name__", "high__",
  "gate_factor__", "gate_label__", "group_factor__", "group_id__", "group_label__", "label_color",
  "label_hjust__", "label_x__", "label_y__", "learner_id__", "learner_label__", "learner_name__", "low__",
  "mean_rank", "net_benefit", "observed_value", "pair_label", "performance_fraction__", "reversal_fraction__",
  "difference__", "performance_display__", "plot_row__", "plot_score__", "prediction_range", "predicted_value",
  "rank__", "reversal_display__",
  "reference_estimate__", "scale_max__", "scale_min__", "segment_id__", "sign_color__", "sign_label__", "status_key",
  "strategy", "threshold__", "value__",
  "axis_label", "bin_left", "bin_right", "h_statistic", "held_out_label", "high__.new", "low__.new",
  "median_prediction", "order_value", "q05_prediction", "q25_prediction", "q75_prediction", "q95_prediction",
  "maximum_penalty", "mean_penalty", "minimum_penalty",
  "outcome_id", "outcome_label", "statistic", "supported", "x_left", "x_right", "x1_left", "x1_right",
  "x2_bottom", "x2_top",
  # Full-fit model-diagnostic symbols
  "absolute_phi", "absolute_rank", "ale", "ale_bootstrap_mean", "ale_bootstrap_median", "ale_lower",
  "ale_upper", "bootstrap_id", "bootstrap_interval_n_median", "bootstrap_refits_successful",
  "bootstrap_replicates_requested", "bootstrap_support_rate", "confidence_level", "global_rank", "grid_scope",
  "ice_curve_id", "interval_type", "limitation", "mean_absolute_phi", "message", "model_refit", "n_bootstrap",
  "n_cell", "n_interval", "phi_monte_carlo_se", "phi_var", "range_threshold", "reference_scope",
  "replicate_supported", "resampling_unit", "row_index", "row_position", "support_threshold",
  # Public evidence-extension API symbols
  "..finite_columns", "..stable_columns", "analysis_role", "case_below_threshold", "claim", "claim_order",
  "claim_plot",
  "conclusion", "conclusion_plot", "evidence_needed", "evidence_plot", "kernel_weight_effective_n",
  "kernel_width", "median_weighted_r2", "nearest_training_distance", "perturbation_replicate", "perturbation_seed",
  "proportion_training_within_kernel", "proximity_quartile", "row_fill", "row_y", "status_plot_label",
  "tenth_nearest_training_distance", "uncertainty_semantics", "valid_average_rank", "valid_bounds",
  # Review-extension diagnostic symbols
  "lower", "upper", "practical_reversal", "exact_reversal", "reference_tie", "comparison_tie",
  "weighted_mae", "maximum_absolute_error", "target_case_absolute_error", "target_scale",
  "neighborhood_method", "median_absolute_rank", "calibration_cluster__", "calibration_stratum__",
  "bootstrap_replicate", "monte_carlo_se", "comparison_importance", "reference_importance",
  "excluded_from_rank_comparison", "reference_rank", "comparison_rank", "normalized_importance",
  "reference_oof_loss", "matched_exclusion_loss", "matched_exclusion_penalty", "scenario_order",
  "uncertainty_scope", "rank_after_value_scale_aggregation", "mean_estimate", "distance_from_reference",
  "cluster_id", "perturbation_seed_index", "crossfit_seed_index", "n_perturbation_seeds",
  "n_crossfit_seeds", "grid_mean", "perturbation_marginal_sd", "crossfit_marginal_sd",
  "interaction_rms", "full_grid_sd", "perturbation_sum_squares_fraction",
  "crossfit_sum_squares_fraction", "interaction_sum_squares_fraction", "metric_order"
))
