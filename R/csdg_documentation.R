#' Define CSDG analysis cards and configuration
#'
#' These constructors create explicit, serializable cards that define the claim, measurement,
#' explanation semantics, and any claim-specific criteria before results are interpreted.
#' CSDG criteria have no universal substantive defaults and require a recorded source and rationale when they are
#' used for adjudication.
#'
#' @param id Stable claim identifier.
#' @param statement Claim to evaluate.
#' @param claim_type One or more controlled claim types.
#' @param target Prediction target or explanation target, including a horizon when relevant.
#' @param semantics Intended interpretation of the claim: a fitted-model description, hypothetical model query,
#'   causal claim, or recourse claim.
#' @param unit Unit of observation or inference.
#' @param population Target population.
#' @param analytic_distribution Distribution represented by the analysis data.
#' @param model_scope Scope over fitted models.
#' @param setting_scope Scope over cohorts, sites, countries, times, or settings.
#' @param scientific_use Intended scientific use of the result.
#' @param explanation_design Explanation design or comparison to which the claim is restricted.
#' @param claim_level Inference level of the claim: functional, predictive, or substantive.
#' @param use_claim Whether the claim additionally asserts adequacy for an audience, workflow, implementation, or use.
#' @param claim_version Stable version label for the claim.
#' @param parent_claim_id Identifier of the parent claim when the current claim is a revision.
#' @param revision_relation Declared relationship to the parent claim across all six claim coordinates.
#' @param intended_users Intended audience or users.
#' @param action Action informed by a decision claim.
#' @param thresholds Prespecified decision or diagnostic thresholds.
#' @param consequences Consequences of correct and incorrect decisions.
#' @param subgroup_variables Prespecified variables for subgroup auditing.
#' @param confirmatory Whether the claim was prospectively specified.
#' @param notes Free-text notes.
#' @param outcome Outcome name and measurement information.
#' @param predictors Model predictors.
#' @param data_source Source of the data.
#' @param sample_definition Inclusion, exclusion, and analytic-sample definition.
#' @param time_index Measurement or prediction time information.
#' @param outcome_scale Outcome scale and coding.
#' @param missingness Documented missingness and handling.
#' @param preprocessing Documented preprocessing pipeline.
#' @param verification Named list with `status`, `artifact`, `reviewer`, and `notes` fields.
#' @param audit_variables Variables reserved for auditing rather than modeling.
#' @param weights Sampling or analytic weights, if any.
#' @param clusters Clustering variables, if any.
#' @param method_ids Controlled explanation-method identifiers.
#' @param scope Global and/or local explanation scope.
#' @param feature_groups Named groups of features for grouped diagnostics.
#' @param perturbation Perturbation semantics for explanation methods.
#' @param background Background distribution or data.
#' @param local_cases Cases for local checks.
#' @param case_selection Whether local cases were prespecified or selected post hoc for communication.
#' @param aggregation Aggregation rule for explanations.
#' @param seed Master random seed.
#' @param resampling,performance,dependence,calibration,faithfulness,stability,generalization,subgroup,decision,export
#'   Named lists that override configuration defaults.
#' @param criteria Named criterion metadata keyed by the documented configuration path.
#'
#'   Every entry must contain nonempty `source` and `rationale` fields.
#' @param ... Explicit documented aliases for card constructors.
#'
#'   Unknown top-level fields supplied to `csdg_config()` are rejected.
#' @return A `CSDGClaim`, `CSDGMeasurement`, `CSDGExplanation`, or `CSDGConfig` object.
#' @name csdg_cards
NULL

#' Explicit out-of-fold resampling
#'
#' These functions create auditable train and assessment splits, store out-of-fold predictions and scores,
#' and optionally retain fold models.
#'
#' Fold-level variation is reported descriptively and is not treated as independent-sample uncertainty.
#'
#' @param task An [mlr3::Task].
#' @param group One cluster identifier per task row.
#' @param folds Number of folds.
#' @param repeats Number of repeated allocations.
#' @param strata Optional balancing strata.
#' @param seed Random seed.
#' @param learner An [mlr3::Learner], including any preprocessing pipeline.
#' @param resampling An [mlr3::Resampling] or a `CSDGGroupedResampling` object.
#' @param measures Measure keys or [mlr3::Measure] objects.
#' @param store_models Whether to store each fitted fold learner.
#' @param x A `CSDGResample` object.
#' @param collapse_repeats Whether to average repeated predictions at the observation level.
#' @return `csdg_grouped_resampling()` returns split definitions, `csdg_resample()` returns a
#'   `CSDGResample`, and the summary functions return data tables or lists of data tables.
#' @name csdg_resampling
NULL

#' CSDG diagnostic functions
#'
#' These functions compute mixed-type dependence, calibration, decision curves, held-out permutation
#' importance, local-surrogate fidelity, and selected-model subgroup diagnostics.
#'
#' @param data A data frame or data table.
#' @param features Feature names.
#' @param max_levels Threshold used to distinguish low-cardinality variables.
#' @param predictions A `CSDGResample` or prediction table.
#' @param task_type Either `"classif"` or `"regr"`.
#' @param positive Positive class label.
#' @param bins Number of calibration bins.
#' @param collapse_repeats Whether to average repeated predictions by row.
#' @param thresholds Decision thresholds.
#' @param x A `CSDGResample` object.
#' @param task An [mlr3::Task].
#' @param feature_groups Named groups for block permutation.
#' @param loss Prediction loss used for permutation importance.
#' @param repetitions Permutation repetitions per fold and feature group.
#' @param strata Optional within-stratum permutation variable.
#' @param cluster Optional row-aligned cluster identifier used for coherent assessment-sample permutation.
#' @param cluster_level_groups Optional feature-group names whose values are permuted coherently at cluster level.
#' @param batch_size Number of permuted assessment tables predicted together to reduce learner-call overhead.
#'
#'   Values above one require deterministic prediction for a fixed trained learner and row.
#'
#'   Additional stacking is limited to a target of 100,000 assessment rows per prediction call, but a single fold
#'   is never divided when it already exceeds that target.
#' @param seed Random seed.
#' @param learner A trained or trainable [mlr3::Learner].
#' @param cases Row ids or feature rows for local diagnostics.
#' @param background Background data that defines the local neighborhood.
#' @param n_perturb Number of synthetic neighborhood perturbations.
#'
#'   Local-surrogate fidelity is scored by deterministic cross-fitting over these perturbations.
#' @param kernel_width Local weighting-kernel width.
#' @param target_scale For classification, either `"response"` for probability or `"link"` for logit scale.
#' @param neighborhood_method Either `"synthetic"` or `"empirical_knn"` neighborhood construction.
#' @param empirical_neighbors Number of nearest training rows eligible for empirical-neighbor resampling.
#' @param train_if_needed Whether to train the learner on the full task when needed.
#' @param selection Whether local cases were prespecified or selected post hoc for communication.
#' @param subgroup An audit vector or row-id-keyed audit data.
#' @param row_id Observation identifiers for a subgroup vector.
#' @param threshold Classification cutoff.
#' @param min_n Minimum descriptive subgroup size.
#'   The package-level default of one applies no universal sample-size adequacy criterion.
#' @param cluster Optional cluster vector aligned with the collapsed prediction table, or a two-column data frame
#'   containing `row_id` and one cluster column.
#' @param bootstrap_strata Optional bootstrap-stratum vector aligned with the collapsed prediction table, or a
#'   two-column data frame containing `row_id` and one stratum column.
#'
#'   When supplied, clusters or analytic rows are resampled independently within each stratum.
#' @param bootstrap_repetitions Number of nonparametric bootstrap repetitions for conditional subgroup intervals and
#'   pairwise contrasts.
#' @param confidence_level Confidence level for percentile bootstrap intervals.
#' @return A named list of diagnostic tables and explicit limitations.
#'
#'   `csdg_calibration()` returns `summary`, `curve`, and `observation_level` tables.
#'
#'   For classification, `calibration_in_the_large` is the intercept from a logistic model with predicted log odds
#'   fixed as an offset, while `calibration_intercept` and `calibration_slope` come from a logistic model that estimates
#'   both coefficients.
#'
#'   For regression, `calibration_in_the_large` is the mean observed-minus-predicted outcome, while
#'   `calibration_intercept` and `calibration_slope` come from regressing observed outcomes on predictions.
#'
#'   For `csdg_local_surrogate()` and `csdg_oof_local_surrogate()`, `weighted_r2` and `weighted_rmse` are cross-fitted
#'   scores over perturbations, and the original case is excluded from those aggregate perturbation scores.
#'   `target_case_absolute_error` separately scores the original case using a surrogate fitted without that case.
#'
#'   The `apparent_weighted_r2` and `apparent_weighted_rmse` fields and returned coefficients describe a separate
#'   full-neighborhood refit rather than cross-fitted performance.
#' @name csdg_diagnostics
NULL

#' Model and setting generalization diagnostics
#'
#' These functions separate robustness across near-equivalent models from predictive generalization across
#' prespecified setting shifts.
#'
#' @param task An [mlr3::Task].
#' @param learners Named candidate learners.
#' @param resampling Identical instantiated splits for all learners.
#' @param primary_measure Primary measure key or object.
#' @param tolerance_absolute,tolerance_relative Prespecified near-equivalence tolerances.
#'   For `csdg_near_equivalence_sensitivity()`, either value may be a vector defining a sensitivity grid;
#'   vector lengths must be one or equal to the number of scenarios.
#' @param reference_learner Optional learner name around which the tolerance is defined.
#'   When omitted, the direction-specific best candidate is the reference.
#' @param tolerance_source Source of the supplied near-equivalence tolerance.
#' @param tolerance_rationale Rationale linking the supplied tolerance to the claim.
#' @param x A `CSDGRashomon` object or candidate table with unique `learner_name` and finite `mean_score` columns.
#' @param scenario_id Optional unique labels for the tolerance scenarios.
#' @param seed Random seed.
#' @param store_models Whether to store fitted learners.
#' @param rashomon A `CSDGRashomon` object.
#' @param pfi Named permutation-importance results for accepted learners.
#' @param top_k Number of top-ranked feature groups.
#' @param learner Focal learner.
#' @param group One setting identifier per task row.
#' @param measures Performance measures.
#' @param scores Setting-level score table.
#' @param measure Column to evaluate.
#' @param direction Whether higher or lower scores are better.
#' @param minimum_transport_score Minimum acceptable score for maximized metrics.
#' @param maximum_transport_score Maximum acceptable score for minimized metrics.
#' @param criterion_source Source of a supplied transport criterion.
#' @param criterion_rationale Rationale linking a supplied transport criterion to the claim.
#' @return Structured candidate, agreement, or leave-one-group-out results.
#' @name csdg_generalization
NULL

#' Build a claim-scoped diagnostic gate plan
#'
#' Maps a prespecified claim to Gates G0a through G7b and records why each gate is required or optional.
#'
#' @param claim A `CSDGClaim` object.
#' @param measurement A `CSDGMeasurement` object.
#' @param explanation A `CSDGExplanation` object or `NULL`.
#' @return A `CSDGGatePlan` data table.
#' @name csdg_gate_plan
NULL

#' Run a claim-scoped diagnostic audit
#'
#' Executes Gates G0a through G7b according to the claim-scoped gate plan.
#'
#' Individual gate errors are retained in the report card so that one failure does not erase other diagnostic
#' evidence.
#'
#' @param task An [mlr3::Task].
#' @param learner Focal learner with preprocessing embedded in its pipeline.
#' @param claim A `CSDGClaim` object.
#' @param measurement A `CSDGMeasurement` object.
#' @param explanation A `CSDGExplanation` object or `NULL`.
#' @param config A `CSDGConfig` object.
#' @param resampling An [mlr3::Resampling] or `CSDGGroupedResampling` object.
#' @param candidate_learners Optional candidates for model-generalization checks.
#' @param setting_group Optional setting labels for leave-one-setting-out refits.
#' @param subgroup Optional row-id-keyed subgroup audit data.
#' @param local_cases Prespecified local cases.
#' @param pfi_strata Optional row-aligned strata within which PFI donors are permuted.
#' @param pfi_cluster Optional row-aligned assessment-cluster identifiers.
#' @param pfi_cluster_level_groups Optional feature-group names whose values are permuted coherently at cluster level.
#' @param evidence Named external gate evidence.
#' @param run_gates Optional gate ids to execute.
#'   This controls computation but does not change claim-derived applicability or evidence roles.
#' @param output_dir Optional bundle-export directory.
#' @param ... Explicit aliases or additional runner inputs accepted by `csdg_audit()`.
#' @return A `CSDGResult` object.
#' @name csdg_audit
NULL

#' Report, export, and plot CSDG results
#'
#' Creates manuscript-facing report cards, diagnostic plots, and an atomic audit bundle containing cards, gate
#' evidence, provenance, uncertainty boundaries, and an MD5 manifest.
#' CSDG report cards keep applicability, evidence role, availability, result direction, criterion provenance,
#' including distinct criterion source and rationale fields, materiality, adjudication basis, and claim consequence
#' in separate fields.
#' They never consume legacy `GateResult` statuses as CSDG claim decisions.
#'
#' @param x A `CSDGResult`, gate result, or supported evidence object.
#' @param path Parent export directory.
#' @param prefix Stable bundle prefix.
#' @param include_models Whether to serialize fitted models.
#' @param include_predictions Whether to retain observation-level predictions.
#' @param gate Gate id to plot.
#' @param type Metric or plot subtype.
#' @param base_size Base text size in points for `csdg_plot()` output.
#' @param style Either `"color"` for the default blue-red rendering or `"monochrome"` for an achromatic
#'   print-oriented rendering with redundant shapes and line types.
#' @param ... Additional plotting arguments, which are currently unsupported.
#' @return A report table, claim report, invisibly returned bundle path, or [ggplot2::ggplot] object.
#' @name csdg_reporting
NULL
