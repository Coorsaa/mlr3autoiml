#' Define CSDG analysis cards and configuration
#'
#' These constructors record the claim, its scope, the measurement, and the explanation before results are
#' interpreted (Steps 1 and 2 of the article), and the configuration of the diagnostics.
#' The scope of a claim has six elements: quantity, model, procedure, data, meaning, and use.
#' CSDG criteria have no universal substantive defaults and require a recorded source and rationale when they are
#' used to judge a property.
#'
#' `csdg_claim()` accepts each scope element under its name in the article (`quantity`, `model`, `procedure`,
#' `data`, `meaning`, `use`) or under the legacy field name of versions up to 0.1.5 (`target`, `model_scope`,
#' `explanation_design`, `analytic_distribution`, `semantics`, `scientific_use`); supplying both names with
#' different values is an error. The card stores the elements in `scope` (in the order of the article) and, for
#' compatibility, in the legacy fields.
#'
#' @param id Stable claim identifier.
#' @param statement The claim: the conclusion that the researcher intends to report, written as it would appear in
#'   the article.
#' @param claim_type One or more claim types: `"predictive_performance"`, `"calibration"`, `"global_explanation"`,
#'   `"local_explanation"`, `"subgroup"`, `"model_generalization"`, `"setting_generalization"`, `"decision"`.
#'   Together with the scope, they determine the gate plan ([csdg_gate_plan()]).
#' @param quantity,target For `csdg_claim()`, the quantity: which quantity (estimand) does the claim interpret?
#'   `target` is its legacy name. For `csdg_explanation()`, `target` is the explained model output (for example,
#'   the predicted probability).
#' @param semantics Deprecated (0.1.5): use `meaning`. Legacy values `"fitted_model_description"` and
#'   `"hypothetical_model_query"` correspond to `meaning = "model_description"`, and `"causal"` and `"recourse"` to
#'   `"causal_claim"`.
#' @param meaning Meaning: model description, population claim, or causal claim (`"model_description"`,
#'   `"population_claim"`, `"causal_claim"`; default `"model_description"`). A population claim uses the model to
#'   learn how predictors relate to the outcome in the population that the sample represents; a causal claim is a
#'   population claim about what would happen if a predictor changed. The meaning is never inferred from
#'   `claim_level`.
#' @param unit Unit of observation or inference.
#' @param population Population that the data element refers to.
#' @param data,analytic_distribution Data: which sample, measures, and preprocessing? `analytic_distribution` is the
#'   legacy name.
#' @param model_scope Model: one fitted model, a learner, or several learners (`"fitted_model"`, `"learner"`,
#'   `"several_models"`, or `"unspecified"`). A learner is an algorithm with fixed settings and preprocessing;
#'   fitting it to data yields a fitted model. The dots alias `model` is accepted. Legacy values
#'   (`"selected_model"`, `"cross_fitted_pipeline"`, `"near_equivalent_models"`, `"model_class"`) are mapped with
#'   a deprecation warning.
#' @param setting_scope Setting of the claim: `"analytic_sample"` (the sampled setting) or a description of other
#'   cohorts, sites, countries, or times that the claim extends to.
#' @param use,scientific_use Use: what will the claim be used for? `scientific_use` is the legacy name.
#' @param procedure,explanation_design Procedure: how is the quantity estimated (estimator)?
#'   `explanation_design` is the legacy name.
#' @param claim_level Documentary level of the claim (`"functional"`, `"predictive"`, or `"substantive"`). It does
#'   not affect the gate plan; use `meaning` for population and causal claims.
#' @param use_claim Whether the claim asserts that intended users understand or benefit from the explanation (G7b).
#' @param claim_version Stable version label for the claim.
#' @param parent_claim_id Identifier of the predecessor when the claim is a revision.
#' @param revision_relation `"original"` or, for a revised claim, the relation of its scope to the predecessor's
#'   scope (`"same"`, `"narrower"`, `"broader"`, `"incomparable"`). Use [csdg_claim_relation()] to record the
#'   relation of each scope element and whether the revised claim follows from its predecessor.
#' @param intended_users Intended audience or users.
#' @param action Action informed by a decision claim.
#' @param thresholds Prespecified decision or diagnostic thresholds.
#' @param consequences Consequences of correct and incorrect decisions.
#' @param subgroup_variables Prespecified variables for subgroup auditing.
#' @param confirmatory Legacy documentary flag for prospective specification; it does not establish valid confirmation.
#' @param provenance Optional named list with `origin`, `date` (YYYY-MM-DD), `time_basis`, `selection_basis`,
#'   and character-vector `evidence_ids`.
#'   Origin is `"specified_before_results"` (prespecified: fixed before the relevant results were seen),
#'   `"retrospective_exploratory"` (exploratory: written after the results were seen; the assessment is labeled
#'   exploratory), or `"independently_confirmed"` (later tested on new data).
#'   Independent confirmation requires evidence identifiers; metadata cannot establish independence by itself.
#'   `NULL` means not recorded, and revisions do not inherit provenance automatically.
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
#'   Named lists that override configuration defaults; `faithfulness` holds the settings of the local-fidelity
#'   diagnostics (G4).
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
#' feature importance (PFI), cross-fitted local fidelity of local surrogates, and subgroup diagnostics.
#'
#' `csdg_fold_pfi()` computes held-out marginal PFI in each resampling iteration: the increase in the chosen loss on
#' the assessment rows when a feature group is permuted, averaged over `repetitions` permutations. If the resample
#' refits the learner in each fold, the fold average estimates learner PFI (the learner's fits to training sets of
#' the given size); the value of one fold describes that fitted model. `strata` restricts the permutation to rows
#' with the same stratum value (for example, a conditional permutation given another item). Use
#' [csdg_learner_pfi_interval()] for the corrected resampled interval of a learner average and
#' [csdg_pfi_mc_difference()] for the Monte Carlo rule.
#'
#' @section Local surrogate:
#' For one case, the package draws `n_perturb` perturbed points (default 500). In the default synthetic
#' neighborhood (`neighborhood_method = "synthetic"`), each feature is perturbed independently of the others: a
#' numeric feature is drawn from a normal distribution centered at the case value with standard deviation 0.25 times
#' the feature's standard deviation in the background data, truncated to the background range (and rounded for
#' integer features); a factor, logical, or character feature keeps the case value with probability 0.70 and is
#' otherwise drawn from the background (factors and logicals with their background frequencies, character values
#' uniformly over the observed values). The alternative `neighborhood_method = "empirical_knn"` resamples, with
#' replacement, from the `empirical_neighbors` background rows nearest to the case (default: `n_perturb` rows) and
#' fills missing values with the background median or mode. For held-out diagnostics
#' (`csdg_oof_local_surrogate()`, [csdg_local_fidelity_audit()]), the background is the training data of the fold
#' model that did not see the case; otherwise it is `background` or all task rows. Missing case values are filled
#' with the background median or mode.
#'
#' The case is added to its perturbations, and the model's prediction is computed for all points, on the
#' probability scale (`target_scale = "response"`) or on the log-odds scale (`"link"`, probabilities clipped to
#' the interval from 1e-15 to 1 - 1e-15); for regression, on the outcome scale. The distance of a point to the case
#' is the root mean square of per-feature distances: for a numeric feature, the difference divided by the background
#' standard deviation; for other features, 0 if equal to the case value and 1 otherwise. Points are weighted with the
#' Gaussian kernel \eqn{w = \exp(-d^2 / h^2)}, where \eqn{h} is `kernel_width` (default 0.75); the case has weight 1.
#'
#' The surrogate is a weighted ridge regression on the design matrix `model.matrix(~ .)` of all features (an
#' intercept, numeric features, and R's default contrasts for factors and character features: treatment coding for
#' unordered factors and polynomial contrasts, `contr.poly()`, for ordered factors).
#' Weights are rescaled to mean 1; each non-intercept column is centered and scaled by its weighted mean and
#' weighted standard deviation, and columns without weighted variance are dropped (coefficient 0).
#' The intercept is not penalized; every other standardized column receives the penalty
#' \eqn{\lambda = m \max(10^{-4}, 0.01 a / \max(n_{eff}, 1))}, where \eqn{m} is the number of points used for the
#' fit, \eqn{a} the number of non-constant design columns (intercept excluded), and
#' \eqn{n_{eff} = (\sum w)^2 / \sum w^2} the Kish effective sample size of the weights. The coefficients solve
#' \eqn{(X^\top W X + \Lambda) b = X^\top W y} and are transformed back to the original scale. The penalty is a
#' fixed rule, not tuned.
#'
#' Fidelity is cross-fitted: the perturbed points are split at random into `crossfit_folds` folds of nearly equal
#' size (default 5; `csdg_local_surrogate()` and `csdg_oof_local_surrogate()` always use 5); each fold is predicted
#' by a surrogate fitted to the case and the perturbed points of the other folds, so no perturbed point is predicted
#' by a surrogate fitted to it (with 500 points, each surrogate is fitted to 400 perturbed points plus the case).
#' Weighted \eqn{R^2} (relative to the kernel-weighted mean of the model's predictions), weighted RMSE, weighted
#' MAE, and the maximum absolute error are computed over the perturbed points only. The error at the case itself
#' (`target_case_absolute_error`) is computed from a separate surrogate fitted to the perturbed points without the
#' case. Apparent metrics (`apparent_*`) and the reported coefficients come from one surrogate fitted to the case and
#' all perturbed points; they describe the fitted surrogate and are not fidelity on new points. Perturbation seeds
#' and cross-fit seeds are separate ([csdg_local_fidelity_audit()]: default 20 perturbation seeds; cross-fit seeds
#' default to the perturbation seed plus 100000), and kernel widths can be varied (`kernel_widths`, default 0.50,
#' 0.75, 1.00; primary 0.75).
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
#' @param loss Prediction loss used for permutation importance: for regression `"rmse"` (default), `"mse"`
#'   (squared error), or `"mae"`; for classification `"logloss"` (default), `"brier"`, or `"error"`.
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
#' @param n_perturb Number of perturbed points per case (see the section "Local surrogate").
#' @param kernel_width Width \eqn{h} of the Gaussian kernel (see the section "Local surrogate").
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
#'   For `csdg_local_surrogate()` and `csdg_oof_local_surrogate()`, `weighted_r2`, `weighted_rmse`, `weighted_mae`,
#'   and `maximum_absolute_error` are cross-fitted over the perturbed points, `target_case_absolute_error` scores the
#'   case with a surrogate fitted without it, and `apparent_weighted_r2`, `apparent_weighted_rmse`, and the returned
#'   coefficients describe the surrogate fitted to all points (see the section "Local surrogate").
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
#' @param learner The learner whose results are compared across settings.
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

#' Run a claim-scoped diagnostic audit
#'
#' Derives the gate plan of the claim ([csdg_gate_plan()]) and computes the diagnostics of the gates in the plan:
#' held-out performance (G1), dependence and observed support (G2), calibration (G3a), decision curves (G3b),
#' cross-fitted local fidelity (G4), stability of held-out PFI (G5), comparison with similarly accurate models
#' (G6a), leave-one-setting-out refits (G6b), and subgroup results (G7a); G0a and G0b check the claim and
#' measurement cards, and G7b records user evidence supplied in `evidence`.
#' Required gates are computed; held-out performance is also computed when it is context for the claim (a model
#' description) or when a required gate uses its fold models.
#' Each gate result has a property status: `"supported"` or `"contradicted"` under a criterion with a recorded source
#' and rationale, `"open"` if the criterion or evidence is missing, `"not_required"` if the gate was not run, or
#' `"error"`. The audit cannot judge whether the procedure computes the quantity that the claim names, so G2 is
#' `"open"` until the researcher records it with [csdg_evidence_record()]; [csdg_claim_report()] accepts such
#' records.
#'
#' Individual gate errors are retained in the report card so that one failure does not erase other diagnostic
#' evidence.
#'
#' @param task An [mlr3::Task].
#' @param learner The [mlr3::Learner] to audit, with preprocessing embedded in its pipeline.
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
#' @param evidence Named external gate evidence (lists with `status`, `summary`, and optional criterion fields,
#'   or `CSDGGateResult` objects), keyed by gate identifier; `audience_evidence` supplies user evidence for G7b.
#' @param run_gates Optional gate ids to execute.
#'   This controls computation but does not change which gates the claim requires.
#' @param output_dir Optional bundle-export directory.
#' @param ... Explicit aliases or additional runner inputs accepted by `csdg_audit()`.
#' @return A `CSDGResult` object.
#' @name csdg_audit
NULL

#' Report, export, and plot CSDG results
#'
#' `csdg_report_card()` lists the gates with their area, evidence question, the condition under which the claim
#' requires them, whether they are required (`required`, `plan_role`), and the status of each required property
#' (`"supported"`, `"contradicted"`, `"open"`, or `"error"`). A gate that the claim does not require is context
#' (`evidence_role = "context"`): it is reported only and has no property status, so its `status` is `"context"` if
#' it was run and `"not_required"` otherwise. The column `diagnostic_status` keeps the computed result of every gate
#' (for a required gate it equals `status`); for a context gate it never changes the assessment.
#' `csdg_claim_report()` applies the decision rule to the required properties: the `assessment` is `"not_met"` if at
#' least one is contradicted; otherwise `"unresolved"` if at least one is open (an error counts as open, and a causal
#' claim without a causal design is open); otherwise `"met"`. `decision_options` lists the decisions that the
#' assessment permits: a met claim is retained; a claim that is not met or unresolved is revised or withheld.
#' Records supplied in `evidence` replace the audit status of their gates, and established counterevidence and
#' unresolved threats are applied as in [csdg_adjudicate_claim()]. A claim written after the results were seen
#' (origin `"retrospective_exploratory"`) is labeled "(exploratory)".
#' `csdg_export()` writes an atomic audit bundle containing cards, gate evidence, provenance, uncertainty
#' boundaries, and an MD5 manifest. No aggregate score is computed.
#' The columns `applicable`, `applicability`, `materiality`, `adjudication_basis`, and `claim_consequence` of the
#' report card and `decision` (an alias of `assessment`), `decision_basis` (an alias of `assessment_basis`),
#' `met_gates`, `not_met_gates`, and `unresolved_gates` of the claim report are deprecated aliases kept for one
#' release for compatibility with versions up to 0.1.5, which stored the assessment under the name `decision`.
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
#' @param evidence Optional list of `CSDGEvidenceRecord` objects for `csdg_claim_report()`.
#' @param ... Additional plotting arguments, which are currently unsupported.
#' @return A report table, claim report, invisibly returned bundle path, or [ggplot2::ggplot] object.
#' @name csdg_reporting
NULL
