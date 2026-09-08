# mlr3autoiml 0.1.0

- Claim-Scoped Diagnostic Gates add prospective claim, measurement, explanation, and configuration cards while retaining the existing AutoIML workflow (no issue).
- Conditional Shapley k-nearest-neighbor sampling now treats observed-versus-missing feature values as mismatches instead of exact matches (no issue).
- One-dimensional ALE curves now align trapezoid-centered accumulated effects with their exported interval midpoints, avoiding geometric distortion on uneven quantile grids (no issue).
- Publication analysis figures now use cataloged aggregate plot sources for calibration, decision curves, and prediction multiplicity, preventing private row-level values from being encoded in shareable vector graphics (no issue).
- Gate 3 no longer applies universal ECE or calibration-slope cutoffs; it reports unadjudicated calibration unless the analyst supplies claim-specific criteria (no issue).
- Gate 7A no longer converts a universal subgroup-ECE cutoff into a warning; subgroup calibration remains descriptive unless it is adjudicated against claim-specific criteria (no issue).
- Second-order ALE interaction surfaces now remove both first-order marginal components and use observed-cell weighting, so additive effects do not appear as interactions (no issue).
- SHILD preprocessing now recodes heights below 100 cm and weights below 20 kg to missing for the adult analytic sample, without inferring or converting alternative units (no issue).
- `AutoIML$shap()` now applies its seed before sampling background rows, making the complete attribution calculation reproducible (no issue).
- `autoiml_palette()` and `autoiml_model_colors()` now accept an explicit `style` argument and expose the shared color or monochrome palettes used by package plots (no issue).
- `csdg_ale_bootstrap()` adds row- or stratified cluster-resampled pointwise percentile intervals that refit the complete learner pipeline and recompute one-dimensional ALE on common grids, while reporting point-curve and bootstrap-interval support separately (no issue).
- `csdg_audit()` plans and executes claim-dependent Gates G0a through G7, preserves per-gate errors, records start and completion times, requires explicit adequacy criteria before claim-support gates are marked sufficient, and can compute explicitly selected descriptive diagnostics without changing their claim-derived applicability or evidence role (no issue).
- `csdg_calibration()` distinguishes calibration-in-the-large from free-intercept calibration models for classification and regression and labels the returned components consistently (no issue).
- `csdg_calibration_bootstrap()` adds natural regression-spline calibration curves, preindexes nested clusters, and can evaluate predetermined bootstrap draws in parallel, preserving deterministic results while making large stratified cluster bootstraps practical (no issue).
- `csdg_claim_matrix()` and `csdg_plot_claim_matrix()` add validated categorical claim-by-evidence reporting with explicit status labels, optional monochrome rendering, and no inferred judgment, gate count, or aggregate score (no issue).
- `csdg_dependence()` drops unused categorical levels before computing bias-corrected Cramér's V, avoiding spurious missing associations (no issue).
- `csdg_effect_diagnostics()`, `csdg_interaction_diagnostics()`, `csdg_shapley_diagnostics()`, and `csdg_prediction_multiplicity()` add package-owned full-fit ALE, PDP, ICE, interaction, Shapley, and prediction-dispersion diagnostics with explicit estimands, limitations, privacy scopes, and support masking (no issue).
- `csdg_explanation_sensitivity()` separates direction-aware performance differences from full-rank reversals, Kendall's tau-b, and deterministic top-k agreement across prespecified near-equivalent models (no issue).
- `csdg_export()` writes atomic audit bundles with provenance, uncertainty boundaries, and named schema-aligned JSON card objects, and its privacy-safe default excludes models, predictions, case-level explanations, split membership, and direct row mappings (no issue).
- `csdg_fold_pfi()` can batch permuted assessment tables for deterministic learners to reduce prediction overhead while preserving held-out importance and the caller's random-number state, and the other CSDG diagnostic functions report dependence, calibration, decision, faithfulness, stability, and subgroup evidence without treating folds as independent observations (no issue).
- `csdg_grouped_resampling()` and `csdg_resample()` add auditable grouped splits, explicit out-of-fold predictions, stored fold learners, and descriptive performance summaries (no issue).
- `csdg_local_fidelity_audit()` evaluates multiple held-out cases across prespecified perturbation seeds and kernel widths, reports cross-fitted fidelity and Monte Carlo variation, exposes fold-training support diagnostics, and completes missing values in sampled empirical neighbors from fold-training medians or modes so surrogate rows remain aligned (no issue).
- `csdg_local_fidelity_randomness_audit()` evaluates a complete crossed grid of perturbation and surrogate cross-fit seeds and separates their marginal and interaction contributions to computational variation without treating them as sampling uncertainty (no issue).
- `csdg_local_support()` reports held-out case proximity to the corresponding fold-training data without treating distance or kernel-weight concentration as proof of joint empirical support (no issue).
- `csdg_matched_setting_exclusion()` adds repeated training-size-matched observed-setting exclusions against pooled out-of-fold reference predictions, separating analysis-size control from unrestricted leave-one-setting-out refits (no issue).
- `csdg_near_equivalence_sensitivity()` evaluates declared absolute and relative tolerance grids around a focal learner without refitting models or treating fold scores as independent observations (no issue).
- `csdg_local_surrogate()` reports deterministic weighted-ridge cross-fitted fidelity over synthetic perturbations, while retaining separately labeled apparent-fit metrics and descriptive full-neighborhood coefficients (no issue).
- `csdg_oof_local_surrogate()` preserves the caller's requested case order across held-out folds, keeping pseudonymous communication-case labels aligned with downstream local outputs (no issue).
- `csdg_plausible_value_summary()` aggregates estimates on their value scale before ranking and labels across-plausible-value spread as descriptive rather than automatically applying a generic pooling rule (no issue).
- `csdg_plot()` restores the shared blue-red visual system, adds an explicit monochrome print style with redundant semantic shapes, provides validated proportional text sizing through `base_size`, uses Arial across its standard plot themes, uses data-adaptive performance axes and compact capped whiskers so short descriptive ranges remain legible, separates unlike metrics into labeled panels, and uses the full zero-to-one scale for subgroup AUC plots (no issue).
- `csdg_plot_data()` adds a table-oriented plotting API for package diagnostic outputs, including performance, calibration, permutation importance, aligned cross-model importance values, model and setting comparisons, rank stability, subgroup estimates and contrasts, gate-status reporting, explanation sensitivity, local fidelity, ALE and ICE effects, interactions, prediction multiplicity, and decision curves; its explicit monochrome style uses achromatic scales plus redundant shapes, line types, or signs where needed, flexible calibration curves use interval ribbons while descriptive calibration bins remain unconnected, point-and-interval and trajectory plots use data-adaptive ranges unless zero is requested, interaction screens zoom to observed values, diverging explanation-sensitivity bars separate tolerance consumption from PFI pair-order reversals without redundant endpoint markers and expose validated bar-width control, ICE limits retain readable tick labels, multiplicity displays use stepwise binned cumulative distributions with directly labeled exact quantiles, and direct labels remain visible at scale boundaries (no issue).
- `csdg_rashomon()` can define a prespecified near-equivalent-model tolerance around either the best candidate or a declared reference learner, separates model sensitivity from leave-one-setting-out predictive generalization, and keeps the acceptance basis explicit (no issue).
- `csdg_subgroup_metrics()` adds reproducible row- or cluster-bootstrap conditional intervals and pairwise contrasts, with optional independent resampling within supplied strata (no issue).
- `csdg_summarize_local_fidelity()` validates and combines complete public local-fidelity replicate and support tables into package-owned case, bandwidth, aggregate, and support summaries (no issue).
- `Gate1Validity` now reports fold and aligned plausible-value variation descriptively, without t-based confidence intervals or Rubin-style pooling of predictive metrics (no issue).
- `Gate6Multiplicity` now reports descriptive fold ranges, defaults to a descriptive standard-deviation tolerance, and treats the former `"1se"` setting as a deprecated alias rather than an uncertainty rule (no issue).
- `save_analysis_plot()` now writes PDF and PNG artifacts on an explicit white background, preventing transparent figures from rendering as black panels in downstream viewers (no issue).

# mlr3autoiml 0.0.8

## Gate 2 / GADGET integration
- Gate 2 now stores the centered ICE matrices used for heterogeneity diagnostics, fixing the previously inert GADGET regionalization path.
- Added joint GADGET-style regionalization across the selected feature set instead of independent one-feature regionalizations. Splits now minimize the summed centered-local-effect risk across target features, retain split rules, region assignments, local curves, global curves, and feature-wise heterogeneity-reduction metrics.
- Added optional GADGET-PINT permutation screening (`ctx$structure$pint_enabled`) for final analyses where repeated refits are acceptable. The screen stores observed centered-ICE risks, permutation null summaries, p-value-style Monte Carlo exceedance rates, and flags.
- Added `AutoIML$plot("g2_gadget")`, `AutoIML$plot("g2_gadget_tree")`, and `AutoIML$plot("g2_pint")` outputs, and exported the corresponding tables and figures through `export_analysis_bundle()`.

## Analysis workflow
- `AutoIML$tables("all")` now returns all supported table groups (`g0`, `g2`, and `g6`) as documented.
- Gate 0B now treats reliability or invariance entries marked as pending, unknown, not assessed, or unsupported as provisional rather than completed evidence.
- Gate 0A now derives conservative non-use, prohibited-interpretation, and decision-policy wording from the declared claims and semantics, while Gate 0B derives pipeline-level missingness and scoring notes from the analyzed task and only requires reliability or comparability evidence when the measurement type makes those claims material.
- Gate 2 now records detected dependence or interaction structure as an explicit claim restriction while keeping the gate at pass status when the relevant diagnostics were successfully computed.
- Gate 7A now records and warns in its messages when audited subgroup variables are also model features, so subgroup audits are easier to interpret as descriptive checks rather than causal or fairness guarantees.
- `gate2_tables()` and `guide_workflow()` now expose and recommend the GADGET/PINT artifacts when they are available.

# mlr3autoiml 0.0.6

## New features
- `AutoIML$plot("overview")` replaces `plot("storyboard")` with a claim-adaptive multi-panel summary: a gate-status strip and G1/G3/G2/G5/G6/G7A evidence panels assembled based on which gates actually ran (requires `patchwork` for composition; returns a named list otherwise).
- `AutoIML$plot("g2_ale_2d")` visualizes a second-order ALE interaction surface for the top feature pair identified by H-statistics. The surface is computed via `.autoiml_ale_2d()` and stored under `gate$artifacts$ale2d` after each G2 run; configurable via `ctx$structure$ale_2d_bins` and `ctx$structure$ale_2d_top_pairs`.
- `AutoIML$plot("g3_calibration")` and `AutoIML$plot("g3_dca")` visualize the reliability curve and decision curve analysis produced by Gate 3.
- `AutoIML$plot("g5_stability")` visualizes permutation importance with bootstrap 95% confidence intervals from Gate 5.
- `AutoIML$plot("g7a_subgroups")` visualizes subgroup performance as a horizontal bar chart from Gate 7A.
- `AutoIML$plot("g6_rank_heatmap")` shows a heatmap of feature importance ranks across Rashomon-set members.
- `AutoIML$plot("gate_strip")` produces a colored tile strip summarizing pass/warn/fail/skip status across all gates.
- Gate 7A regression subgroup tables now include `r2` (coefficient of determination) and `mean_y` alongside `rmse`.

## Alignment with manuscript workflow
- Decision-support defaults no longer implicitly request local/person-level claims.
- Human-factors evidence (Gate 7B) is now triggered only for user-facing/non-technical claims, rather than for every high-stakes run.
- Extended report-card requirements now support applicability conditions and richer artifact/metric checks, which makes the exported audit bundle line up more closely with the paper's gate logic.

## Internal
- Added helper utilities for audience heuristics, applicability checks, and evidence-presence validation.
- Updated package tests to reflect the manuscript-aligned reporting logic.

# mlr3autoiml 0.0.5

## Breaking changes
None. All changes are additive and backward-compatible with `mlr3autoiml` 0.0.4 users. Existing analysis scripts that
did not use plausible-value pooling will produce the same numerical output as before.

## New features

### Plausible-value summaries in Gate 1
Gate 1 (`Gate1Validity`) supports outcome targets given as multiple plausible
values. Pass them through:
```r
auto$ctx$plausible_values$pv_tasks = list(task_pv2, task_pv3, ..., task_pvm)
```
where each element is an `mlr3::TaskRegr` whose target column holds an alternative plausible value of the same
outcome. Gate 1 resamples each extra task on the same instantiated fold assignment and reports PV-specific and
across-PV descriptive summaries. These summaries are not confidence intervals and do not apply Rubin's rules to
predictive metrics.

When `pv_tasks` is `NULL` (the default), Gate 1 behaves exactly as before and
the plausible-value artifacts are `NULL`.

## Bug fixes
- Gate 1 plausible-value summaries reuse the exact instantiated resampling splits across plausible-value tasks, so differences do not reflect new split allocations.

## Internal
- `R/AutoIMLBase.R`: added `ctx$validation` and `ctx$plausible_values`
  initialization slots.
- `R/gate_04_faithfulness.R`: adds a compact `faithfulness_summary` artifact for claim-specific evidence checks.
- `R/gate_06_multiplicity.R`: exposes `shift_assessment` as the transport assessment artifact.

# mlr3autoiml 0.0.4

(Initial release used in the article submission.)
