# mlr3autoiml 0.0.8

## Gate 2 / GADGET integration
- Gate 2 now stores the centered ICE matrices used for heterogeneity diagnostics, fixing the previously inert GADGET regionalization path.
- Added joint GADGET-style regionalization across the selected feature set instead of independent one-feature regionalizations. Splits now minimize the summed centered-local-effect risk across target features, retain split rules, region assignments, local curves, global curves, and feature-wise heterogeneity-reduction metrics.
- Added optional GADGET-PINT permutation screening (`ctx$structure$pint_enabled`) for final analyses where repeated refits are acceptable. The screen stores observed centered-ICE risks, permutation null summaries, p-value-style Monte Carlo exceedance rates, and flags.
- Added `AutoIML$plot("g2_gadget")`, `AutoIML$plot("g2_gadget_tree")`, and `AutoIML$plot("g2_pint")` outputs, and exported the corresponding tables and figures through `export_analysis_bundle()`.

## Analysis workflow
- `gate2_tables()` and `guide_workflow()` now expose and recommend the GADGET/PINT artifacts when they are available.
- Gate 0A now derives conservative non-use, prohibited-interpretation, and decision-policy wording from the declared claims and semantics, while Gate 0B derives pipeline-level missingness and scoring notes from the analyzed task and only requires reliability or comparability evidence when the measurement type makes those claims material.

# mlr3autoiml 0.0.6

## New features
- `AutoIML$plot("overview")` replaces `plot("storyboard")` with a claim-adaptive multi-panel summary: gate-status strip, IEL triplet, G1/G3/G2/G5/G6/G7A evidence panels assembled based on which gates actually ran (requires `patchwork` for composition; returns a named list otherwise).
- `AutoIML$plot("g2_ale_2d")` visualizes a second-order ALE interaction surface for the top feature pair identified by H-statistics. The surface is computed via `.autoiml_ale_2d()` and stored under `gate$artifacts$ale2d` after each G2 run; configurable via `ctx$structure$ale_2d_bins` and `ctx$structure$ale_2d_top_pairs`.
- `AutoIML$plot("g3_calibration")` and `AutoIML$plot("g3_dca")` visualize the reliability curve and decision curve analysis produced by Gate 3.
- `AutoIML$plot("g5_stability")` visualizes permutation importance with bootstrap 95% confidence intervals from Gate 5.
- `AutoIML$plot("g7a_subgroups")` visualizes subgroup performance as a horizontal bar chart from Gate 7A.
- `AutoIML$plot("g6_rank_heatmap")` shows a heatmap of feature importance ranks across Rashomon-set members.
- `AutoIML$plot("gate_strip")` produces a colored tile strip summarizing pass/warn/fail/skip status across all gates.
- Gate 7A regression subgroup tables now include `r2` (coefficient of determination) and `mean_y` alongside `rmse`.

## Alignment with manuscript workflow
- Claim-scoped IEL assignment now follows the manuscript's simplified backbone: Claim and Semantics Card -> core gates -> triggered additional gates -> claim-scoped IEL.
- Decision-support defaults no longer implicitly request local/person-level claims.
- Human-factors evidence (Gate 7B) is now triggered only for user-facing/non-technical claims, rather than for every high-stakes run.
- IEL rules were revised so that global/local scopes do not require subgroup or human-factors gates by default, Gate 6 mainly matters for stronger transport/robustness tiers, and decision IELs no longer require local-faithfulness evidence unless local claims are actually requested.
- Extended report-card requirements now support applicability conditions and richer artifact/metric checks, which makes the exported audit bundle line up more closely with the paper's gate logic.

## Internal
- Added helper utilities for audience heuristics, applicability checks, and evidence-presence validation.
- Updated package tests to reflect the manuscript-aligned IEL and reporting logic.

# mlr3autoiml 0.0.5

## Breaking changes
None. All changes are additive and backward-compatible with `mlr3autoiml` 0.0.4
users — `iel_rules.yaml` 0.2.0 rules continue to work without modification, and
existing analysis scripts that did not use plausible-value pooling will produce
the same numerical output as before.

## New features

### Artifact-content checks in the IEL rule engine
The declarative IEL rule evaluator (`.autoiml_eval_iel_rule()`) now recognises a
new optional per-rule field, `requires_artifact_keys` (schema `iel_rules.yaml`
v0.3.0). When present, it specifies a named list mapping each gate id to the
artifact keys that must exist (and be non-empty) in `gate$artifacts` for the
rule to match.

This makes the IEL ceiling reflect substantive evidence rather than only
gate-level pass/warn status. For example, IEL-2 for the local scope now
requires not just that Gate 4 (faithfulness) ran successfully, but also that it
actually produced a faithfulness summary; IEL-2 for decision now requires both
Gate 3 decision artifacts and Gate 4 faithfulness evidence, which matches the
package's existing claim-scope guidance.

`iel_rules.yaml` is shipped at `inst/extdata/iel_rules.yaml` v0.3.0 with
artifact-key specs added to the IEL-2 and IEL-3 rules. The same YAML is shared
verbatim with the Python companion `pyautoiml`.

### Plausible-values pooling in Gate 1 (Rubin's rules)
Gate 1 (`Gate1Validity`) now supports outcome targets given as multiple
plausible values. Pass them through:
```r
auto$ctx$plausible_values$pv_tasks <- list(task_pv2, task_pv3, ..., task_pvm)
```
where each element is an `mlr3::TaskRegr` whose target column holds an
alternative plausible value of the same outcome. Gate 1 will resample each
extra task on the same instantiated fold assignment and pool per-fold metrics
across all m PVs using Rubin's rules:
```
T = W + (1 + 1/m) * B
```
Pooled means, within/between/total variances, approximate degrees of freedom,
and 95% CIs are written to a new
`gate1$artifacts$pv_pool` data.table with one row per measure (`measure_id`,
`n_pv`, `pooled_mean`, `pooled_se`, `within_var`, `between_var`, `total_var`,
`df`, `ci_low`, `ci_high`).

When `pv_tasks` is `NULL` (the default), Gate 1 behaves exactly as before and
`pv_pool` is `NULL`.

## Bug fixes
- Gate 1 plausible-value pooling now reuses the exact instantiated resampling
  splits across plausible-value tasks, so the pooled uncertainty is not inflated
  by split-to-split randomness.
- The IEL rule validator now checks the shape of `requires_artifact_keys`
  instead of silently accepting malformed schema entries.

## Internal
- `R/IEL.R`: `.autoiml_eval_iel_rule()` extended with the artifact-key path
  (still backward-compatible).
- `R/AutoIMLBase.R`: added `ctx$validation` and `ctx$plausible_values`
  initialisation slots.
- `R/gate_04_faithfulness.R`: adds a compact `faithfulness_summary` artifact so
  higher IEL tiers can require substantive local-faithfulness evidence.
- `R/gate_06_multiplicity.R`: exposes `shift_assessment` as the transport
  assessment artifact used by the IEL rules.

# mlr3autoiml 0.0.4

(Initial release used in the article submission.)
