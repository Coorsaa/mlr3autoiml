# mlr3autoiml 0.0.6

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
