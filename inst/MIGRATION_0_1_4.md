# Claim-decision contract in mlr3autoiml 0.1.4

This patch corrects claim applicability and record-consequence handling. It does not change fitted models,
diagnostics, empirical estimates, or the earlier software identities that produced retained analyses.

## Claim scope and evidence applicability

`csdg_adjudicate_claim()` accepts `claim_applicable` separately from each module's `applicable` field. For existing
calls that omit the argument, the claim is treated as in scope, and the result records
`applicability_source = "legacy_default"`. That default never infers an out-of-scope proposition from irrelevant
modules. New calls should explicitly state the claim's scope:

```r
csdg_adjudicate_claim(list(), claim_applicable = TRUE)
# decision: unresolved; no supplied necessary evidence

csdg_adjudicate_claim(
  list(), claim_applicable = FALSE,
  applicability_rationale = "The proposition is outside the declared evaluation scope."
)
# decision: not_applicable
```

An explicit out-of-scope decision requires a nonempty rationale and rejects any supplied module simultaneously
marked applicable to that claim. Empty evidence, missing required evidence, and irrelevant modules cannot produce
`met`. A damaged record missing a mandatory field is rejected; it is not completed using constructor defaults.
Legitimate constructor defaults remain available when initially creating a record. Missing optional legacy linkage
metadata remain explicitly unrecorded.

## Consequences are binding constraints

`claim_consequence` is a constraint on adjudication, not a separate overall verdict. Its meaning is:

| Consequence | Meaning in adjudication |
| --- | --- |
| `none` | Adds no constraint beyond the record's role, availability, and observed direction. |
| `retain_exact_claim` | Supplies favorable record-level support; it cannot override another record's constraint. |
| `unresolved` | Prevents `met`, including when the record supports a necessary property. |
| `revise_claim` | Blocks retention of the unchanged claim when a coherent claim-constraining record justifies revision. |
| `exact_claim_not_retained` | Blocks the unchanged claim under the validated necessary-requirement or defeater rule. |

Across records, `not_met` takes precedence over `unresolved`, which takes precedence over `met`. A materialized
applicable defeater cannot be compensated by unrelated support. Conflicting inputs within one record are rejected:
for example, an inapplicable module cannot impose an unresolved consequence, and a complete challenging necessary
requirement cannot simultaneously declare its consequence unresolved. A complete mixed necessary record may remain
unresolved or justify revision; its recorded consequence determines whether the exact claim is still undecided or
must be revised. If the significance of an adverse observation is itself undecided, do not characterize it as an
already unmet necessary requirement.

```r
record = csdg_evidence_record(
  "G2", TRUE, "necessary_requirement", result_direction = "supports",
  claim_consequence = "unresolved",
  rationale = "The property is supported, but an unresolved qualification prevents retention of the exact claim."
)
csdg_adjudicate_claim(list(record), claim_applicable = TRUE)
# decision: unresolved
```

`met` remains conditional on the supplied applicable necessary requirements. The caller must identify all relevant
requirements; the package cannot prove that omitted obligations do not exist. Filling fields does not independently
validate the observation, its relevance, or the proposition.

## Serialization and historical records

Current adjudications include `claim_applicable`, `applicability_source`, and `applicability_rationale`.
`inst/schema/csdg-claim-adjudication.schema.json` validates these current result objects. Evidence-record and
report-card schemas apply the same record-consequence restrictions. Gate-identifier collections remain JSON arrays,
including singletons and empty collections. Optional unknown evidence booleans serialize as `null`; the constructor
accepts them as unrecorded metadata during a round trip.

Earlier saved adjudications are historical outputs, not automatically rewritten or revalidated under this schema.
When reassessing their evidence with the corrected implementation, record the new package identity and compare
decisions explicitly. The two intentional behavioral corrections are: an in-scope claim with no applicable evidence
now remains unresolved, and a supporting record with a binding unresolved consequence no longer yields `met`.

## Executable checks

R unit tests cover scope, empty and incomplete evidence, rejection, legitimate support, mixed-record precedence,
non-compensation, estimand linkage, and JSON round trips. `tests/schema/export_claim_contract.R` exports bounded
fixture calls from an installed package; `tests/schema/test_claim_contract.py` validates them and adversarial
mutations with a Draft 2020-12 JSON Schema implementation. Neither script fits a model or opens empirical data.
