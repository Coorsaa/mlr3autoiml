# Migrating to mlr3autoiml 0.1.6

Version 0.1.6 uses the vocabulary of the article. Old labels and argument values still work but emit a deprecation
warning of class `mlr3autoiml_deprecated` once per session. Stored report cards with old statuses can still be
plotted.

## Old and new values

| Where | Up to 0.1.5 | 0.1.6 |
|---|---|---|
| Scope element names (`csdg_claim()`) | `target`, `model_scope`, `explanation_design`, `analytic_distribution`, `semantics`, `scientific_use` | `quantity`, `model`, `procedure`, `data`, `meaning`, `use` (old names remain aliases) |
| Meaning | `semantics`: `fitted_model_description`, `hypothetical_model_query`, `causal`, `recourse` | `meaning`: `model_description`, `population_claim`, `causal_claim` |
| Model | `selected_model`, `cross_fitted_pipeline`, `near_equivalent_models`, `model_class` | `fitted_model`, `learner`, `several_models` (`unspecified` unchanged) |
| Evidence role | `necessary_requirement` | `required_property` |
| | `potential_defeater` with `materiality = "materialized"` | `established_counterevidence` |
| | `potential_defeater`, not materialized, with `claim_consequence = "unresolved"` | `unresolved_threat` |
| | other `potential_defeater`, `graded_support`, `descriptive_context` | `context` |
| Property status | implicit in `result_direction`, `availability`, `claim_consequence` | `status`: `supported`, `contradicted`, `open` |
| Gate status (`csdg_report_card()`) | `met`, `not_met`, `unresolved`, `not_applicable`, `error` | required gates: `supported`, `contradicted`, `open`, `error`; gates that the claim does not require: `context` if run (computed result in `diagnostic_status`), otherwise `not_required` |
| Claim consequence | `retain_exact_claim`, `revise_claim`, `exact_claim_not_retained`, `unresolved` | deprecated; use `status` (on context it is ignored) |
| Assessment | `met`, `not_met`, `unresolved`, `not_applicable`, stored in the field `decision` | same values in the field `assessment`; `decision` remains a deprecated alias of `assessment` (and `decision_basis` of `assessment_basis` in `csdg_claim_report()`) |
| Decision | implied by the assessment, not listed | `retain`, `revise`, `withhold`: `decision_options` lists the decisions that the assessment permits; `csdg_claim_matrix(decision = )` records the one taken |
| Scope relation | `alternative_or_incomparable` | `incomparable` |
| Revision kind | `context_restriction`, `estimand_change` | `restriction_without_entailment` (requires a narrower scope and no declared entailment), `change_of_question`; `logical_weakening` (weakening) unchanged |
| Gate names | software labels such as "Model multiplicity" | the names of Table 3 of the article ("Models"), see `csdg_gate_registry()` |

## Behavior that changed

* `csdg_gate_plan()` derives the required gates from the scope as in Table 3 of the article: G1 is required only
  for population claims or claims that use the predictions (context for other explanation claims); G2 and G5 are
  required properties of every explanation claim; G3a only for `"calibration"` claims; G4 only for local
  explanations with a local surrogate; G6a also for population claims; G7a only for `"subgroup"` claims; G7b for
  `use_claim = TRUE` and decisions based on explanations; a causal claim requires a causal design (`"CD"`).
* `csdg_adjudicate_claim()` ignores consequences on context records. In 0.1.5, a `graded_support` record with
  `claim_consequence = "unresolved"` left an otherwise met claim unresolved; the claim is now met.
* `csdg_claim_report()` applies the decision rule instead of always reporting `"unresolved"`.
* The audit reports G2 as open: whether the procedure computes the quantity that the claim names needs the
  researcher's judgment, recorded with `csdg_evidence_record()`.
* An incomplete claim (for example, a decision claim that names no decision) leaves G0a open instead of
  contradicting it.
* A population claim must set `meaning = "population_claim"`. The meaning is never inferred from `claim_level`: a
  0.1.5 claim with noncausal semantics and `claim_level = "substantive"` becomes a model description, for which G1
  and G6a are not required. Set the meaning explicitly when migrating such a claim.
* A gate that the claim does not require is reported as context: in `csdg_report_card()` its `status` is
  `"context"` (if it was run) or `"not_required"`, and its computed result is in the new column
  `diagnostic_status`; `csdg_plot()` labels it "Context".

## Examples of the four roles

```r
# A required property and its status.
csdg_evidence_record("G6a", TRUE, "required_property", status = "contradicted",
  rationale = "Model B reverses the ordering.",
  required_property = "Item 1 has the larger marginal PFI in A and in B.",
  observation = "2 versus 0 in A, 0 versus 2 in B.",
  relevance_to_proposition = "Only the model differs, and the claim covers both models.")

# Established counterevidence: a demonstrated problem shown to affect the result.
csdg_evidence_record("G2", TRUE, "established_counterevidence",
  rationale = "Two items were swapped before the computation.",
  required_property = "The procedure computes the named quantity.",
  observation = "The coding error changes which item each value refers to.",
  relevance_to_proposition = "The reported ordering concerns the swapped items.")

# An unresolved threat: plausible but unquantified.
csdg_evidence_record("G1", TRUE, "unresolved_threat",
  rationale = "The optimism was not quantified; a nested analysis would resolve it.",
  required_property = "Held-out performance is not inflated by predictor selection.",
  observation = "Predictors were selected on the same data outside the cross-validation.")

# Context: reported, never changes the assessment.
csdg_evidence_record("G5", TRUE, "context", result_direction = "descriptive",
  rationale = "Grouped PFI is reported for comparison.")
```
