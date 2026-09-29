
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlr3autoiml

`mlr3autoiml` helps researchers decide whether the evidence supports a
conclusion that they draw from an explanation of a machine learning
model, such as “the model relies mainly on self-rated health.” It
implements **Claim-Scoped Diagnostic Gates (CSDG)** for the **mlr3
ecosystem** in five steps:

1.  **Write the claim**, the conclusion you intend to report
    (`csdg_claim()`).
2.  **Specify its scope** in six elements: the quantity, model,
    procedure, data, meaning, and use.
3.  **Derive the required properties** from the 12 gates
    (`csdg_gate_registry()`, `csdg_gate_plan()`).
4.  **Evaluate the evidence** on each required property
    (`csdg_evidence_record()`, with diagnostics from `csdg_audit()`).
5.  **Decide and report** with a fixed rule (`csdg_adjudicate_claim()`).

The package records the claim and its scope, derives the gate plan,
computes diagnostics, and applies the decision rule. The researcher sets
the criteria, states the relevance of each observation, and enters
established counterevidence and unresolved threats. Favorable results
never offset a contradicted property, and no aggregate score is
computed.

| Term | Values |
|----|----|
| Scope elements | quantity, model, procedure, data, meaning (model description, population claim, causal claim), use |
| Evidence roles | required property, established counterevidence, unresolved threat, context |
| Status of a required property | supported, contradicted, open |
| Assessment of a claim | not met if a required property is contradicted; otherwise unresolved if one is open; otherwise met |
| Decision | a met claim is retained; a claim that is not met or unresolved is revised or withheld |

## Installation

Install the current version from GitHub:

``` r
remotes::install_github("coorsaa/mlr3autoiml", build_vignettes = TRUE)
library(mlr3autoiml)
```

Released versions are tagged, for example
`remotes::install_github("coorsaa/mlr3autoiml@v0.1.7")`.

## Quick start: the numerical example of the article

Two items carry the same information: `Z` is standard normal, and both
items and the outcome equal `Z`. Model A predicts with item 1 and model
B with item 2 (`f_A(x) = x1`, `f_B(x) = x2`); because the outcome equals
each item, the linear fit in every fold is exact. The package computes
the held-out permutation feature importance (PFI) with squared error.

``` r
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3autoiml)
library(data.table)

set.seed(20260926)
n = 10000L
z = rnorm(n)
task = as_task_regr(data.frame(item_1 = z, item_2 = z, y = z), target = "y", id = "numerical_example")
model_using = function(item) {
  as_learner(po("select", selector = selector_name(item), id = paste0("use_", item)) %>>% lrn("regr.lm"))
}
folds = rsmp("cv", folds = 2L)
fits = list(
  A = csdg_resample(task, model_using("item_1"), folds, measures = msrs("regr.mse"), seed = 1L),
  B = csdg_resample(task, model_using("item_2"), folds, measures = msrs("regr.mse"), seed = 1L)
)
groups = list(item_1 = "item_1", item_2 = "item_2", both_items = c("item_1", "item_2"))
pfi = lapply(fits, csdg_fold_pfi, feature_groups = groups, loss = "mse", repetitions = 20L, seed = 2L)
marginal = rbindlist(lapply(names(pfi), function(model) {
  pfi[[model]]$summary[, .(model = model, feature_group, mean_importance)]
}))
dcast(marginal, model ~ feature_group, value.var = "mean_importance")[, lapply(.SD, function(x) {
  if (is.numeric(x)) round(x, 2) else x
})]
#> Key: <model>
#>     model both_items item_1 item_2
#>    <char>      <num>  <num>  <num>
#> 1:      A       1.98   1.98   0.00
#> 2:      B       1.98   0.00   1.99
```

The exact population values are 2 and 0 in model A and 0 and 2 in model
B; permuting both items together (grouped PFI) gives 2 in both models.
Conditional PFI, which draws the permuted item from its distribution
given the other item, is 0 for both items in both models; here the
permutation within strata of `Z` implements it, because each item equals
the other.

``` r
conditional = lapply(fits, csdg_fold_pfi, feature_groups = groups[1:2], loss = "mse", repetitions = 5L,
  strata = z, seed = 3L)
rbindlist(lapply(names(conditional), function(model) {
  conditional[[model]]$summary[, .(model = model, feature_group, mean_importance)]
}))
#>     model feature_group mean_importance
#>    <char>        <char>           <num>
#> 1:      A        item_1               0
#> 2:      A        item_2               0
#> 3:      B        item_1               0
#> 4:      B        item_2               0
```

**Steps 1 and 2: the claim and its scope.** The claim covers both
models.

``` r
claim_both = csdg_claim(
  id = "both_models",
  statement = "Under marginal permutation, both models rely more on item 1 than on item 2.",
  claim_type = "global_explanation",
  quantity = "marginal PFI with squared error: increase in expected squared error when one item is permuted",
  model = "several_models",
  procedure = "each item permuted independently of the other item and the outcome; 20 permutations per fold",
  data = "Z standard normal; item 1 = item 2 = outcome = Z; 10,000 simulated observations",
  meaning = "model_description",
  use = "scientific description",
  provenance = list(origin = "specified_before_results", date = "2026-09-26",
    time_basis = "date of the example", selection_basis = "written before the PFI values were computed",
    evidence_ids = character())
)
measurement = csdg_measurement(outcome = "y", predictors = c("item_1", "item_2"), data_source = "simulation",
  sample_definition = "all simulated observations", missingness = "none", preprocessing = "none")
explanation = csdg_explanation(method_ids = "pfi", feature_groups = groups)
```

**Step 3: the required properties.** Besides G0a and G0b, the claim
requires G2 (it interprets PFI values), G5 (it states an ordering), and
G6a (it covers two models). Held-out performance (G1) is context,
because a model description holds whatever the model’s accuracy.

``` r
plan = csdg_gate_plan(claim_both, measurement, explanation)
plan[, .(gate_id, gate_name, required, plan_role)]
#>     gate_id              gate_name required    plan_role
#>      <char>                 <char>   <lgcl>       <char>
#>  1:     G0a          Specification     TRUE     required
#>  2:     G0b   Measurement and data     TRUE     required
#>  3:      G1 Predictive performance    FALSE      context
#>  4:      G2              Procedure     TRUE     required
#>  5:     G3a            Calibration    FALSE not_required
#>  6:     G3b              Decisions    FALSE not_required
#>  7:      G4         Local fidelity    FALSE not_required
#>  8:      G5              Stability     TRUE     required
#>  9:     G6a                 Models     TRUE     required
#> 10:     G6b               Settings    FALSE not_required
#> 11:     G7a              Subgroups    FALSE not_required
#> 12:     G7b                  Users    FALSE not_required
```

**Step 4: the evidence.** In model A, the difference between the two
items is far beyond the error due to random permutation (Monte Carlo
rule, per fold). The decisive property is G6a: model B reverses the
ordering.

``` r
csdg_pfi_mc_difference(pfi$A, "item_1", "item_2")[, .(iteration, estimate, threshold, beyond_monte_carlo_error)]
#>    iteration estimate   threshold beyond_monte_carlo_error
#>        <int>    <num>       <num>                   <lgcl>
#> 1:         1 1.965369 0.009051588                     TRUE
#> 2:         2 1.995245 0.009445336                     TRUE

property = function(gate_id, status, required_property, observation, relevance, criterion = NULL) {
  csdg_evidence_record(
    gate_id, TRUE, "required_property", status = status,
    criterion = if (!is.null(criterion)) list(value = criterion, direction = "qualitative"),
    criterion_source = if (!is.null(criterion)) "translated from the claim",
    criterion_rationale = if (!is.null(criterion)) required_property,
    rationale = observation, required_property = required_property, observation = observation,
    relevance_to_proposition = relevance
  )
}
foundation = list(
  property("G0a", "supported", "The claim and its six scope elements are stated.",
    "All six elements are recorded.", "They fix the quantity, the models, and the procedure."),
  property("G0b", "supported", "The data cover what the claim names.",
    "The population is defined exactly; there is no measurement error or preprocessing.",
    "The claim names no construct and no other population."),
  property("G2", "supported", "Marginal permutation computes the PFI that the claim names.",
    "Squared error and marginal permutation; both models are defined for all inputs.",
    "The claim names the perturbation, so the unrealistic permuted inputs are part of what it describes."),
  property("G5", "supported", "The ordering persists when the quantity is estimated again.",
    "Exact values do not vary.", "The same quantity is computed again.")
)
g6a = property("G6a", "contradicted", "Item 1 has the larger marginal PFI in A and in B.",
  "PFI of item 1 versus item 2: 2 versus 0 in A, 0 versus 2 in B.",
  "Only the model differs between the two computations, and the claim covers both models.",
  criterion = "strict ordering in both models")
```

**Step 5: decide and report.** The original claim is not met and is
revised to model A.

``` r
assessment_both = csdg_adjudicate_claim(c(foundation, list(g6a)), claim_applicable = TRUE, plan = plan)
assessment_both
#> <CSDGClaimAdjudication>
#>   Assessment: not met 
#>   Decision options: revise or withhold 
#>   Required properties:
#>    G0a: supported (record)
#>    G0b: supported (record)
#>    G2: supported (record)
#>    G5: supported (record)
#>    G6a: contradicted (record)
#> 
#>   A required property is contradicted; favorable results never offset it.

claim_a = csdg_claim_revision(
  claim_both, id = "model_a", claim_version = "C1", revision_relation = "narrower",
  statement = "Under marginal permutation, model A relies more on item 1 than on item 2.",
  model = "fitted_model",
  provenance = list(origin = "retrospective_exploratory", date = "2026-09-26", time_basis = "date of the example",
    selection_basis = "restriction chosen after the comparison of A and B", evidence_ids = "both_models_G6a")
)
plan_a = csdg_gate_plan(claim_a, measurement, explanation)
plan_a[required == TRUE, gate_id]
#> [1] "G0a" "G0b" "G2"  "G5"
g2_a = property("G2", "supported",
  "Marginal PFI with squared error in model A is larger for item 1, and the wording names the perturbation.",
  "2 versus 0; conditional PFI 0 versus 0.",
  "The computation uses the model, perturbation, and loss that the claim names.",
  criterion = "strict ordering of the exact values; qualifier in the wording")
assessment_a = csdg_adjudicate_claim(c(foundation[c(1L, 2L, 4L)], list(g2_a)), claim_applicable = TRUE,
  plan = plan_a)
assessment_a$assessment
#> [1] "met"
assessment_a$decision_options
#> [1] "retain"
```

The revised claim is met (exploratory; it was written after the results
were seen) and retained.

## Learn more

- **[The CSDG
  walkthrough](https://stefancoors.de/mlr3autoiml/articles/claim_scoped_diagnostic_gates.html)**
  (`vignette("claim_scoped_diagnostic_gates", package = "mlr3autoiml")`)
  follows the five steps with the numerical example, an unresolved claim
  with an unresolved threat, and a met claim assessed with
  `csdg_audit()`.
- **[The function
  reference](https://stefancoors.de/mlr3autoiml/reference/index.html)**
  documents every function, including the diagnostics of `csdg_audit()`
  and the local surrogate described in `?csdg_diagnostics`.
- `csdg_claim_record_template()` returns the blank claim record of the
  article as a table to fill in, and `supported()`, `contradicted()`,
  and `open_status()` return the three property statuses.
- Labels of versions before 0.1.6 still work with a deprecation warning;
  see [the migration
  note](https://github.com/coorsaa/mlr3autoiml/blob/main/inst/MIGRATION_0_1_6.md)
  and [the
  changelog](https://stefancoors.de/mlr3autoiml/news/index.html).

## Diagnostics in an mlr3 workflow

`csdg_audit()` derives the gate plan and computes the diagnostics of an
mlr3 learner: held-out performance, dependence and support, calibration,
decision curves, cross-fitted local fidelity, stability of held-out PFI,
comparisons with similarly accurate models, leave-one-setting-out
refits, and subgroup results. `csdg_report_card()` lists the gates with
the status of each required property, and `csdg_claim_report()` applies
the decision rule to the audit and to evidence records that the
researcher supplies. A numerical criterion needs a source and a
rationale; without them, the property remains open. By default,
`csdg_export()` omits fitted models and observation-level predictions;
review the generated bundle before sharing it.

## The accompanying article

Version 0.1.0 produced the primary analyses of the accompanying article.
The scripts of its two empirical applications are kept in a separate
analysis repository.

## Legacy AutoIML workflow

The earlier `AutoIML` interface remains available for compatibility. Its
gate identifiers and pass/warn/fail/skip labels are separate from the
CSDG gate registry and vocabulary. Its examples and gate overview are in
[the legacy AutoIML
article](https://stefancoors.de/mlr3autoiml/articles/legacy_autoiml.html)
(`vignette("legacy_autoiml", package = "mlr3autoiml")`); new analyses
start from the CSDG interface above.

## License

LGPL-3.
