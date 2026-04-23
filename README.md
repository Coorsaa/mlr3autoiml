
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlr3autoiml

`mlr3autoiml` is an **auditing and automation layer** for interpretable
machine learning (IML) on top of the **mlr3 ecosystem**. It implements a
**gate-based workflow** (G0A/G0B → G1 → G2 → G3 → G4 → G5 → G6 →
G7A/G7B) and assigns **claim-scoped Interpretation Evidence Levels
(IEL-0…3)** that scope the strength of permissible interpretive claims.

Core dependencies: `mlr3`, `mlr3measures`, `mlr3misc`, `data.table`,
`checkmate`, `R6`. Optional integrations (pipelines, SHAP, iml,
plotting) activate when the corresponding packages are available.

## Installation

``` r
# install.packages("remotes")
remotes::install_local("mlr3autoiml")
```

------------------------------------------------------------------------

## Example 1 — Classification · decision support

**Task:** `german_credit` (1 000 rows, 20 mixed-type features, binary
`credit_risk`). Full gate path including calibration, decision utility,
and subgroup audit.

``` r
library(mlr3)
library(mlr3autoiml)

task    <- tsk("german_credit")
learner <- lrn("classif.rpart", predict_type = "prob", maxdepth = 6L)
```

``` r
auto <- AutoIML$new(
  task       = task,
  learner    = learner,
  resampling = rsmp("cv", folds = 5),
  purpose    = "decision_support",
  seed       = 42
)

auto$ctx$claim <- make_claim_card(
  purpose       = "decision_support",
  semantics     = "within_support",
  stakes        = "medium",
  claims        = list(global = TRUE, local = TRUE, decision = TRUE),
  decision_spec = list(
    thresholds = seq(0.20, 0.60, by = 0.05),
    utility    = list(tp = 1, fp = -0.5, fn = -1, tn = 0)
  ),
  target_population          = "Loan applicants, German Credit dataset",
  intended_use               = "Support credit-risk screening decisions.",
  prohibited_interpretations = "Causal claims; individual enforcement decisions."
)

auto$ctx$measurement        <- make_measurement_card("item")
auto$ctx$sensitive_features <- c("personal_status_sex", "foreign_worker")
auto$ctx$alt_learners       <- list(
  Featureless = lrn("classif.featureless", predict_type = "prob")
)
```

``` r
res <- auto$run(verbose = TRUE)
```

``` r
auto$report_card()
cat("IEL:  G =", res$iel$global, "| L =", res$iel$local,
        "| D =", res$iel$decision, "| overall =", res$iel$overall, "\n")
```

``` r
auto$plot("overview")
auto$plot("g2_hstats")
auto$plot("g2_effect",        feature = "amount")
auto$plot("g3_calibration")
auto$plot("g3_dca")
auto$plot("g5_stability")
auto$plot("g6_performance")
auto$plot("g6_pred_multiplicity")
auto$plot("g7a_subgroups")
auto$plot("shap_importance")
auto$plot("shap_local",       row_id = 1L)
```

``` r
export_analysis_bundle(auto, dir = "bundle_german_credit", prefix = "german_credit")
```

------------------------------------------------------------------------

## Example 2 — Regression · global insight

**Task:** `california_housing` (20 640 rows, 8 numeric + 1 factor
feature, continuous `median_house_value`). Correlated spatial predictors
(`latitude`, `longitude`, `households`) trigger ALE selection and an
interaction screen in G2.

``` r
task    <- tsk("california_housing")
learner <- lrn("regr.rpart", maxdepth = 8L)
```

``` r
auto <- AutoIML$new(
  task       = task,
  learner    = learner,
  resampling = rsmp("cv", folds = 5),
  purpose    = "global_insight",
  seed       = 42
)

auto$ctx$claim <- make_claim_card(
  purpose           = "global_insight",
  semantics         = "within_support",
  stakes            = "low",
  claims            = list(global = TRUE, local = FALSE, decision = FALSE),
  target_population = "Californian housing blocks (1990 census)",
  intended_use      = "Identify global drivers of median house value.",
  prohibited_interpretations = "Causal claims; individual valuation."
)

auto$ctx$measurement        <- make_measurement_card("item")
auto$ctx$sensitive_features <- "ocean_proximity"
auto$ctx$alt_learners       <- list(
  Featureless = lrn("regr.featureless")
)
```

``` r
res <- auto$run(verbose = TRUE)
```

``` r
auto$report_card()
cat("IEL:  G =", res$iel$global, "| overall =", res$iel$overall, "\n")
```

``` r
auto$plot("overview")
auto$plot("g2_hstats")
auto$plot("g2_effect",    feature = "median_income")
auto$plot("g5_stability")
auto$plot("g6_performance")
auto$plot("g6_pred_multiplicity")
auto$plot("g7a_subgroups")
auto$plot("shap_importance")
```

``` r
export_analysis_bundle(auto, dir = "bundle_california", prefix = "california")
```

------------------------------------------------------------------------

## Gate overview

| Gate | Triggered when | Evidence checks |
|----|----|----|
| G0A | always | Claim and semantics declaration; hard-stop on `causal_recourse` without identification |
| G0B | always | Measurement readiness; reliability / invariance evidence for high-stakes |
| G1 | always | CV performance + calibration snapshot |
| G2 | always | Feature dependence · ALE vs PDP selection · pairwise interaction screening |
| G3 | `claims$decision = TRUE` | Calibration curve + Decision Curve Analysis |
| G4 | `claims$local = TRUE` | SHAP faithfulness + perturbation sensitivity checks |
| G5 | always | Stability of narrated patterns under bootstrap resampling |
| G6 | high-resolution profile | Rashomon multiplicity + transport probes |
| G7A | `sensitive_features` set | Subgroup performance + calibration audit |
| G7B | user-facing + high-stakes | Human-factors (task-based evaluation) evidence |

IELs (0–3) are assigned per claim scope (global / local / decision). The
overall IEL is the minimum across all requested scopes.

## Status

The package follows a claim-first, gate-based architecture with explicit
semantics, claim-scoped IELs, and evidence-driven gate planning. Default
runs use the `high_resolution` profile; `quick_start = TRUE` is
available for rapid prototyping. IEL-3 requires G6 evidence across all
scopes and, for decision claims, a passed G7A subgroup audit.

## License

LGPL-3.
