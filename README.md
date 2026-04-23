# mlr3autoiml

`mlr3autoiml` is an **auditing and automation layer** for interpretable machine learning (IML)
on top of the **mlr3 ecosystem**. It implements a **gate-based workflow** (G0A/G0B, G1–G6, G7A/G7B) and assigns
**claim-scoped Interpretation Evidence Levels (IEL-0…3)** to scope the strength of permissible interpretive claims.

The design goal is to keep core dependencies small: the package builds on `mlr3`, `mlr3measures`,
`mlr3misc`, `data.table`, `checkmate`, and `R6`. Optional integrations (pipelines, tuning, SHAP,
DALEX/iml, plotting) are enabled when corresponding packages are installed.

## Installation (development)

```r
# install.packages("remotes")
remotes::install_local("mlr3autoiml")
```

## Quick example

```r
library(mlr3)
library(mlr3autoiml)

task = tsk("penguins")
learner = lrn("classif.rpart", predict_type = "prob")
resampling = rsmp("cv", folds = 5)

res = AutoIML$new(
  task = task,
  learner = learner,
  resampling = resampling,
  purpose = "global_insight",
  quick_start = TRUE,
  seed = 123
)

res$run()
report_card(res)
```

## Visual-first guided workflow (recommended)

For most users, the easiest path is:

1. run AutoIML,
2. inspect the overview plot,
3. follow `guide()` actions,
4. export an audit bundle.

```r
library(mlr3)
library(mlr3autoiml)

task = tsk("penguins")
learner = lrn("classif.rpart", predict_type = "prob")
resampling = rsmp("cv", folds = 5)

auto = AutoIML$new(
  task = task,
  learner = learner,
  resampling = resampling,
  purpose = "global_insight",
  quick_start = FALSE,
  seed = 123
)

result = auto$run(verbose = FALSE)

# 1) Visual orientation first
auto$plot("overview")

# 2) Actionable next steps
guide = auto$guide(max_actions = 6)
guide$summary
guide$actions
guide$recommended_plots

# 3) Drill-down visuals (examples)
auto$plot("g2_effect", feature = task$feature_names[1], method = "auto")
auto$plot("g2_hstats")
auto$plot("g6_summary")

# 4) Evidence tables
report_card(result)
report_card_extended(result)

# 5) Reproducibility / reporting handoff
bundle_paths = export_audit_bundle(result, dir = "autoiml_audit_bundle", prefix = "run1")
bundle_paths
```

### How to interpret the workflow

- `overview`: first-pass visual narrative of structure + multiplicity evidence.
- `guide$actions`: prioritized "what to do next" recommendations.
- `report_card_extended()`: requirement-level evidence matrix for transparent reporting.
- `export_audit_bundle()`: one-command artifact export for audit and governance handoff.

### Typical user loop

- Start with `purpose = "global_insight"` and inspect `overview`.
- Apply top `guide$actions` (e.g., add subgroup fields, add alternative learners).
- Re-run AutoIML and verify IEL/claim-scope changes before strengthening conclusions.
- Export the audit bundle for reproducibility and review.

## Status

The package follows a claim-first, gate-based architecture with explicit semantics,
claim-scoped IELs, and evidence-driven gate planning. Default runs prioritize full
auditing paths; quick mode is available only for iterative prototyping. IEL-3 is
reserved for declared high-stakes contexts and requires human-factors evidence
(Gate 7B) in addition to technical gate evidence.

## License

LGPL-3.
