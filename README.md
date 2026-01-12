# mlr3autoiml

`mlr3autoiml` is an **auditing and automation layer** for interpretable machine learning (IML)
on top of the **mlr3 ecosystem**. It implements a **gate-based workflow** (G0A/G0B, G1–G6, G7A/G7B) and assigns
**claim-scoped Interpretation Readiness Levels (IRL-0…3)** to scope the strength of permissible interpretive claims.

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

## Status

This is a scaffold that provides a working, end-to-end structure and core computations
for Gates 0–5 with additional scaffolding for Gates 6–7. You can extend gates incrementally.

## License

LGPL-3.
