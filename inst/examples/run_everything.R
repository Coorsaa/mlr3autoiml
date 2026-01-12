# Example: run the full AutoIML workflow and inspect all outputs

library(mlr3)
library(mlr3autoiml)

# --- Setup --------------------------------------------------------------

task <- tsk("penguins")
learner <- lrn("classif.rpart", predict_type = "prob")
resampling <- rsmp("cv", folds = 5)

auto <- AutoIML$new(
  task = task,
  learner = learner,
  resampling = resampling,
  purpose = "global_insight",
  quick_start = FALSE,
  seed = 123
)

# Optional Gate 2 configuration (persisted in ctx)
auto$ctx$structure$sample_n <- 200
auto$ctx$structure$max_features <- 15

# Grid choices (to match 'iml' defaults):
auto$ctx$structure$grid_n <- 10
auto$ctx$structure$grid_type <- "equidist"    # iml default

# ALE bins (quantile grid):
auto$ctx$structure$ale_bins <- 10

# ICE heterogeneity metric uses centered ICE (c-ICE)
auto$ctx$structure$ice_center <- "mean"

# Optional: interaction-aware regionalization (GADGET)
auto$ctx$structure$regionalize <- TRUE
auto$ctx$structure$regional_method <- "auto"  # follow Gate 2 recommendation

# --- Run ---------------------------------------------------------------
result <- auto$run(verbose = TRUE)

# --- Overview ----------------------------------------------------------
auto$overview()

# --- Tables ------------------------------------------------------------
auto$report_card()

g2_tables <- auto$tables("g2")
str(g2_tables, max.level = 1)

# --- Plots (requires ggplot2; patchwork is optional) -------------------

# Gate 1: CV score trend
p_g1 <- auto$plot("g1_scores", measure = "classif.logloss")
print(p_g1)

# Gate 2: ICE spread (heterogeneity)
p_ice <- auto$plot("g2_ice_spread", class_label = "Adelie", top_n = 10)
print(p_ice)

# Gate 2: Global effect (method = auto picks ALE if dependence flagged)
p_eff <- auto$plot("g2_effect", feature = "body_mass", class_label = "Adelie", method = "auto", show_ice = TRUE)
print(p_eff)

# Gate 2: interaction screening (bar plot)
p_h <- auto$plot("g2_hstats", class_label = "Adelie", top_n = 12)
print(p_h)

# Gate 2: regionalization plot (only if regionalization created regions)
# p_gadget <- auto$plot("g2_gadget", feature = "body_mass", class_label = "Adelie")
# print(p_gadget)

# --- Local SHAP (Shapley) ---------------------------------------------
# Pick a row id present in the task
row_id <- task$row_ids[1]

# Compute attributions (this can take a few seconds depending on sample_size)
shap_dt <- auto$shap(row_id = row_id, class_label = "Adelie", sample_size = 100, background_n = 200)
print(shap_dt[order(-abs(phi))][1:10])

p_shap <- auto$plot("shap_local", row_id = row_id, class_label = "Adelie", sample_size = 100, top_n = 12)
print(p_shap)

