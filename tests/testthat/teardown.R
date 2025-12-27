# FILE: tests/testthat/teardown.R

# nocov start
if (exists("old_opts", inherits = FALSE)) {
  options(old_opts)
}

if (exists("lg", inherits = FALSE) && !is.null(lg) && exists("old_threshold", inherits = FALSE) && !is.null(old_threshold)) {
  lg$set_threshold(old_threshold)
}

if (requireNamespace("future", quietly = TRUE) && exists("old_plan", inherits = FALSE) && !is.null(old_plan)) {
  future::plan(old_plan)
}
# nocov end
