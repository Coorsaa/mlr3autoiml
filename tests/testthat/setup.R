# FILE: tests/testthat/setup.R
#
# This setup follows the mlr3 test style: strict partial matching warnings
# and reduced logging noise.

# nocov start
old_opts = options(
  warnPartialMatchArg = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchAttr = TRUE
)

# Silence mlr3 logging for tests (mlr3 exports `lg`; otherwise use lgr).
if (exists("lg", inherits = TRUE)) {
  lg = get("lg", inherits = TRUE)
} else if (requireNamespace("lgr", quietly = TRUE)) {
  lg = lgr::get_logger("mlr3")
} else {
  lg = NULL
}

old_threshold = NULL
if (!is.null(lg)) {
  old_threshold = lg$threshold
  lg$set_threshold("warn")
}

old_plan = NULL
if (requireNamespace("future", quietly = TRUE)) {
  old_plan = future::plan()
  future::plan("sequential")
}
# nocov end
