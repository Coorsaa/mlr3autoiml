# FILE: tests/testthat/helper-mlr3.R
#
# Optionally source mlr3's internal test helpers. This mirrors the pattern
# recommended by mlr3 (helpers live in inst/testthat within mlr3).

if (requireNamespace("mlr3", quietly = TRUE)) {
  library(mlr3)
  helper_dir = system.file("testthat", package = "mlr3")
  helper_files = list.files(helper_dir, pattern = "^helper.*\\.R$", full.names = TRUE)
  if (length(helper_files)) {
    for (f in helper_files) {
      source(f, local = TRUE)
    }
  }
}
