if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(exSampleR)
  test_check("exSampleR")
} else {
  message("Skipping tests: 'testthat' not installed.")
}
