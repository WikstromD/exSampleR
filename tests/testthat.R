if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(exsampler)
  test_check("exsampler")
} else {
  message("Skipping tests: 'testthat' not installed.")
}
