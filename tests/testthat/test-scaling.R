test_that("scale_to_* produce bounded outputs", {
  f01 <- try(utils::getFromNamespace("scale_to_0_10", "exsampler"), silent = TRUE)
  f11 <- try(utils::getFromNamespace("scale_to_1_10", "exsampler"), silent = TRUE)

  skip_if(inherits(f01, "try-error") && inherits(f11, "try-error"), "scaling helpers not found")

  set.seed(1)
  x <- rnorm(100)

  if (!inherits(f01, "try-error")) {
    y0 <- f01(x)
    expect_true(is.numeric(y0))
    expect_true(all(is.finite(y0)))
    expect_gte(min(y0, na.rm = TRUE), 0)
    expect_lte(max(y0, na.rm = TRUE), 10)
  }
  if (!inherits(f11, "try-error")) {
    y1 <- f11(x)
    expect_true(is.numeric(y1))
    expect_true(all(is.finite(y1)))
    expect_gte(min(y1, na.rm = TRUE), 1)
    expect_lte(max(y1, na.rm = TRUE), 10)
  }
})
