test_that("generate_exsample_distributions is deterministic with a seed", {
  gen <- try(utils::getFromNamespace("generate_exsample_distributions", "exSampleR"), silent = TRUE)
  skip_if(inherits(gen, "try-error"), "generator not found")

  a <- gen(n = 100, seed = 123)
  b <- gen(n = 100, seed = 123)

  expect_identical(a, b)
})
