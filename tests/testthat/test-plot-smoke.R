test_that("create_single_plot runs for supported plot types", {
  make <- try(utils::getFromNamespace("create_single_plot", "exsampler"), silent = TRUE)
  skip_if(inherits(make, "try-error"), "create_single_plot not found or not exported")

  df <- data.frame(sample_data = rnorm(50))
  types <- c("qq_normal", "qq_detrended", "pp_plot", "histogram")

  for (t in types) {
    p <- make(df = df, var = "sample_data", plot_type = t)
    expect_true(inherits(p, "ggplot"))
  }
})
