# data-raw/ExSample.R
# Regenerate exSample_Distributions and save to data/

if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all()

exSample_Distributions <- generate_exsample_distributions(n = 1000, seed = 123)

usethis::use_data(exSample_Distributions, overwrite = TRUE)
