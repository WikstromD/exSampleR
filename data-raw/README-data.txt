This package's dataset `exSample_Distributions` is generated from R/distributions.R
via the function `generate_exsample_distributions()`.

To regenerate the data:
1) Open an R session at the package root.
2) Run: source("data-raw/ExSample.R")
   (This will call devtools::load_all(), generate the data, and save data/exSample_Distributions.rda)
3) Rebuild: devtools::document(); devtools::install()

Note: The CSV/Pickle artifacts were removed to avoid drift. The canonical package data is the .rda file.
