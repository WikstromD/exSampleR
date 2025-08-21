
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exsampler

<!-- badges: start -->

[![R-CMD-check](https://github.com/WikstromD/exsampler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WikstromD/exsampler/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/WikstromD/exsampler/graph/badge.svg)](https://app.codecov.io/gh/WikstromD/exsampler)
[![test-coverage](https://github.com/WikstromD/exsampler/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/WikstromD/exsampler/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The exsampler package is a tool to assist visual inspection of (non)normality. It displays a 3×3 grids of either histograms, QQ plots, detrended QQ plots, or PP Plots where one randomly assigned plot contains the real data and the seven other plots contain simulated normal samples (Shapiro–Wilk p's ≤ 0.01) with the same number of observations, mean, and standard deviation (and optionally, the same rounding). Plots with simulated normal data help calibrate the user's eyes for visually inspecting the distribution of their data. Selecting a plot reveals the location of the real data. Users can choose to view plots from an example dataset with labeled distributions, or use with their own data already loaded in their global environment.

## Installation

You can install the development version of exsampler from
[GitHub](https://github.com/) with:

``` r
install.packages("pak") # install 'pak' if needed
pak::pak("WikstromD/exsampler")
```

