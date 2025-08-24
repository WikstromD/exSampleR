---
title: "exsampler"
---

## Overview
This page provides an overview of the exsampler package.

**exsampler** is a tool to assist visual inspection of (non)normality. It displays a 3×3 grids of either histograms, QQ plots, detrended QQ plots, or PP Plots where one randomly assigned plot contains the real data and the seven other plots contain data sampled from simulated normal distributions (Shapiro–Wilk p's > 0.05) with the same number of observations, mean, and standard deviation (and optionally, the same rounding). 

Plots from simulated data help calibrate the user's eyes for visually inspecting the distribution of their data. Selecting a plot reveals the location of the real data. Simulated data come from random sampling of normal distributions; while distributions pass the widely-used Shapiro-Wilk cutoff of p > 0.05, users will see that this does not guarantee true normal (gaussian) distributions. 

This application is intended for educational purposes and not meant for formal assessment of normality.

**Read more:**
- [Get started](articles/exsampler-get-started.html)
- [References](articles/exsampler-references.html)