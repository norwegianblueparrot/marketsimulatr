
<!-- README.md is generated from README.Rmd. Please edit that file -->

# marketsimulatr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of marketsimulatr is to simulate a financial market with an
active market maker. The package models a continuous-time, stochastic,
double auction, where buyers and sellers randomly submit orders.

Market and limit orders are allowed. If market orders are submitted, and
sufficient market depth is available, a weighted average price is
calculated. If there is insufficient market depth to fill market orders,
the market maker intervenes, and offers a price 5% above/below the
current best bid/ask.

A more detailed explanation of the model is available in the vignette.

## Installation

You can install marketsimulatr from [GitHub](https://www.github.com)
with:

``` r
remotes::install_github("norwegianblueparrot/marketsimulatr")
```

## Configuration

The simulator ships with three configurations (via the config package):
`default`, `test`, and `production`. The configs are stored in
`config.yml`. At the moment, the configs only change the number of days’
trading to simulate.

## Reproducibility

The code comes bundled with a renv environment. To replicate my
development environment, pull the source code from GitHub and run:

``` r
renv::restore()
```

Once you have installed the required packages you can run the code by
building the source using:

``` r
devtools::load_all()
```
