# blockr.ui


<!-- index.md is generated from index.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![ci](https://github.com/cynkra/blockr.ui/actions/workflows/ci.yml/badge.svg)](https://github.com/cynkra/blockr.ui/actions/workflows/ci.yml)
[![CRAN
status](https://www.r-pkg.org/badges/version/blockr.ui)](https://CRAN.R-project.org/package=blockr.ui)
[![Codecov test
coverage](https://codecov.io/gh/cynkra/blockr.ui/graph/badge.svg)](https://app.codecov.io/gh/cynkra/blockr.ui)
<!-- badges: end -->

The goal of blockr.ui is to provide an alternative user interface for
[`{blockr.core}`](https://cynkra.github.io/blockr.core/).

## Installation

You can install the development version of blockr.ui from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.ui")
```

<p style="text-align: center;">
<img src="./man/figures/hex.png" style="width:50.0%" />
</p>

## Example

To run the demo app:

``` r
library(blockr.ui)
library(blockr.dplyr)
library(blockr.sdtm)
library(blockr.io)

Sys.setenv("DASHBOARD_TYPE" = "dock")
run_demo_app()
```

> **Note**
>
> The demo below runs with shinylive. Not all feature may work as
> expected due to compatibility issues with webR.

<iframe class="border border-5 rounded shadow-lg" src="https://shinylive.io/r/app/#h=0&amp;code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwwBXFkNwACBnFRF2CgLwKhAC1KlU7RAHpjBAJ4RJUDAHMWpHdLoYWRYyLGSMtOlmNCAJQCEL5MrBxcvILCohJSACao3OYM8koqapraYHoGRqYWVgw29o7Oru6e8T70-kEhYcxsnDz8QtXe7AmkMOnKqupauvqGJmaW1nYOTi5uHnHevvVgwaH04S1R7bFeUm79mUM5eWOFkyXT5XNVi1LLAash3CyMUAzmfJ1Ssmsvbx8vncMEkUgw-q8SoDvhhur0IQDPjC3GsQgBlczsWFwcgQABuMQAIgBBNEACQAQgB5YlYQkAfQAKgBNAAKAFEhNkhAkvA0IAxpBB6Qk4DAiPS0Kg+IEwABfAC6QA" style="zoom: 0.75;" width="100%" height="1100px"></iframe>

## App options

There are few **options** you can customize through setting up
environment variables:

-   `N_STACKS_COLORS`: how many colors to support in the stack color
    pickerInput. Default is 40.
-   `STACKS_COLOR_PALETTE`: the color palette type. Default is
    `spectral`. We use `hcl.colors` to setup the palette.
-   `DASHBOARD_TYPE`: either `grid` to leverage `{gridstackr}` or `dock`
    for `{dockVieweR}`. Default is `dock`.
    -   `DOCK_THEME`: when `DASHBOARD_TYPE` is `dock` you can customize
        the dock skin. Choose among
        `c("light", "abyss", "dark", "vs", "dracula", "replit")`.
        Default is `replit`.

## Development

JS code is managed by `esbuild`
[`{charpente}`](https://github.com/RinteRface/charpente?tab=readme-ov-file#using-esbuild-and-mocha).
To create a new JS file do and compile the entire project:

``` r
charpente::create_js("file-name")
charpente::build_js()
```
