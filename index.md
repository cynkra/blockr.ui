
<!-- index.md is generated from index.Rmd. Please edit that file -->

# blockr.ui

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![ci](https://github.com/cynkra/blockr.ui/actions/workflows/ci.yml/badge.svg)](https://github.com/cynkra/blockr.ui/actions/workflows/ci.yml)
[![CRAN
status](https://www.r-pkg.org/badges/version/blockr.ui)](https://CRAN.R-project.org/package=blockr.ui)
<!-- badges: end -->

The goal of blockr.ui is to provide an alternative user interface for
`{blockr.core}`.

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
library(blockr.dplyr)
library(blockr.ui)
run_demo_app()
```

<iframe src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwwBXFkNwACBnFRF2CgLwKhAC1KlU7RAHpjBAJ4RJUDAHMWpHdLoYWRYyLGSMtOlmNCAJQCEL5MrBxcvILCohJSACao3OYM8koqapraYHoGRqYWVgw29o7Oru6e8T70-kEh3CyMUAzmfNXeSSkMwRBNLW0dcd6yfQzSEAD6CXAwRFNoqHyBYAC+ALpAA" width="150%" height="900px"></iframe>

## Development

JS code is managed by `esbuild`
[`{charpente}`](https://github.com/RinteRface/charpente?tab=readme-ov-file#using-esbuild-and-mocha).
To create a new JS file do and compile the entire project:

``` r
charpente::create_js("file-name")
charpente::build_js()
```
