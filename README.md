
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.ui

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
`{blockr.core}`.

## Installation

You can install the development version of blockr.ui from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.ui")
```

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

### Create a new block

![](./man/figures/blockr-new.png)

### Block properties

![](./man/figures/blockr-properties.png)

### Append block and invalid state

![](./man/figures/blockr-invalid.png)

### Join independant data

![](./man/figures/blockr-join.png)

### Rearrange output on a grid

![](./man/figures/blockr-dashboard.png)

## App options

There are few **options** you can customize through setting up
environment variables:

- `N_STACKS_COLORS`: how many colors to support in the stack color
  pickerInput. Default is 40.
- `STACKS_COLOR_PALETTE`: the color palette type. Default is `spectral`.
  We use `hcl.colors` to setup the palette.
- `DASHBOARD_TYPE`: Default is `dock` to leverage `{dockViewR}`. Other
  implementation may come later.
  - `DOCK_THEME`: when `DASHBOARD_TYPE` is `dock` you can customize the
    dock skin. Choose among
    `c("light", "abyss", "dark", "vs", "dracula", "replit")`. Default is
    `replit`.

## Development

JS code is managed by `esbuild`
[`{charpente}`](https://github.com/RinteRface/charpente?tab=readme-ov-file#using-esbuild-and-mocha).
To create a new JS file do and compile the entire project:

``` r
charpente::create_js("file-name")
charpente::build_js()
```
