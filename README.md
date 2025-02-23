
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.ui

<!-- badges: start -->
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
library(blockr.dplyr)
library(blockr.ai)
library(blockr.ui)

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
