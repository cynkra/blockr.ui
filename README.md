
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

To run the app:

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ai)
library(blockr.ui)
library(bslib)
library(shiny)

shiny::addResourcePath(
  "assets",
  system.file("examples/demo/www", package = "blockr.ui")
)

app_board <- new_board(class = "custom_board")

ui <- page_fillable(
  shinyjs::useShinyjs(),
  tags$script(src = "assets/custom.js"),
  main_ui("main", app_board)
)

server <- function(input, output, session) {
  main_server("main", app_board)
}

shinyApp(ui, server)
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
