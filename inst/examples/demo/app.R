library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)
library(bslib)
library(shiny)

shiny::addResourcePath("assets", "./inst/examples/demo/www")

ui <- page_fillable(
  shinyjs::useShinyjs(),
  tags$script(src = "assets/custom.js"),
  main_ui("board")
)

server <- function(input, output, session) {
  main_server("board")
}

shinyApp(ui, server)
