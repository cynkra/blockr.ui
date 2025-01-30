library(blockr.core)
library(blockr.dplyr)
#library(blockr.ai)
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
