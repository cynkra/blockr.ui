library(blockr.ui)
library(bslib)
library(shiny)

ui <- page_fillable(
  board_ui("board")
)

server <- function(input, output, session) {
  board_server("board")
}

shinyApp(ui, server)