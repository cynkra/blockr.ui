#' Demo app
#'
#' Run demo app
#'
#' @export
run_demo_app <- function() {
  app_board <- new_board(class = "custom_board")

  ui <- page_fillable(
    shinyjs::useShinyjs(),
    main_ui("main", app_board)
  )

  server <- function(input, output, session) {
    main_server("main", app_board)
  }

  shinyApp(add_blockr.ui_deps(ui), server)
}
