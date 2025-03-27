#' Demo app
#'
#' Run demo app
#'
#' @param ... Forwarded to [new_board()]
#'
#' @export
run_demo_app <- function(...) {
  Sys.setenv("blockr_dark_mode" = "light")
  addResourcePath(
    "www/images",
    system.file("assets/images", package = utils::packageName())
  )
  # TBD customize blockr options
  app_board <- new_board(
    ...,
    class = "custom_board",
    options = new_board_options(dark_mode = "light")
  )

  ui <- page_fillable(
    shinyjs::useShinyjs(),
    main_ui("main", app_board)
  )

  server <- function(input, output, session) {
    main_server("main", app_board)
  }

  shinyApp(add_blockr.ui_deps(ui), server)
}
