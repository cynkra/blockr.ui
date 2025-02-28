#' Demo app
#'
#' Run demo app
#'
#' @export
run_demo_app <- function() {
  Sys.setenv("blockr.blockr_dark_mode" = FALSE)
  addResourcePath(
    "www/images",
    system.file("assets/images", package = utils::packageName())
  )
  # TBD customize blockr options
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
