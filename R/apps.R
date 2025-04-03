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
  n_stacks <- 40
  stacks_color_palette <- "spectral"
  if (nchar(Sys.getenv("N_STACKS_COLORS")) > 0) {
    n_stacks <- Sys.getenv("N_STACKS_COLORS")
  }
  if (nchar(Sys.getenv("STACKS_COLOR_PALETTE")) > 0) {
    stacks_color_palett <- Sys.getenv("STACKS_COLOR_PALETTE")
  }

  app_board <- new_board(
    ...,
    class = "custom_board",
    options = new_board_options(
      dark_mode = "light",
      stacks_colors = hcl.colors(n_stacks, palette = stacks_color_palette)
    )
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
