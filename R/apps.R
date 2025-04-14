#' Demo app
#'
#' Run demo app
#'
#' @param ... Forwarded to [new_board()]
#'
#' @export
run_demo_app <- function(...) {
  serve(
    new_custom_board(...),
    "main"
  )
}

#' @rdname run_demo_app
#' @export
new_custom_board <- function(...) {

  # TBD customize blockr options
  n_stacks <- 40
  stacks_color_palette <- "spectral"
  if (nchar(Sys.getenv("N_STACKS_COLORS")) > 0) {
    n_stacks <- Sys.getenv("N_STACKS_COLORS")
  }
  if (nchar(Sys.getenv("STACKS_COLOR_PALETTE")) > 0) {
    stacks_color_palett <- Sys.getenv("STACKS_COLOR_PALETTE")
  }

  new_board(
    ...,
    class = "custom_board",
    options = new_board_options(
      dark_mode = "light",
      stacks_colors = hcl.colors(n_stacks, palette = stacks_color_palette)
    )
  )
}

#' @export
serve.custom_board <- function(x, id = "main", ...) {

  Sys.setenv("blockr_dark_mode" = "light")

  addResourcePath(
    "www/images",
    system.file("assets/images", package = utils::packageName())
  )

  ui <- page_fillable(
    shinyjs::useShinyjs(),
    main_ui(id, x)
  )

  server <- function(input, output, session) {
    main_server(id, x)
  }

  shinyApp(add_blockr.ui_deps(ui), server)
}
