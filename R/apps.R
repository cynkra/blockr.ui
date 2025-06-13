#' Demo app
#'
#' Run demo app
#'
#' @param ... Forwarded to \link[blockr.core]{new_board}.
#'
#' @export
run_demo_app <- function(...) {
  serve(
    new_dash_board(...),
    "main"
  )
}

#' @rdname run_demo_app
#' @export
new_dash_board <- function(...) {
  # TBD customize blockr options
  n_stacks <- 40
  stacks_color_palette <- "spectral"
  if (nchar(Sys.getenv("N_STACKS_COLORS")) > 0) {
    n_stacks <- Sys.getenv("N_STACKS_COLORS")
  }
  if (nchar(Sys.getenv("STACKS_COLOR_PALETTE")) > 0) {
    stacks_color_palett <- Sys.getenv("STACKS_COLOR_PALETTE")
  }

  dashboard_type <- "dock"
  if (nchar(Sys.getenv("DASHBOARD_TYPE")) > 0) {
    dashboard_type <- validate_dashboard_type(Sys.getenv("DASHBOARD_TYPE"))
  }

  snapshot_location <- tempdir()
  if (nchar(Sys.getenv("SNAPSHOT_LOCATION")) > 0) {
    snapshot_location <- Sys.getenv("SNAPSHOT_LOCATION")
  }

  auto_snapshot <- FALSE
  if (nchar(Sys.getenv("AUTO_SNAPSHOT")) > 0) {
    auto_snapshot <- as.logical(Sys.getenv("AUTO_SNAPSHOT"))
  }

  new_board(
    ...,
    class = c(sprintf("%s_board", dashboard_type), "dash_board"),
    options = new_board_options(
      dark_mode = "light",
      stacks_colors = hcl.colors(n_stacks, palette = stacks_color_palette),
      dashboard_type = dashboard_type,
      dashboard_zoom = 1,
      snapshot = list(
        location = snapshot_location,
        auto = auto_snapshot
      )
    )
  )
}

#' @keywords internal
validate_dashboard_type <- function(type = c("dock")) {
  type <- match.arg(type)
}

#' @export
serve.dash_board <- function(x, id = "main", ...) {
  Sys.setenv("blockr_dark_mode" = "light")

  ui <- page_fillable(
    padding = 0,
    gap = 0,
    shinyjs::useShinyjs(),
    custom_icons(),
    add_busy_load_deps(main_ui(id, x))
  )

  server <- function(input, output, session) {
    main_server(id, x)
  }

  shinyApp(add_blockr.ui_deps(ui), server)
}
