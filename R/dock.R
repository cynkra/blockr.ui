#' Dashboard dock UI
#'
#' @param id Module id.
#' @rdname dock
#' @export
dock_ui <- function(id) {
  ns <- NS(id)
  list(
    add_to_grid = bslib::input_switch(
      ns("add_to_grid"),
      "Use in dashboard?"
    ),
    options = tagList(),
    content = div(
      id = ns("grid_zoom_target"),
      style = "zoom: 0.5;",
      dock_viewOutput(ns("dock"))
    )
  )
}


#' Dashboard dock server
#'
#' @param board Board reactiveValues. Read-only.
#' @param update Update reactiveVal to signal change to the board.
#' @param parent Parent global reactiveValues.
#' @param ... Extra parameters.
#' @rdname dock
#' @export
dock_server <- function(board, update, parent, ...) {
  session <- get("session", parent.frame(1))
  input <- session$input
  ns <- session$ns
  output <- session$output

  output$dock <- renderDock_view({
    dock_view(
      panels = list(
        dockViewR::panel(1, "Panel1", "My panel")
      ),
      theme = if (nchar(Sys.getenv("DOCK_THEME")) > 0)
        Sys.getenv("DOCK_THEME") else "replit"
    )
  })
}
