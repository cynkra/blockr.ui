#' Dashboard Ui generic
#' @param id Module id.
#' @rdname dashboard
dashboard_ui <- function(id, board, ...) {
  UseMethod("dashboard_ui", board)
}

#' Dashboard grid server
#'
#' @param board Board reactiveValues. Read-only.
#' @param update Update reactiveVal to signal change to the board.
#' @param session Shiny session.
#' @param parent Parent global reactiveValues.
#' @param ... Extra parameters.
#' @rdname dashboard
#' @export
dashboard_server <- function(board, update, session, parent, ...) {
  UseMethod("dashboard_server", isolate(board$board))
}
