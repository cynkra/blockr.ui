#' Main ui for blockr2
#'
#' The board is composed of 2 views pointing to the network module
#' or the grid/dashboard module.
#'
#' @param id Unique id.
#' @rdname main
#' @export
main_ui <- function(id, board) {
  ns <- NS(id)

  my_board_ui <- board_ui(
    ns("board"),
    board,
    ser_deser = ser_deser_ui,
    add_rm_block = add_rm_block_ui,
    add_rm_link = add_rm_link_ui
  )

  layout_sidebar(
    class = "p-0",
    sidebar = sidebar(
      id = ns("dashboard"),
      title = "Dashboard",
      position = "right",
      width = "60%",
      open = FALSE,
      "Grid TBD"
      # GRID CONTENT
    ),
    layout_sidebar(
      border = FALSE,
      sidebar = sidebar(
        id = ns("properties"),
        title = "Block properties",
        open = FALSE,
        width = "40%",
        position = "right",
        "TBD block ui"
      ),
      # Action bar
      actions_ui(ns = ns),
      "TBD canvas",
      my_board_ui
    )
  )
}

#' Main server function
#'
#' Server module for board.
#'
#' @param board Board object.
#'
#' @rdname main
#' @export
main_server <- function(id, board) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(mode = NULL)

      # For shinytest2 (don't remove)
      exportTestValues(rv = rv)

      # App mode
      observeEvent(input$mode, {
        if (input$mode) rv$mode <- "network" else rv$mode <- "dashboard"
      })

      manage_sidebars(rv, reactive(""), session)

      board_out <- board_server(
        "board",
        board,
        ser_deser = ser_deser_server,
        add_rm_block = add_rm_block_server,
        add_rm_link = add_rm_link_server,
        block_notifications = block_notification_server
      )
    }
  )
}
