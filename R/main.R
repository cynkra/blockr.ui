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
    plugins = list(
      preseve_board = ser_deser_ui,
      manage_blocks = add_rm_block_ui,
      manage_links = add_rm_link_ui
    )
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
        my_board_ui[[3]],
        my_board_ui[[1]]$children[[2]]$sidebar
      ),
      # Action bar
      actions_ui(my_board_ui[[1]]$children[[2]]$toolbar, ns = ns),
      my_board_ui[[1]]$children[[3]],
      # Notifications
      my_board_ui[[2]]
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

      # Toggle sidebars based on the mode and selected node
      manage_sidebars(rv, reactive(board_out$selected_block), session)

      # Re-hide sidebar when block is removed
      observeEvent(board_out$removed_block, {
        bslib::toggle_sidebar("properties", open = FALSE)
      })

      observeEvent(board_out$links, {
        #browser()
      })

      board_out <- board_server(
        "board",
        board,
        plugins = list(
          preseve_board = ser_deser_server,
          manage_blocks = add_rm_block_server,
          manage_links = add_rm_link_server,
          notify_user = block_notification_server
        ),
        callbacks = list(
          block_visibility = manage_block_visibility
        ),
        parent = rv
      )
    }
  )
}
