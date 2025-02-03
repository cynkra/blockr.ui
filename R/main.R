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
      preserve_board = ser_deser_ui,
      manage_blocks = add_rm_block_ui,
      manage_links = add_rm_link_ui
    )
  )
  my_grid <- grid_ui(ns("grid"))

  # TO DO: rework this my_board_ui[[1]]$children[[2]]$sidebar
  # this is ugly and will break

  tagList(
    # Action bar
    actions_ui(
      my_board_ui[[1]]$children[[1]],
      v_rule(),
      my_board_ui[[1]]$children[[2]]$toolbar,
      ns = ns
    ),
    layout_sidebar(
      border = FALSE,
      class = "p-0",
      sidebar = sidebar(
        id = ns("dashboard"),
        title = "Dashboard",
        position = "right",
        width = "75%",
        open = FALSE,
        # GRID CONTENT
        my_grid[c(2, 3)]
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
          my_grid[[1]],
          my_board_ui[[1]]$children[[2]]$sidebar
        ),
        my_board_ui[[1]]$children[[3]],
        # Notifications
        my_board_ui[[2]]
      )
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

      vals <- reactiveValues(mode = NULL)

      # For shinytest2 (don't remove)
      exportTestValues(vals = vals, grid = list())

      # App mode
      observeEvent(input$mode, {
        if (input$mode) vals$mode <- "network" else vals$mode <- "dashboard"
      })

      # When restoring a snapshot we restore the old mode
      observeEvent(board_out$refreshed, {
        val <- if (board_mode(board_out$board) == "dashboard") FALSE else TRUE
        update_switch("mode", value = val)
      })

      # Toggle sidebars based on the mode and selected node
      manage_sidebars(
        vals,
        reactive(board_out$selected_block),
        reactive(board_out$removed_block),
        session
      )

      # Board module
      board_out <- board_server(
        "board",
        board,
        plugins = list(
          preserve_board = ser_deser_server,
          manage_blocks = add_rm_block_server,
          manage_links = add_rm_link_server,
          notify_user = block_notification_server
        ),
        callbacks = list(
          block_visibility = manage_block_visibility,
          serialize_extra = capture_for_serialize,
          # Callback to signal other modules that the restore is done.
          # This allows to restore each part in the correct order.
          on_board_restore = board_restore
        ),
        parent = vals
      )

      # grid module
      grid_out <- grid_server(
        "grid",
        board_out,
        reactive(vals$mode),
        blocks_ns = "main-board"
      )

      observeEvent(grid_out$grid(), {
        vals$grid <- grid_out$grid()
      })

      observeEvent(grid_out$grid_restored(), {
        vals$grid_restored <- grid_out$grid_restored()
      })
    }
  )
}
