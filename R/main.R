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
  board_ui(
    ns("board"),
    board,
    plugins = custom_board_plugins(
      c(
        "preserve_board",
        "manage_blocks",
        "manage_links",
        "manage_stacks",
        "generate_code",
        "notify_user"
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
      ns <- session$n

      app_state <- reactiveValues(
        mode = "network",
        preview = FALSE,
        grid = NULL,
        in_grid = list(),
        refreshed = NULL,
        nodes = data.frame(),
        append_block = FALSE,
        added_block = NULL,
        removed_block = NULL,
        selected_block = NULL,
        edges = data.frame(),
        cancelled_edge = NULL,
        added_edge = NULL,
        removed_edge = NULL
      )

      # For shinytest2 (don't remove)
      exportTestValues(res = process_app_state(app_state))

      # Board module
      board_server(
        "board",
        board,
        plugins = custom_board_plugins(
          c(
            "preserve_board",
            "manage_blocks",
            "manage_links",
            "manage_stacks",
            "generate_code",
            "notify_user"
          )
        ),
        callbacks = list(
          grid = grid_server,
          app_mod = manage_app_mode,
          manage_sidebars = manage_sidebars,
          # Only one block can be visible at a time in the sidebar,
          # as only one block can be selected at a time in the network
          block_visibility = manage_block_visibility,
          # Callback to signal other modules that the restore is done.
          # This allows to restore each part in the correct order.
          on_board_restore = board_restore
        ),
        parent = app_state
      )
    }
  )
}
