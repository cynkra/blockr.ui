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
    plugins = dash_board_plugins(
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

#' Create app state generic
#'
#' @param board Board object.
#'
#' @rdname main
#' @export
create_app_state <- function(board) {
  UseMethod("create_app_state", board)
}

#' Create app state dock board method
#'
#' @rdname main
#' @export
create_app_state.dock_board <- function(board) {
  reactiveValues(
    mode = "network",
    cold_start = TRUE,
    preview = FALSE,
    grid = structure(list(), class = "dock"),
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
    removed_edge = NULL,
    added_stack = NULL,
    stack_added_block = NULL,
    stack_removed_block = NULL,
    removed_stack = NULL
  )
}

#' Create app state grid board method
#'
#' @rdname main
#' @export
create_app_state.grid_board <- function(board) {
  reactiveValues(
    mode = "network",
    cold_start = TRUE,
    preview = FALSE,
    grid = data.frame(),
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
    removed_edge = NULL,
    added_stack = NULL,
    stack_added_block = NULL,
    stack_removed_block = NULL,
    removed_stack = NULL
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

      app_state <- create_app_state(board)

      # Indicate whether we start from an empty board or not
      # This is needed by downtream plugins such as links ...
      observeEvent(TRUE, {
        if (length(board_blocks(board))) app_state$cold_start <- FALSE
      })

      # For shinytest2 (don't remove)
      exportTestValues(res = process_app_state(app_state))

      # Board module
      board_server(
        "board",
        board,
        plugins = dash_board_plugins(
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
          grid = dashboard_server,
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
