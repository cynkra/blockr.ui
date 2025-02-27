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
    plugins = custom_board_plugins(
      c(
        "preserve_board",
        "manage_blocks",
        "manage_links",
        "generate_code",
        "notify_user"
      )
    )
  )

  # TO DO: move the grid at the level of the board?
  # Inject it within a callback
  my_grid <- grid_ui(ns("grid"))

  # TO DO: maybe this entire thing may go the board_ui.custom_board?
  tagList(
    div(
      class = "d-flex align-items-center justify-content-around gap-5",
      # Action bar
      dropdown_button(
        icon = icon("bars"),
        tags$li(
          tags$h6(
            class = "dropdown-header",
            "Save and Restore"
          )
        ),
        my_board_ui$toolbar_ui$preserve_board$restore,
        tags$li(
          tags$h6(
            class = "dropdown-header",
            "Grid options"
          )
        ),
        my_grid$options
      ),
      actions_ui(
        div(
          class = "btn-group btn-group-sm",
          role = "group",
          my_board_ui$toolbar_ui$manage_blocks$toolbar,
          my_board_ui$toolbar_ui$generate_code,
          my_board_ui$toolbar_ui$preserve_board$buttons,
          actionButton(
            ns("preview"),
            "Preview",
            icon = icon("eye")
          ),
          actionButton(
            ns("mode"),
            "Mode",
            icon = icon("network-wired")
          )
        ),
        ns = ns
      ),
      div()
      #my_board_ui$board_options_ui
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
        padding = c("0px", "10px"),
        # GRID CONTENT
        my_grid$content
      ),
      layout_sidebar(
        border = FALSE,
        sidebar = sidebar(
          id = ns("properties"),
          title = "Block properties",
          open = FALSE,
          width = "40%",
          position = "right",
          padding = c("0px", "10px"),
          my_board_ui$blocks_ui,
          my_grid$add_to_grid,
          my_board_ui$toolbar_ui$manage_blocks$sidebar
        ),
        my_board_ui$toolbar_ui$manage_links,
        # Notifications
        my_board_ui$notifications
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

      vals <- reactiveValues(
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
      exportTestValues(
        mode = vals$mode,
        preview = vals$preview,
        in_grid = vals$in_grid
      )

      # App mode
      observeEvent(
        input$mode,
        {
          if (input$mode %% 2 == 0) vals$mode <- "network" else
            vals$mode <- "dashboard"

          if (vals$mode == "network" && input$preview %% 2 != 0) {
            shinyjs::click("preview")
          }
          updateActionButton(
            session,
            "mode",
            icon = if (vals$mode == "network") icon("network-wired") else
              icon("table-columns")
          )
        }
      )

      # When restoring a snapshot we restore the old mode
      observeEvent(req(vals$refreshed == "network"), {
        if (vals$mode == "dashboard") {
          shinyjs::click("mode")
        }
      })

      # Toggle sidebars based on the mode and selected node
      manage_sidebars(vals, session)

      # Viewer mode: maximize dashboard view to save space
      observeEvent(input$preview, {
        toggle_preview(vals, session)
      })

      # Board module
      board_out <- board_server(
        "board",
        board,
        plugins = custom_board_plugins(
          c(
            "preserve_board",
            "manage_blocks",
            "manage_links",
            "generate_code",
            "notify_user"
          )
        ),
        callbacks = list(
          # Only one block can be visible at a time in the sidebar,
          # as only one block can be selected at a time in the network
          block_visibility = manage_block_visibility,
          # Callback to signal other modules that the restore is done.
          # This allows to restore each part in the correct order.
          on_board_restore = board_restore
        ),
        parent = vals
      )

      # grid module
      grid_server(
        "grid",
        vals,
        reactive(board_out[[1]]$blocks),
        blocks_ns = "main-board"
      )

      observeEvent(vals$refreshed == "grid", {
        vals$refreshed <- NULL
      })
    }
  )
}
