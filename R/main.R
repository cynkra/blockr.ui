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
      manage_links = add_rm_link_ui,
      generate_code = gen_code_ui
    )
  )
  my_grid <- grid_ui(ns("grid"))

  # TO DO: rework this my_board_ui[[1]]$children[[2]]$sidebar
  # this is ugly and will break

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
        my_board_ui[[1]]$children[[1]]$restore,
        tags$li(
          tags$h6(
            class = "dropdown-header",
            "Grid options"
          )
        ),
        my_grid[[2]]
      ),
      actions_ui(
        div(
          class = "btn-group btn-group-sm",
          role = "group",
          my_board_ui[[1]]$children[[2]]$toolbar, # new block
          my_board_ui[[1]]$children[[4]], # show code
          my_board_ui[[1]]$children[[1]]$buttons, # undo/redo
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
      # For spacing
      div()
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
        my_grid[[3]]
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

      vals <- reactiveValues(
        mode = "network",
        preview = FALSE,
        grid = NULL,
        grid_restored = NULL,
        in_grid = list()
      )

      # For shinytest2 (don't remove)
      exportTestValues(vals = vals)

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
      observeEvent(req(board_out$refreshed == "network"), {
        if (board_mode(board_out$board) == "dashboard") {
          shinyjs::click("mode")
        }
      })

      # Toggle sidebars based on the mode and selected node
      manage_sidebars(
        vals,
        reactive(board_out$selected_block),
        reactive(board_out$removed_block),
        session
      )

      # Viewer mode: maximize dashboard view to save space
      observeEvent(input$preview, {
        toggle_preview(vals, session)
      })

      # Board module
      board_out <- board_server(
        "board",
        board,
        plugins = list(
          preserve_board = ser_deser_server,
          manage_blocks = add_rm_block_server,
          manage_links = add_rm_link_server,
          notify_user = block_notification_server,
          generate_code = gen_code_server
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
        reactive(vals$in_grid),
        blocks_ns = "main-board"
      )

      observeEvent(grid_out$grid(), {
        vals$grid <- grid_out$grid()
      })

      observeEvent(grid_out$in_grid(), {
        vals$in_grid <- grid_out$in_grid()
      })

      observeEvent(grid_out$grid_restored(), {
        vals$grid_restored <- grid_out$grid_restored()
      })
    }
  )
}
