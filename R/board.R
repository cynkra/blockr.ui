#' Main ui for blockr2
#'
#' The board is composed of 2 views pointing to the network module
#' or the grid/dashboard module.
#'
#' @param id Unique id.
#' @rdname main
#' @export
main_ui <- function(id) {
  ns <- NS(id)

  network_ui <- network_ui(ns("dag"))

  layout_sidebar(
    class = "p-0",
    sidebar = grid_ui(ns),
    layout_sidebar(
      border = FALSE,
      sidebar = properties_ui(network_ui$sidebar, ns = ns),
      actions_ui(network_ui$action_bar, ns = ns),
      network_ui$canvas
    )
  )
}

#' Main server function
#'
#' Server module for board.
#'
#' @rdname main
#' @export
main_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # The board must know about the blocks and connections between them
      rv <- reactiveValues(
        blocks = list(),
        connections = list(),
        obs = list(),
        # Cache where nodes should be in the dashboard mode.
        grid = data.frame(),
        mode = "network"
      )

      # For shinytest2 (don't remove)
      exportTestValues(
        blocks = rv$blocks,
        network_out = network_out
      )

      # Board mode
      observeEvent(input$mode, {
        if (input$mode) rv$mode <- "network" else rv$mode <- "dashboard"
      })

      manage_sidebars(rv, network_out$selected_node, session)

      # DAG representation
      network_out <- network_server("dag", rv)

      # Dashboard mode
      #dashboard_out <- dashboard_server("dash")

      # Manage new connections
      observeEvent(req(network_out$added_edge()), {
        # In some cases like the join block, multiple edges can
        # be added
        for (edge in network_out$added_edge()) {
          rv <- add_connection(edge, network_out$edges(), rv)
        }
      })

      # When an edge is removed, we reset the correponding connection
      # so that blocks don't show outdated data ...
      observeEvent(req(network_out$removed_edge()), {
        # As removing a node may remove multiple edges ...
        # we need to loop over ...
        for (con in network_out$removed_edge()) {
          rv <- remove_connection(con, rv)
        }
      })

      # Call block server module when node is added
      observeEvent(network_out$added_node(), {
        rv <- init_block(network_out$added_node(), rv, session)
      })

      # When a node is selected, we need to only show the
      # UI of the selected block
      manage_block_visibility(rv, network_out$selected_node)

      # Handle node removal
      observeEvent(network_out$removed_node(), {
        # cleanup rv
        to_remove <- network_out$removed_node()
        rv$blocks[[to_remove]] <- NULL

        # When we remove a connection, all observers are removed
        # This logic only works for non connected/isolated nodes.
        lapply(grep(to_remove, names(rv$obs), value = TRUE), \(nme) {
          rv$obs[[nme]]$destroy()
          rv$obs[[nme]] <- NULL
        })

        # Remove node from grid: need to find in which grid it is ...
        # Remove block UI
        removeUI(sprintf("#%s", ns(to_remove)))
        bslib::toggle_sidebar("properties", open = FALSE)
      })

      # Move items between properties tab and grid.
      # Since we can't rebuilt the UI of each block and preserve its state
      # we have to move elements fromm one place to another. This also avoids ID
      # duplication.
      # TO DO: find how to make this a module
      dashboard_blocks <- reactive({
        rv$blocks[
          which(chr_ply(rv$blocks, `[[`, "mode") == "dashboard")
        ]
      })
      manage_board_grid(rv, dashboard_blocks, session)

      # TO DO: make this a module
      # Render grid of block outputs in dashboard mode.
      # We rely on an rv cache to keep track of where a node was before switching from
      # a mode to another. This cache is a list contains bucket and body dataframes with block
      # id, (x, y) position and dimensions (h and w).
      # NOTE: it seems that Shiny some inputs won't work in a gridstack like selectize.js elements

      # The GridStack layout can be retrieved via the special shiny input ⁠input$<outputId>_layout⁠.
      # This allows us to know which block is where and restore the correct layout via a proxy (see
      # observer above).
      output$grid <- renderGridstack({
        gridstack(
          margin = "10px",
          cellHeight = "140px",
          resize_handles = "all",
          removable = TRUE,
          float = TRUE,
          options = list(
            acceptWidgets = TRUE
          )
        )
      })
      outputOptions(output, "grid", suspendWhenHidden = FALSE)

      # Format layout elements as dataframe for easier use
      grid_content <- reactive({
        process_grid_content(rv, input$grid_layout, dashboard_blocks)
      })

      # Update rv cache to real time change in the grid only
      # in dashboard mode.
      observeEvent(
        {
          grid_content()
        },
        {
          rv$grid <- grid_content()
        }
      )

      # Debug only
      output$grid_content <- renderPrint(grid_content())
    }
  )
}
