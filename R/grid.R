#' Dashboard grid UI
#'
#' @param id Module id.
#' @rdname dashboard
#' @export
grid_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::input_switch(
      ns("add_to_grid"),
      "Use in dashboard?"
    ),
    gridstackOutput(ns("grid"))
  )
}

#' Dashboard grid server
#'
#' @param board Board object. Contains info about blocks,
#' selected block, refreshed status ...
#' @param mode App mode.
#' @param blocks_ns In which namespace are the blocks
#' @rdname dashboard
#' @export
grid_server <- function(id, board, mode, blocks_ns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals <- reactiveValues(
      grid = data.frame(),
      in_grid = list(),
      grid_restored = NULL
    )

    # Restore grid from serialisation only when network is restored
    observeEvent(
      {
        req(board$refreshed == "network")
      },
      {
        vals$grid_restored <- NULL
        restore_grid(board$board, blocks_ns, vals)
        vals$grid_restored <- "grid"
      }
    )

    # Initialise a mapping list containing all blocks
    # ids and whether they are in the grid. Must update
    # whenever a new block is created
    observeEvent(
      {
        req(length(board$blocks) > 0)
        board$blocks
      },
      {
        maintain_blocks_grid_state(board$blocks, vals)
      }
    )

    # When we change block, update the switch to the value it should
    # be from vals$in_grid[[selected()]]
    observeEvent(
      {
        req(board$selected_block)
        board$selected_block
      },
      {
        update_block_grid_input(
          board$selected_block,
          input$add_to_grid,
          vals,
          session
        )
      }
    )

    # Toggle state for each selected block to update the state
    observeEvent(
      input$add_to_grid,
      {
        update_block_grid_state(board$selected_block, input$add_to_grid, vals)
      },
      ignoreInit = TRUE
    )

    # Move items between properties tab and grid.
    # Since we can't rebuilt the UI of each block and preserve its state
    # we have to move elements from one place to another. This also avoids ID
    # duplication.
    observeEvent(
      {
        mode()
        req(length(vals$in_grid), sum(unlist(vals$in_grid)) > 0)
      },
      {
        manage_board_grid(mode, vals, blocks_ns, session)
      }
    )

    # Render grid of block outputs in dashboard mode.
    # We rely on an rv cache to keep track of where a node was before switching from
    # a mode to another. This cache is a list contains bucket and body dataframes with block
    # id, (x, y) position and dimensions (h and w).
    # NOTE: it seems that Shiny some inputs won't work in a gridstack like selectize.js elements

    # The GridStack layout can be retrieved via the special shiny input ⁠input$<outputId>_layout⁠.
    # This allows us to know which block is where and restore the correct layout via a proxy (see
    # observer above).
    output$grid <- renderGridstack({
      req(length(board$blocks) > 0)
      gridstack(
        margin = "10px",
        cellHeight = "200px",
        resize_handles = "all",
        float = TRUE,
        options = list(
          acceptWidgets = TRUE
        )
      )
    })
    outputOptions(output, "grid", suspendWhenHidden = FALSE)

    # Format layout elements as dataframe for easier use
    grid_content <- reactive({
      process_grid_content(mode, vals, input$grid_layout)
    })

    # Update rv cache to real time change in the grid only
    # in dashboard mode.
    observeEvent(
      {
        grid_content()
      },
      {
        vals$grid <- grid_content()
      }
    )

    return(
      list(
        grid = reactive(vals$grid),
        grid_restored = reactive(vals$grid_restored)
      )
    )
  })
}
