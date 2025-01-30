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
    gridstackOutput(ns("grid")),
    verbatimTextOutput(ns("grid_content"))
  )
}

#' Dashboard grid server
#'
#' @param blocks List of blocks.
#' @param selected Selected block.
#' @param mode App mode.
#' @param blocks_ns In which namespace are the blocks
#' @rdname dashboard
#' @export
grid_server <- function(id, blocks, selected, mode, blocks_ns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals <- reactiveValues(
      grid = data.frame(),
      in_grid = list()
    )

    # Initialise a mapping list containing all blocks
    # ids and whether they are in the grid. Must update
    # whenever a new block is created
    observeEvent(blocks(), {
      # Add new block entries
      lapply(names(blocks()), \(nme) {
        if (is.null(vals$in_grid[[nme]])) {
          vals$in_grid[[nme]] <- FALSE
        }
      })

      # Remove block entries from being tracked
      to_remove <- which(!(names(vals$in_grid) %in% names(blocks())))
      lapply(to_remove, \(blk) {
        vals$in_grid[[blk]] <- NULL
      })
    })

    # When we change block, update the switch to the value it should
    # be from vals$in_grid[[selected()]]
    observeEvent(
      selected(),
      {
        if (vals$in_grid[[selected()]] == input$add_to_grid) return(NULL)
        freezeReactiveValue(input, "add_to_grid")
        update_switch("add_to_grid", value = vals$in_grid[[selected()]])
      }
    )

    # Toggle state for each selected block to update the state
    observeEvent(
      input$add_to_grid,
      {
        if (vals$in_grid[[selected()]] == input$add_to_grid) return(NULL)
        vals$in_grid[[selected()]] <- input$add_to_grid
      }
    )

    # Move items between properties tab and grid.
    # Since we can't rebuilt the UI of each block and preserve its state
    # we have to move elements from one place to another. This also avoids ID
    # duplication.
    manage_board_grid(mode, vals, blocks_ns, session)

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

    # Debug only
    output$grid_content <- renderPrint(grid_content())

    return(reactive(input$grid_layout))
  })
}
