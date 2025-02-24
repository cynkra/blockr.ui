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
    numericInput(
      ns("grid_zoom"),
      "Grid zoom level",
      min = 0.5,
      max = 1,
      value = 0.5,
      step = 0.1,
    ),
    div(
      id = ns("grid_zoom_target"),
      style = "zoom: 0.5;",
      input_switch(ns("lock"), "Lock grid?", FALSE),
      gridstackOutput(ns("grid"))
    )
  )
}

#' Dashboard grid server
#'
#' @param board Board object. Contains info about blocks,
#' selected block, refreshed status ...
#' @param mode App mode.
#' @param in_grid Callback received from  links plugin to
#' tell which block should be in grid.
#' @param blocks_ns In which namespace are the blocks
#' @rdname dashboard
#' @export
grid_server <- function(id, board, mode, in_grid, blocks_ns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals <- reactiveValues(
      grid = data.frame(),
      in_grid = list(),
      grid_restored = NULL
    )

    # Store observers if needed
    obs <- list()

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
        init_blocks_grid_state(board$blocks, vals)
      }
    )

    # Callback from links module: any change in the add to grid
    # options updates the local in_grid reactive value to move
    # the blocks between the sidebar and the grid.
    observeEvent(req(length(board$blocks) > 0), {
      register_links_grid_callbacks(
        names(board$blocks),
        in_grid,
        obs,
        vals
      )
    })

    # When we change block, update the switch to the value it should
    # be from vals$in_grid[[selected()]]
    observeEvent(
      {
        req(board$selected_block %in% names(board$blocks))
        c(board$selected_block, in_grid())
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
        req(length(vals$in_grid))
      },
      {
        manage_board_grid(mode, vals, blocks_ns, session)
      },
      ignoreInit = TRUE
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
      gridstack(
        margin = "10px",
        #cellHeight = "200px",
        resize_handles = "all",
        float = TRUE, # allow to drop elements anywhere
        options = list(
          acceptWidgets = TRUE #,
          # Avoids dead space and scrolling bars
          #sizeToContent = TRUE
        )
      )
    })
    outputOptions(output, "grid", suspendWhenHidden = FALSE)

    # Handle zoom on grid element
    observeEvent(input$grid_zoom, {
      handle_grid_zoom(session)
    })

    # Format layout elements as dataframe for easier use
    grid_content <- reactive({
      req(mode() == "dashboard")
      process_grid_content(vals, input$grid_layout)
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

    # Lock grid
    observeEvent(input$lock, {
      gridstackr::gs_proxy_enable_move(ns("grid"), !input$lock)
    })

    return(
      list(
        grid = reactive(vals$grid),
        in_grid = reactive(vals$in_grid),
        grid_restored = reactive(vals$grid_restored)
      )
    )
  })
}
