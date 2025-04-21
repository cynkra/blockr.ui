#' Dashboard dock UI method
#'
#' @rdname dashboard
#' @export
dashboard_ui.dock_board <- function(id, x, ...) {
  ns <- NS(id)
  list(
    add_to_grid = bslib::input_switch(
      ns("add_to_grid"),
      "Use in dashboard?"
    ),
    options = tagList(),
    content = div(
      id = ns("grid_zoom_target"),
      style = "zoom: 0.5;",
      dock_viewOutput(ns("dock"))
    )
  )
}

#' Dashboard dock server method
#'
#' @rdname dashboard
#' @export
dashboard_server.dock_board <- function(board, update, parent, ...) {
  session <- get("session", parent.frame(1))
  input <- session$input
  ns <- session$ns
  output <- session$output

  # Local reactiveValues
  vals <- reactiveValues(
    grid = structure(list(), class = "dock"),
    in_grid = list()
  )

  # Restore dock from serialisation only when network is restored
  observeEvent(
    {
      req(parent$refreshed == "network")
    },
    {
      restore_dashboard(board$board, board$blocks, vals, parent, session)
      parent$refreshed <- "grid"
    }
  )

  # Initialise a mapping list containing all blocks
  # ids and whether they are in the grid.
  observeEvent(
    {
      req(length(board$blocks) > 0)
    },
    {
      init_dashboard_state(board$board, board$blocks, vals)
    },
    once = TRUE
  )

  # Whenever a new block is created, we initialise its grid state
  observeEvent(parent$added_block, {
    vals$in_grid[[block_uid(parent$added_block)]] <- FALSE
  })

  # Removed block(s) must not be referenced in the grid
  observeEvent(parent$removed_block, {
    lapply(parent$removed_block, \(removed) {
      vals$in_grid[[removed]] <- NULL
      # remove panel from dock
      remove_panel(ns("dock"), removed)
    })
  })

  # Toggle state for each selected block and update the state
  observeEvent(
    {
      req(length(board$blocks) > 0, parent$selected_block)
      input$add_to_grid
    },
    {
      update_dashboard_state(
        board$board,
        parent$selected_block,
        input$add_to_grid,
        vals
      )
    }
  )

  # When we change block, update the switch to the value it should
  # be from vals$in_grid[[selected()]]. Also, callback from
  # links module: any change in the add to grid
  # options updates the local in_grid reactive value to move
  # the blocks between the sidebar and the grid.
  observeEvent(
    {
      req(length(board$blocks) > 0, length(parent$in_grid) > 0)
      req(parent$selected_block %in% names(parent$in_grid))
      c(parent$selected_block, parent$in_grid)
    },
    {
      update_block_grid_input(
        board$board,
        parent$selected_block,
        parent$in_grid[[parent$selected_block]],
        vals,
        session
      )
    }
  )

  # Move items between properties tab and grid.
  # Since we can't rebuilt the UI of each block and preserve its state
  # we have to move elements from one place to another. This also avoids ID
  # duplication.
  observeEvent(
    {
      parent$mode
      req(length(vals$in_grid))
      parent$refreshed == "grid"
    },
    {
      manage_dashboard(board$board, parent$mode, vals, session)
    }
  )

  output$dock <- renderDock_view({
    dock_view(
      panels = list(), # TBD handle when we initalise from a non empty dock
      theme = if (nchar(Sys.getenv("DOCK_THEME")) > 0)
        Sys.getenv("DOCK_THEME") else "replit"
    )
  })
  outputOptions(output, "dock", suspendWhenHidden = FALSE)

  # Update rv cache to real time change in the grid only
  # in dashboard mode.
  observeEvent(
    {
      input$dock_state
    },
    {
      vals$grid <- input$dock_state
    }
  )

  observeEvent(vals$in_grid, {
    parent$in_grid <- vals$in_grid
  })

  observeEvent(vals$grid, {
    parent$grid <- vals$grid
  })
}
