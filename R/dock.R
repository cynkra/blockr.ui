#' Dashboard dock UI method
#'
#' @rdname dashboard
#' @export
dashboard_ui.dock_board <- function(id, board, ...) {
  ns <- NS(id)
  list(
    add_to_dashboard = bslib::input_switch(
      ns("add_to_dashboard"),
      "Use in dashboard?"
    ),
    options = tagList(
      numericInput(
        ns("dashboard_zoom"),
        "Dock zoom level",
        min = 0.5,
        max = 1.5,
        value = 1,
        step = 0.1,
      )
    ),
    content = div(
      id = ns("dashboard_zoom_target"),
      style = "zoom: 1;",
      dockViewOutput(ns("dock"), height = "100vh")
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
      # We don't even need to call restore_dock!
    }
  )

  # Whenever a new block is created, we initialise its grid state
  observeEvent(parent$added_block, {
    vals$in_grid[[block_uid(parent$added_block)]] <- FALSE
  })

  # Removed block(s) must not be referenced in the grid
  observeEvent(parent$removed_block, {
    lapply(parent$removed_block, \(removed) {
      # Signal to remove panel from dock.
      # Panel will be removed by manage_dashboard.
      vals$in_grid[[removed]] <- NULL
      if (paste0("block_", removed) %in% get_panels_ids("dock")) {
        remove_panel("dock", paste0("block_", removed))
      }
    })
  })

  # Toggle state for each selected block and update the state
  observeEvent(
    {
      req(length(board$blocks) > 0, parent$selected_block)
      input$add_to_dashboard
    },
    {
      update_dashboard_state(
        board$board,
        parent$selected_block,
        input$add_to_dashboard,
        vals
      )

      # Render a second output containing only
      # the block result on demand
      if (input$add_to_dashboard) {
        output[[sprintf(
          "dock-%s",
          parent$selected_block
        )]] <- block_output(
          board$blocks[[parent$selected_block]]$block,
          board$blocks[[parent$selected_block]]$server$result(),
          session
        )

        add_panel(
          "dock",
          sprintf("block_%s", parent$selected_block),
          panel = dockViewR::panel(
            id = sprintf("block-%s", parent$selected_block),
            title = sprintf("Block: %s", parent$selected_block),
            content = DT::dataTableOutput(
              session$ns(
                sprintf(
                  "dock-%s",
                  parent$selected_block
                )
              )
            )
          )
        )
      } else {
        if (
          sprintf("block_%s", parent$selected_block) %in% get_panels_ids("dock")
        ) {
          # Remove output from dock
          remove_panel("dock", sprintf("block_%s", parent$selected_block))
          output[[sprintf("dock-%s", parent$selected_block)]] <- NULL
        }
      }
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
      req(length(parent$selected_block) == 1)
      c(parent$selected_block, parent$in_grid)
    },
    {
      update_block_dashboard_input(
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
      req(parent$mode)
      req(length(vals$in_grid) > 0)
      parent$refreshed == "grid"
    },
    {
      manage_dashboard(board$board, parent$mode, vals, session)
    }
  )

  output$dock <- renderDockView({
    dock_view(
      panels = list(), # TBD handle when we initalise from a non empty dock
      # TBD: handle theme from global app theme
      theme = if (nchar(Sys.getenv("DOCK_THEME")) > 0) {
        Sys.getenv("DOCK_THEME")
      } else {
        "replit"
      }
    )
  })
  outputOptions(output, "dock", suspendWhenHidden = FALSE)

  # Handle zoom on grid element
  observeEvent(input$dashboard_zoom, {
    handle_dashboard_zoom(session)
  })

  # Update rv cache to real time change in the grid only
  # in dashboard mode.
  observeEvent(
    {
      input$dock_state
    },
    {
      vals$grid <- structure(input$dock_state, class = "dock")
    }
  )

  observeEvent(vals$in_grid, {
    parent$in_grid <- vals$in_grid
  })

  observeEvent(vals$grid, {
    parent$grid <- vals$grid
  })
}
