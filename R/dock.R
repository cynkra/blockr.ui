#' Dashboard dock UI method
#'
#' @rdname dashboard
#' @export
dashboard_ui.dag_board <- function(id, board, ...) {
  ns <- NS(id)
  div(
    id = ns("dashboard_zoom_target"),
    style = "zoom: 1;",
    dockViewOutput(ns("dock"), height = "100vh")
  )
}

#' Dashboard dock server method
#'
#' @rdname dashboard
#' @export
dashboard_server.dag_board <- function(board, update, session, parent, ...) {
  isolate(
    {
      parent$in_grid <- list()
    }
  )

  input <- session$input
  ns <- session$ns
  output <- session$output

  res <- reactiveVal()

  observeEvent(input$dock_state, res(input$dock_state))

  # Restore dock from serialisation only when network is restored
  observeEvent(
    {
      req(parent$refreshed == "network")
    },
    {
      # Restore reactive values
      restore_dashboard(board$board, board, parent, session)
    }
  )

  # Whenever a new block is created, we initialise its grid state
  observeEvent(parent$added_block, {
    parent$in_grid[[block_uid(parent$added_block)]] <- FALSE
  })

  # Removed block(s) must not be referenced in the grid and
  # the panel must be removed from the dock.
  observeEvent(parent$removed_block, {
    lapply(parent$removed_block, \(removed) {
      # Signal to remove panel from dock.
      # Panel will be removed by manage_dashboard.
      parent$in_grid[[removed]] <- NULL
      if (paste0("block-", removed) %in% get_panels_ids("dock")) {
        remove_panel("dock", paste0("block-", removed))
      }
    })
  })

  # Initialise outputs for any existing block in the grid
  # TBD: this is not needed yet as we can't initialise a board
  # with existing blocks in the grid. Uncomment the code
  # below when we can do that.
  #lapply(
  #  isolate(parent$in_grid),
  #  \(id) {
  #    generate_dashboard_blk_output(
  #      id,
  #      board,
  #      session
  #    )
  #  }
  #)

  # Add panel to dashboard + handle secondary output
  observeEvent(
    {
      req(
        parent$added_to_dashboard,
        parent$in_grid[[parent$added_to_dashboard]]
      )
    },
    {
      add_blk_panel_to_dashboard(
        parent$added_to_dashboard,
        board,
        session
      )
      generate_dashboard_blk_output(
        parent$added_to_dashboard,
        board,
        session
      )
      parent$added_to_dashboard <- NULL
    }
  )

  # Toggle state for each selected block and update the state
  observeEvent(
    {
      req(parent$removed_from_dashboard)
      parent$in_grid[[parent$removed_from_dashboard]]
    },
    {
      # Remove output from dock
      remove_blk_from_dashboard(parent$removed_from_dashboard, session)
      parent$removed_from_dashboard <- NULL
    }
  )

  output$dock <- renderDockView({
    dock_view(
      panels = list(), # TBD handle when we initalise from a non empty dock
      # TBD: handle theme from global app options
      theme = "replit"
    )
  })

  # Handle zoom on grid element
  observeEvent(get_board_option_value("dashboard_zoom"), {
    handle_dashboard_zoom(session)
  })

  res
}
