#' Dashboard dock UI method
#'
#' @rdname dashboard
#' @export
dashboard_ui.dock_board <- function(id, board, ...) {
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

  # Render a second output containing only
  # the block result on demand
  observeEvent(
    {
      req(parent$selected_block)
      board$blocks[[parent$selected_block]]$server$result()
    },
    {
      output[[sprintf(
        "dock-%s",
        parent$selected_block
      )]] <- block_output(
        board$blocks[[parent$selected_block]]$block,
        board$blocks[[parent$selected_block]]$server$result(),
        session
      )
    }
  )

  # Toggle state for each selected block and update the state
  observeEvent(
    {
      req(parent$selected_block)
      vals$in_grid[[parent$selected_block]]
    },
    {
      # Render a second output containing only
      # the block result on demand
      if (parent$in_grid[[parent$selected_block]]) {
        if (
          !(sprintf("block-%s", parent$selected_block) %in%
            get_panels_ids("dock"))
        ) {
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
        }
      } else {
        if (
          sprintf("block-%s", parent$selected_block) %in% get_panels_ids("dock")
        ) {
          # Remove output from dock
          remove_panel("dock", sprintf("block-%s", parent$selected_block))
          output[[sprintf("dock-%s", parent$selected_block)]] <- NULL
        }
      }
    },
    ignoreInit = TRUE
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

  # Update dock theme based on board options
  observeEvent(get_board_option_value("dark_mode"), {
    update_dock_view(
      "dock",
      list(theme = get_board_option_value("dark_mode"))
    )
  })

  # Handle zoom on grid element
  observeEvent(get_board_option_value("dashboard_zoom"), {
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

  # Callback from links plugin
  observeEvent(parent$in_grid, {
    vals$in_grid <- parent$in_grid
  })

  # Maintain consistency between parent and local reactive values
  observeEvent(vals$in_grid, {
    parent$in_grid <- vals$in_grid
  })

  observeEvent(vals$grid, {
    parent$grid <- vals$grid
  })
}
