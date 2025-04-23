#' Init dashboard state generic
#'
#' Whenever a new block is created or some blocks
#' are removed, we update the mapping to know which
#' block should be in the dashboard, so that \link{manage_dashboard}
#' knows what to do.
#'
#' @param board Board object.
#' @param blocks Board blocks.
#' @param vals Local reactive values.
#' @rdname init-dashboard-state
#' @export
init_dashboard_state <- function(board, blocks, vals) {
  UseMethod("init_dashboard_state", board)
}

#' Init dock state method
#'
#' @rdname init-dashboard-state
#' @export
init_dashboard_state.dock_board <- function(board, blocks, vals) {
  lapply(names(blocks), \(nme) {
    vals$in_grid[[nme]] <- FALSE
  })
}

#' Init grid state method
#'
#' @rdname init-dashboard-state
#' @export
init_dashboard_state.grid_board <- function(board, blocks, vals) {
  # Add new block entries
  lapply(names(blocks), \(nme) {
    if (is.null(vals$in_grid[[nme]])) {
      vals$in_grid[[nme]] <- FALSE
    }
  })

  # Remove block entries from being tracked
  to_remove <- which(!(names(vals$in_grid) %in% names(blocks)))
  lapply(to_remove, \(blk) {
    vals$in_grid[[blk]] <- NULL
  })
}

#' Update block dashboard state generic
#'
#' @param board Board object.
#' @param selected Selected block id.
#' @param value New value.
#' @param vals Local reactive values.
#' @export
#' @rdname update-dashboard-state
update_dashboard_state <- function(board, selected, value, vals) {
  UseMethod("update_dashboard_state", board)
}

#' Update block dock state method
#'
#' For a given selected block, toggle its dock presence
#' based on the specified value.
#'
#' @export
#' @rdname update-dashboard-state
update_dashboard_state.dock_board <- function(board, selected, value, vals) {
  if (is.null(selected) || !length(vals$in_grid)) return(NULL)
  if (vals$in_grid[[selected]] == value) return(NULL)
  vals$in_grid[[selected]] <- value
}

#' Update block grid state method
#'
#' For a given selected block, toggle its grid presence
#' based on the specified value.
#'
#' @export
#' @rdname update-dashboard-state
update_dashboard_state.grid_board <- function(board, selected, value, vals) {
  if (is.null(selected)) return(NULL)
  if (!(selected %in% names(vals$in_grid))) {
    vals$in_grid[[selected]] <- value
  } else {
    if (vals$in_grid[[selected]] == value) return(NULL)
    vals$in_grid[[selected]] <- value
  }
}

#' Update switch input
#'
#' For a given selected block, update the switch input
#' according to the block grid state (if values are different).
#'
#' @param board Board object.
#' @param selected Selected block id.
#' @param value New value.
#' @param vals Local reactive values.
#' @param session Shiny session object.
#' @keywords internal
update_block_grid_input <- function(board, selected, value, vals, session) {
  update_dashboard_state(board, selected, value, vals)
  update_switch(
    "add_to_grid",
    value = value
  )
}

#' Add block to dashboard generic
#'
#' @rdname manage-dashboard
#' @export
add_dashboard_block <- function(board, id, vals, session) {
  UseMethod("add_dashboard_block", board)
}

#' Add block to dock method
#'
#' @rdname manage-dashboard
#' @export
add_dashboard_block.dock_board <- function(board, id, vals, session) {
  ns <- session$ns

  will_add_panel <- FALSE
  if (!length(get_panels_ids("dock"))) {
    will_add_panel <- TRUE
  } else {
    if (!any(grepl(id, get_panels_ids("dock")))) {
      will_add_panel <- TRUE
    }
  }

  # If panel was not here, we create it, if not we just move
  # from sidebar to dock
  if (will_add_panel) {
    add_panel(
      "dock",
      panel = dockViewR::panel(
        id = id,
        title = sprintf("Block %s", strsplit(id, "block_")[[1]][2]),
        content = tagList()
      )
    )
  }

  dock_id <- sprintf("#%s", ns("dock"))
  # Move sidebar block ui to panel
  session$sendCustomMessage(
    "move-block-in-dock",
    list(
      dock_id = dock_id,
      panel_id = id,
      block_id = sprintf("#%s", ns(id))
    )
  )
}

#' Add block to grid method
#'
#' @rdname manage-dashboard
#' @export
add_dashboard_block.grid_board <- function(board, id, vals, session) {
  ns <- session$ns
  # Similar gs_proxy_add so that we can
  # move an element to the grid and call the JS method
  # with parameters we like.
  # Restore dimensions and position from
  # the grid state if this exist for the given block

  # Create default dims in case (new block)
  pars <- list(
    id = ns(id),
    w = 4,
    h = 4
  )

  # If node was in the grid ...
  if (nrow(vals$grid) && any(grepl(id, vals$grid$id))) {
    pars <- as.list(
      vals$grid[vals$grid$id == ns(id), ]
    )
  }

  session$sendCustomMessage(
    "add-grid-widget",
    message = list(
      id = ns("grid"),
      data = pars
    )
  )
}

#' Remove block from dashboard generic
#'
#' @param id Block id.
#' @rdname manage-dashboard
#' @export
remove_dashboard_block <- function(board, id, session) {
  UseMethod("remove_dashboard_block", board)
}

#' Remove block from dock method
#'
#' @rdname manage-dashboard
#' @export
remove_dashboard_block.dock_board <- function(board, id, session) {
  ns <- session$ns
  dock_id <- sprintf("#%s", ns("dock"))

  session$sendCustomMessage(
    "remove-block-from-dock",
    list(
      id = sprintf("#%s_blocks", ns(NULL)),
      panel_id = id,
      dock_id = sprintf("%s", dock_id)
    )
  )
}

#' Remove block from grid method
#'
#' @rdname manage-dashboard
#' @export
remove_dashboard_block.grid_board <- function(board, id, session) {
  ns <- session$ns
  # Move items back to properties panel
  session$sendCustomMessage(
    "move-widget-to-sidebar",
    list(
      id = sprintf("#%s_blocks", ns(NULL)),
      block_id = sprintf("#%s", ns(id))
    )
  )
}

#' Manage dashboard generic
#' @param board Board.
#' @param mode App mode.
#' @param vals Local reactive values.
#' @param session Shiny session object.
#' @rdname manage-dashboard
#' @export
manage_dashboard <- function(board, mode, vals, session) {
  UseMethod("manage_dashboard", board)
}

#' Manage board dock method
#'
#' @rdname manage-dashboard
#' @export
manage_dashboard.dock_board <- function(board, mode, vals, session) {
  ns <- session$ns

  in_grid_ids <- get_panels_ids("dock")

  # Cleanup grid in editor mode
  if (mode == "network") {
    lapply(names(which(vals$in_grid == TRUE)), \(id) {
      if (length(in_grid_ids)) {
        remove_dashboard_block(board, paste0("block_", id), session)
      }
    })
    return(NULL)
  }

  lapply(names(vals$in_grid), \(nme) {
    new_state <- vals$in_grid[[nme]]
    nme <- paste0("block_", nme)
    if (new_state) {
      add_dashboard_block(board, nme, vals, session)
    }
    if (!new_state && length(in_grid_ids)) {
      if (nme %in% in_grid_ids) {
        remove_dashboard_block(board, nme, session)
        remove_panel("dock", nme)
      }
    }
  })
}

#' Manage board grid method
#'
#' @rdname manage-dashboard
#' @export
manage_dashboard.grid_board <- function(board, mode, vals, session) {
  ns <- session$ns

  current_grid_layout <- process_grid_content(vals, session$input$grid_layout)
  in_grid_ids <- current_grid_layout$id

  # Cleanup grid in editor mode
  if (mode == "network") {
    lapply(names(which(vals$in_grid == TRUE)), \(id) {
      if (nrow(current_grid_layout) > 0) {
        remove_dashboard_block(board, paste0("block_", id), session)
      }
    })
    gs_proxy_remove_all(ns("grid"))
    return(NULL)
  }

  lapply(names(vals$in_grid), \(nme) {
    new_state <- vals$in_grid[[nme]]
    nme <- paste0("block_", nme)

    # If there was no grid item and we the item added
    # is marked, we can insert it
    if (nrow(current_grid_layout) == 0) {
      if (new_state) {
        add_dashboard_block(board, nme, vals, session)
      }
    } else {
      # Update if any of the in_grid state or layout has changed
      if (new_state) {
        # Need to clean the existing element to re-add it at a different place
        remove_dashboard_block(board, nme, session)
        gs_proxy_remove_item(
          ns("grid"),
          id = grep(nme, in_grid_ids, value = TRUE)
        )
        add_dashboard_block(board, nme, vals, session)
      }
      if (!new_state) {
        # Only remove
        remove_dashboard_block(board, nme, session)
        gs_proxy_remove_item(
          ns("grid"),
          id = grep(nme, in_grid_ids, value = TRUE)
        )
      }
    }
  })
}

#' Process grid layout
#'
#' @param vals Local module reactive values.
#' @param grid_layout Returned by input$<GRID_ID>_layout.
#' Contains blocks coordinates, dimensions, ...
#' @keywords internal
process_grid_content <- function(vals, grid_layout) {
  if (
    is.null(grid_layout) ||
      (!length(grid_layout$children) && length(which(vals$in_grid == TRUE)) > 0)
  ) {
    return(vals$grid)
  }
  res <- do.call(rbind.data.frame, grid_layout$children)
  #if (nrow(res) == 0) return(data.frame())
  res[, !names(res) %in% c("content")]
}

#' Restore dashboard state from board state
#'
#' @param board Board object.
#' @param blocks Board block objects.
#' @param vals Local reactive values.
#' @param parent Parent reactive values.
#' @param session Shiny session object.
#' Contains blocks coordinates, dimensions, ...
#' @export
#' @rdname restore-dashboard
restore_dashboard <- function(board, blocks, vals, parent, session) {
  UseMethod("restore_dashboard", board)
}

#' @export
#' @rdname restore-dashboard
restore_dashboard.dash_board <- function(board, blocks, vals, parent, session) {
  vals$in_grid <- NULL
  vals$grid <- parent$grid
  ids <- names(blocks)

  in_grid_ids <- find_blocks_ids(board, parent, session)

  # When the grid was empty, we still need to initialise the block state
  # and all values are false
  if (!length(in_grid_ids)) {
    lapply(ids, \(id) {
      vals$in_grid[[id]] <- FALSE
    })
    return(NULL)
  }

  # Otherwise we spread elements between the grid and the network
  not_in_grid <- which(!(ids %in% in_grid_ids))

  lapply(in_grid_ids, \(id) {
    vals$in_grid[[id]] <- TRUE
  })

  lapply(ids[not_in_grid], \(id) {
    vals$in_grid[[id]] <- FALSE
  })
  parent$refreshed <- "grid"
}

#' Find blocks ids generic
#'
#' @rdname restore-dashboard
#' @export
find_blocks_ids <- function(
  board,
  parent,
  session
) {
  UseMethod("find_blocks_ids", board)
}

#' Find blocks ids dock method
#'
#' @rdname restore-dashboard
#' @export
find_blocks_ids.dock_board <- function(
  board,
  parent,
  session
) {
  if (!length(names(parent$grid$panels))) return(NULL)
  chr_ply(
    strsplit(names(parent$grid$panels), "block_"),
    `[[`,
    2
  )
}

#' Find blocks ids grid method
#'
#' @rdname restore-dashboard
#' @export
find_blocks_ids.grid_board <- function(
  board,
  parent,
  session
) {
  if (!length(parent$grid$id)) return(NULL)
  chr_ply(
    strsplit(parent$grid$id, session$ns("block_")),
    `[[`,
    2
  )
}

#' Update grid zoom on the client
#'
#' This allows to set different zoom level to handle more
#' crowded dashboards.
#'
#' @param session Shiny session object
#' @keywords internal
handle_grid_zoom <- function(session) {
  session$sendCustomMessage(
    "update-grid-zoom",
    list(
      id = sprintf("#%s", session$ns("grid_zoom_target")),
      zoom = session$input$grid_zoom
    )
  )
}
