#' Whether blocks should be in grid or not
#'
#' Whenever a new block is created or some blocks
#' are removed, we update the mapping to know which
#' block should be in the grid, so that \link{manage_board_grid}
#' knows what to do.
#'
#' @param blocks Board blocks.
#' @param vals Local reactive values.
#' @keywords internal
init_blocks_grid_state <- function(blocks, vals) {
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

#' Update block grid state
#'
#' For a given selected block, toggle its grid presence
#' based on the specified value.
#'
#' @param selected Selected block id.
#' @param value New value.
#' @param vals Local reactive values.
#' @keywords internal
update_block_grid_state <- function(selected, value, vals) {
  if (vals$in_grid[[selected]] == value) return(NULL)
  vals$in_grid[[selected]] <- value
}

#' Update switch input
#'
#' For a given selected block, update the switch input
#' according to the block grid state (if values are different).
#'
#' @param selected Selected block id.
#' @param value New value.
#' @param vals Local reactive values.
#' @param session Shiny session object.
#' @keywords internal
update_block_grid_input <- function(selected, value, vals, session) {
  update_block_grid_state(selected, value, vals)
  update_switch(
    "add_to_grid",
    value = value
  )
}

#' Add block to grid
#'
#' @param id Block id
#' @rdname board-grid
add_block_to_grid <- function(id, vals, blocks_ns, session) {
  ns <- session$ns
  # Similar gs_proxy_add so that we can
  # move an element to the grid and call the JS method
  # with parameters we like.
  # Restore dimensions and position from
  # the grid state if this exist for the given block

  # Create default dims in case (new block)
  pars <- list(
    id = sprintf("%s-%s", blocks_ns, id),
    w = 4,
    h = 4
  )

  # If node was in the grid ...
  if (nrow(vals$grid) && any(grepl(id, vals$grid$id))) {
    pars <- as.list(
      vals$grid[vals$grid$id == sprintf("%s-%s", blocks_ns, id), ]
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

#' Remove block from grid
#'
#' @rdname board-grid
remove_block_from_grid <- function(id, blocks_ns, session) {
  ns <- session$ns
  # Move items back to properties panel
  session$sendCustomMessage(
    "move-widget-to-sidebar",
    list(
      id = sprintf("#%s_blocks", blocks_ns),
      block_id = sprintf("#%s-%s", blocks_ns, id)
    )
  )
}

#' Manage board grid
#' @param mode App mode.
#' @param vals Local reactive values.
#' @param blocks_ns Blocks namespace.
#' @param session Shiny session object.
#' @rdname board-grid
manage_board_grid <- function(mode, vals, blocks_ns, session) {
  ns <- session$ns

  current_grid_layout <- process_grid_content(vals, session$input$grid_layout)
  in_grid_ids <- current_grid_layout$id

  # Cleanup grid in editor mode
  if (mode == "network") {
    lapply(names(which(vals$in_grid == TRUE)), \(id) {
      remove_block_from_grid(id, blocks_ns, session)
    })
    gs_proxy_remove_all(ns("grid"))
    return(NULL)
  }

  lapply(names(vals$in_grid), \(nme) {
    new_state <- vals$in_grid[[nme]]

    # If there was no grid item and we the item added
    # is marked, we can insert it
    if (nrow(current_grid_layout) == 0) {
      if (new_state) {
        add_block_to_grid(nme, vals, blocks_ns, session)
      }
    } else {
      # Update if any of the in_grid state or layout has changed
      if (new_state) {
        # Need to clean the existing element to re-add it at a different place
        remove_block_from_grid(nme, blocks_ns, session)
        gs_proxy_remove_item(
          ns("grid"),
          id = grep(nme, in_grid_ids, value = TRUE)
        )
        add_block_to_grid(nme, vals, blocks_ns, session)
      }
      if (!new_state) {
        # Only remove
        remove_block_from_grid(nme, blocks_ns, session)
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
  if (is.null(grid_layout)) return(data.frame())
  if (
    !length(grid_layout$children) && length(which(vals$in_grid == TRUE)) > 0
  ) {
    return(vals$grid)
  }
  res <- do.call(rbind.data.frame, grid_layout$children)
  #if (nrow(res) == 0) return(data.frame())
  res[, !names(res) %in% c("content")]
}

#' Restore grid state from board state
#'
#' @param blocks Board block objects.
#' @param blocks_ns Blocks namespace.
#' @param vals Local reactive values.
#' @param parent Parent reactive values.
#' Contains blocks coordinates, dimensions, ...
#' @keywords internal
restore_grid <- function(blocks, blocks_ns, vals, parent) {
  vals$in_grid <- NULL
  vals$grid <- parent$grid
  ids <- names(blocks)

  # When the grid was empty, we still need to initialise the block state
  # and all values are false
  if (!nrow(vals$grid)) {
    lapply(ids, \(id) {
      vals$in_grid[[id]] <- FALSE
    })
    return(NULL)
  }

  # Otherwise we spread elements between the grid and the network
  in_grid_ids <- chr_ply(
    strsplit(vals$grid$id, paste0(blocks_ns, "-")),
    `[[`,
    2
  )
  not_in_grid <- which(!(ids %in% in_grid_ids))

  lapply(in_grid_ids, \(id) {
    vals$in_grid[[id]] <- TRUE
  })

  lapply(ids[not_in_grid], \(id) {
    vals$in_grid[[id]] <- FALSE
  })
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
