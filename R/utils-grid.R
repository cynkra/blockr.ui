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
#'
#' @param mode App mode.
#' @param vals Local reactive values.
#' @param blocks_ns Blocks namespace.
#' @param session Shiny session object.
#' @rdname board-grid
manage_board_grid <- function(mode, vals, blocks_ns, session) {
  ns <- session$ns
  observeEvent(
    {
      mode()
      req(length(vals$in_grid), sum(unlist(vals$in_grid)) > 0)
    },
    {
      to_add <- which(vals$in_grid == TRUE)
      lapply(names(vals$in_grid)[to_add], \(nme) {
        if (mode() == "dashboard") {
          add_block_to_grid(nme, vals, blocks_ns, session)
        } else {
          remove_block_from_grid(nme, blocks_ns, session)
        }
      })

      # Cleanup grid in editor mode
      if (mode() == "network") {
        gs_proxy_remove_all(ns("grid"))
      }
    }
  )
}

#' Process grid layout
#'
#' @param mode App mode.
#' @param vals Local module reactive values.
#' @param grid_layout Returned by input$<GRID_ID>_layout.
#' Contains blocks coordinates, dimensions, ...
#' @keywords internal
process_grid_content <- function(mode, vals, grid_layout) {
  req(mode() == "dashboard")
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
