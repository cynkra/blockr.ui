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

#' Restore dashboard state from board state
#'
#' @param board Board object.
#' @param blocks Board block objects.
#' @param parent Parent reactive values.
#' @param session Shiny session object.
#' Contains blocks coordinates, dimensions, ...
#' @export
#' @rdname restore-dashboard
restore_dashboard <- function(board, blocks, parent, session) {
  UseMethod("restore_dashboard", board)
}

#' @export
#' @rdname restore-dashboard
restore_dashboard.dash_board <- function(board, blocks, parent, session) {
  parent$in_grid <- NULL
  ids <- names(blocks)

  in_grid_ids <- find_blocks_ids(board, parent, session)

  # When the dock was empty, we still need to initialise the block state
  # and all values are false
  if (!length(in_grid_ids)) {
    lapply(ids, \(id) {
      parent$in_grid[[id]] <- FALSE
    })
    return(NULL)
  }

  # Otherwise we spread elements between the dock and the network
  not_in_grid <- which(!(ids %in% in_grid_ids))

  lapply(in_grid_ids, \(id) {
    parent$in_grid[[id]] <- TRUE
  })

  lapply(ids[not_in_grid], \(id) {
    parent$in_grid[[id]] <- FALSE
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
  if (!length(names(parent$grid$panels))) {
    return(NULL)
  }
  chr_ply(
    strsplit(names(parent$grid$panels), "block-"),
    `[[`,
    2
  )
}

#' Update dashboard zoom on the client
#'
#' This allows to set different zoom level to handle more
#' crowded dashboards.
#'
#' @param session Shiny session object
#' @keywords internal
handle_dashboard_zoom <- function(session) {
  ns <- session$ns
  session$sendCustomMessage(
    "update-dashboard-zoom",
    list(
      id = sprintf("#%s", ns("dashboard_zoom_target")),
      zoom = get_board_option_value("dashboard_zoom")
    )
  )
}
