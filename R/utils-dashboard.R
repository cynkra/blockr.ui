#' Restore dashboard state from board state
#'
#' @param board Board object.
#' @param rv Board reactive values object. Read-only
#' @param parent Parent reactive values.
#' @param session Shiny session object.
#' Contains blocks coordinates, dimensions, ...
#' @export
#' @rdname restore-dashboard
restore_dashboard <- function(board, rv, parent, session) {
  UseMethod("restore_dashboard", board)
}

#' @keywords internal
generate_dashboard_blk_output <- function(id, rv, session) {
  output <- session$output
  out_name <- sprintf(
    "dock-%s-result",
    id
  )

  observeEvent(
    {
      req(id %in% board_block_ids(rv$board))
      rv$msgs()[[id]]
      rv$blocks[[id]]$server$result()
    },
    {
      output[[out_name]] <- block_output(
        rv$blocks[[id]]$block,
        {
          # Provide user feedback in the dashboard
          # to explain why an output is blank. shiny.emptystate
          # could also be a more polished alternative ...
          validate(
            need(
              rv$blocks[[id]]$server$result(),
              "Not data available. Please update the pipeline."
            )
          )
          rv$blocks[[id]]$server$result()
        },
        session
      )
    },
    ignoreNULL = FALSE
  )
}

#' @keywords internal
add_blk_panel_to_dashboard <- function(id, rv, session) {
  ns <- session$ns
  dock_blk_ui <- block_ui(
    ns(
      sprintf(
        "dock-%s",
        id
      )
    ),
    rv$blocks[[id]]$block
  )

  add_panel(
    "dock",
    sprintf("block_%s", id),
    panel = dockViewR::panel(
      id = sprintf("block-%s", id),
      title = sprintf("Block: %s", id),
      content = dock_blk_ui
    )
  )
}

#' @keywords internal
remove_blk_from_dashboard <- function(id, session) {
  output <- session$output
  out_name <- sprintf("dock-%s-result", id)
  remove_panel("dock", sprintf("block-%s", id))
  output[[out_name]] <- NULL
}

#' @export
#' @rdname restore-dashboard
restore_dashboard.dag_board <- function(board, rv, parent, session) {
  # cleanup old state
  if (length(parent$in_grid)) {
    lapply(names(parent$in_grid), \(id) {
      remove_blk_from_dashboard(id, session)
    })
  }
  parent$in_grid <- list()
  ids <- names(rv$blocks)

  if (!length(ids)) {
    parent$refreshed <- "grid"
    return(NULL)
  }

  in_grid_ids <- find_blocks_ids(rv$board, parent, session)

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
    # Regenerate the output for the block as well as dock panel
    generate_dashboard_blk_output(id, rv, session)
    add_blk_panel_to_dashboard(id, rv, session)
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
find_blocks_ids.dag_board <- function(
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
