#' Create block choices for scoutbaR widget
#'
#' Utility to populate the scoutbar with block
#' registry information. Create one page per block category
#'
#' @export
blk_choices <- function() {
  blk_cats <- sort(
    unique(chr_ply(available_blocks(), \(b) attr(b, "category")))
  )

  lapply(blk_cats, \(cat) {
    scout_section(
      label = cat,
      .list = dropNulls(
        unname(
          lapply(available_blocks(), \(choice) {
            if (attr(choice, "category") == cat) {
              scout_action(
                id = attr(choice, "classes")[1],
                label = attr(choice, "name"),
                description = attr(choice, "description")
              )
            }
          })
        )
      )
    )
  })
}

#' Board blocks grid
#'
#' Default board grid.
#'
#' @param ns Namespace
#' @param width Sidebar width.
#' @param title Sidebar title
#' @export
#' @rdname main
grid_ui <- function(ns, width = "75%", title = "Dashboard") {
  sidebar(
    id = ns("dashboard"),
    title = title,
    position = "right",
    width = width,
    open = FALSE,
    gridstackOutput(ns("grid")),
    verbatimTextOutput(ns("grid_content"))
  )
}

#' Block properties sidebar
#'
#' Default block sidebar.
#'
#' @param ... Extra UI elements.
#' @rdname main
#' @export
properties_ui <- function(..., ns, width = "40%", title = "Block properties") {
  sidebar(
    id = ns("properties"),
    title = title,
    open = FALSE,
    width = width,
    position = "right",
    div(id = ns("block_container_ui")),
    ...
  )
}

#' Board action bar
#'
#' Default action bar.
#'
#' @param ... Extra UI elements.
#' @rdname main
#' @export
actions_ui <- function(..., ns) {
  tagList(
    shinyWidgets::switchInput(
      ns("mode"),
      onStatus = "default",
      onLabel = icon("network-wired"),
      offLabel = icon("table-columns"),
      value = TRUE,
      size = "mini"
    ),
    div(
      class = "d-flex justify-content-center align-items-center",
      div(
        class = "btn-group",
        role = "group",
        ...
      )
    )
  )
}

#' Init connections for a block
#'
#' @param blk Block object.
#' @export
#' @rdname connections
init_connection <- function(blk) {
  stats::setNames(
    lapply(block_inputs(blk), \(x) reactiveVal()),
    block_inputs(blk)
  )
}

#' Add connection between 2 blocks
#'
#' @param con Edge id. Character.
#' @param edges Edges dataframe.
#' @param rv Reactivevalues containing connections information.
#' @rdname connections
#' @export
#' @return A list with new observers and connections reactive values.
add_connection <- function(con, edges, rv) {
  # edge id is made as follows: <FROM_NODE_ID>_<TO_NODE_ID>
  ids <- strsplit(con, "_")[[1]]
  from_id <- ids[1]
  to_id <- ids[2]

  from_blk <- rv$blocks[[from_id]]
  to_blk <- rv$blocks[[to_id]]

  # Check receiver block input slots
  blk_inputs <- block_inputs(to_blk$block)
  if (!length(block_inputs(to_blk$block))) return(NULL)

  # Find connections
  con_label <- edges[edges$id == con, "label"]

  if (!length(con_label)) return(NULL)

  # Add connections
  # Inject result of connected downstream block if the connection
  # is not yet made. This needs an observer to listen to any change
  # in the upstream block result.
  obs_id <- sprintf("%s_%s_%s", from_id, to_id, con_label)
  rv$obs[[obs_id]] <- observeEvent(rv$blocks[[from_id]]$server$result(), {
    rv$connections[[to_id]][[con_label]](rv$blocks[[from_id]]$server$result())
  })

  rv
}

#' Remove connection between 2 blocks
#'
#' @rdname connections
#' @export
remove_connection <- function(con, rv) {
  ids <- strsplit(con, "_")[[1]]
  id_from <- ids[1]
  id_to <- ids[2]

  # Reset connections
  for (slot in names(rv$connections[[id_to]])) {
    rv$connections[[id_to]][[slot]](NULL)
  }

  # Destroy all update observers
  obs_to_destroy <- grep(con, names(rv$obs), value = TRUE)
  for (el in obs_to_destroy) {
    rv$obs[[el]]$destroy()
    rv$obs[[el]] <- NULL
  }

  rv
}

#' Init block UI elements
#'
#' Insert block UI in the DOM.
#'
#' @rdname init-block
init_block_ui <- function(blk, session) {
  ns <- session$ns
  # Block ui needs to come before server is initialized
  # so that the UI is updated
  insertUI(
    sprintf("#%s", ns("block_container_ui")),
    ui = div(
      class = "m-2",
      id = ns(block_uid(blk)),
      card(
        full_screen = TRUE,
        card_title(sprintf("Node %s properties", block_uid(blk))),
        block_ui(blk, ns(NULL)),
        card_footer(
          bslib::input_switch(
            ns(sprintf("%s-mode", block_uid(blk))),
            "Use in dashboard?"
          )
        )
      )
    ),
    # DO NOT REMOVE !!!!!!!
    immediate = TRUE
  )
}

#' Init block callbacks
#'
#' This initializes some observers needed
#' during the block lifecycle.
#'
#' @param input Shiny input object.
#'
#' @rdname init-block
init_block_callbacks <- function(blk, rv, input) {
  # The add to dashboard should not be visible in dashboard mode
  rv$obs[[sprintf("toggle-%s-mode-ui", block_uid(blk))]] <- observeEvent(
    rv$mode,
    {
      if (rv$mode == "network") {
        shinyjs::show(sprintf("%s-mode", block_uid(blk)))
      } else {
        shinyjs::hide(sprintf("%s-mode", block_uid(blk)))
      }
    }
  )

  # To be able to know if the block needs to
  # be rendered in the dashboard grid.
  # TO DO: fix a bug when adding multiple nodes
  # the button should be reset to FALSE when a node
  # isn't in the dashboard mode...
  rv$obs[[sprintf("switch-%s-mode", block_uid(blk))]] <- observeEvent(
    input[[sprintf("%s-mode", block_uid(blk))]],
    {
      if (input[[sprintf("%s-mode", block_uid(blk))]]) {
        rv$blocks[[block_uid(blk)]]$mode <- "dashboard"
      } else {
        rv$blocks[[block_uid(blk)]]$mode <- "editor"
      }
    }
  )

  rv$obs
}

#' Init block ui and server module
#'
#' Insert all the pieces so that the block works.
#'
#' @param blk Block object.
#' @param rv Reactivevalues containing connections information.
#' @param session Shiny session object.
#' @export
#' @rdname init-block
init_block <- function(blk, rv, session) {
  input <- session$input
  rv$connections[[block_uid(blk)]] <- init_connection(blk)

  init_block_ui(blk, session)

  rv$blocks[[block_uid(blk)]] <- list(
    # We need the block object to access some of its elements
    block = blk,
    # The server is the module from which we can
    # extract data, ...
    server = block_server(
      blk,
      data = rv$connections[[block_uid(blk)]]
    ),
    mode = "editor"
  )

  rv$obs <- init_block_callbacks(blk, rv, input)

  list(connections = rv$connections, blocks = rv$blocks, obs = rv$obs)
}

#' Manage board sidebars
#'
#' @param rv Reactive values.
#' @param selected Selected node.
#' @param session Shiny session object.
#' @keywords internal
#' @rdname handlers-utils
manage_sidebars <- function(rv, selected, session) {
  ns <- session$ns

  # Hide the sidebar toggles to avoid accidental clicks by users
  # The switching is handles via below observeEvents
  session$sendCustomMessage("hide-sidebars-toggles", list(ns = ns(NULL)))

  # Toggle sidebars based on the board mode.
  # Since we render the same UI either in the properties sidebar
  # or the dashboard sidebar, they can't be opened at the same time.
  observeEvent(c(rv$mode, selected()), {
    cond <- if (is.null(selected())) {
      rv$mode == "network"
    } else {
      (rv$mode == "network" && nchar(selected()) > 0)
    }

    toggle_sidebar(
      id = "properties",
      open = cond
    )
    toggle_sidebar(
      id = "dashboard",
      open = (rv$mode == "dashboard")
    )
  })
}

#' Manage blocks visibility
#'
#' @keywords internal
#' @rdname handlers-utils
manage_block_visibility <- function(rv, selected) {
  observeEvent(
    {
      req(rv$mode == "network")
      selected()
    },
    {
      browser()
      to_hide <- which(names(rv$blocks) != selected())

      shinyjs::show(selected())
      if (length(to_hide)) {
        lapply(names(rv$blocks)[to_hide], \(el) {
          shinyjs::hide(el)
        })
      }
    }
  )
}

#' Add block to grid
#'
#' @rdname board-grid
add_block_to_grid <- function(blk, rv, session) {
  ns <- session$ns
  # Similar gs_proxy_add so that we can
  # move an element to the grid and call the JS method
  # with parameters we like.
  # Restore dimensions and position from
  # the grid state if this exist for the given block

  # Create default dims in case (new block)
  pars <- list(
    id = ns(block_uid(blk)),
    w = 4,
    h = 4
  )

  # If node was in the grid ...
  if (nrow(rv$grid) && any(grepl(block_uid(blk), rv$grid$id))) {
    pars <- as.list(
      rv$grid[rv$grid$id == ns(block_uid(blk)), ]
    )
  }

  session$sendCustomMessage(
    "add-grid-widget",
    message = list(
      id = ns("grid"),
      data = pars
    )
  )
  # The block may be hidden so we need to show it
  shinyjs::show(block_uid(blk))
}

#' Remove block from grid
#'
#' @rdname board-grid
remove_block_from_grid <- function(blk, session) {
  ns <- session$ns
  # Move items back to properties panel
  session$sendCustomMessage(
    "move-widget-to-sidebar",
    list(
      id = sprintf("#%s", ns("block_container_ui")),
      block_id = sprintf("#%s", ns(block_uid(blk)))
    )
  )
}

#' Manage board grid
#'
#' @param blk Block object.
#' @param rv Reactive values.
#' @param dashboard_blocks List of blocks that should be in the grid.
#' @param session Shiny session object.
#' @rdname board-grid
manage_board_grid <- function(rv, dashboard_blocks, session) {
  observeEvent(
    {
      rv$mode
    },
    {
      if (!length(dashboard_blocks())) return(NULL)

      lapply(dashboard_blocks(), \(blk) {
        if (rv$mode == "dashboard") {
          add_block_to_grid(blk$block, rv, session)
        } else {
          remove_block_from_grid(blk$block, session)
        }
      })

      # Cleanup grid in editor mode
      if (rv$mode == "network") {
        gs_proxy_remove_all("grid")
      }
    }
  )
}

#' Process grid layout
#'
#' @param rv Reactive values
#' @param grid_layout Returned by input$<GRID_ID>_layout.
#' Contains blocks coordinates, dimensions, ...
#' @param dashboard_blocks Blocks that should be in the grid.
#' @keywords internal
process_grid_content <- function(rv, grid_layout, dashboard_blocks) {
  req(rv$mode == "dashboard")
  if (is.null(grid_layout)) return(data.frame())
  if (!length(grid_layout$children) && length(dashboard_blocks()) > 0) {
    return(rv$grid)
  }
  res <- do.call(rbind.data.frame, grid_layout$children)
  #if (nrow(res) == 0) return(data.frame())
  res[, !names(res) %in% c("content")]
}
