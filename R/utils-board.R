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
grid_ui_wrapper <- function(ns, width = "75%", title = "Dashboard") {
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

#' Manage board sidebars
#'
#' @param rv Reactive values.
#' @param selected Selected block.
#' @param removed Any removed block.
#' @param session Shiny session object.
#' @keywords internal
#' @rdname handlers-utils
manage_sidebars <- function(rv, selected, removed, session) {
  ns <- session$ns

  # Hide the sidebar toggles to avoid accidental clicks by users
  # The switching is handles via below observeEvents
  session$sendCustomMessage("hide-sidebars-toggles", list(ns = ns(NULL)))

  # Toggle sidebars based on the board mode.
  # Since we render the same UI either in the properties sidebar
  # or the dashboard sidebar, they can't be opened at the same time.
  observeEvent(c(rv$mode, selected()), {
    cond <- if (is.null(selected())) {
      FALSE
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

  # Re-hide sidebar when block is removed
  observeEvent(removed(), {
    bslib::toggle_sidebar("properties", open = FALSE)
  })
}

#' Manage blocks visibility
#'
#' @keywords internal
#' @rdname handlers-utils
manage_block_visibility <- function(parent, rv) {
  observeEvent(
    {
      req(parent$mode == "network")
      rv$selected_block
    },
    {
      to_hide <- which(names(rv$blocks) != rv$selected_block)

      shinyjs::show(rv$selected_block)
      if (length(to_hide)) {
        lapply(names(rv$blocks)[to_hide], \(el) {
          shinyjs::hide(el)
        })
      }
    }
  )
  return(NULL)
}

#' Add block to grid
#'
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
