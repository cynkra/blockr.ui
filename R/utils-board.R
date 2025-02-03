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
  div(
    class = "btn-toolbar justify-content-center align-items-center gap-5",
    role = "toolbar",
    `aria-label` = "Toolbar with button groups",
    shinyWidgets::switchInput(
      ns("mode"),
      onStatus = "default",
      onLabel = icon("network-wired"),
      offLabel = icon("table-columns"),
      value = TRUE,
      inline = TRUE,
      size = "normal"
    ),
    ...
  )
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

capture_for_serialize <- function(parent, rv) {
  # For serialisation
  observeEvent(parent$mode, {
    rv$board[["mode"]] <- parent$mode
  })
  observeEvent(rv$selected_block, {
    rv$board[["selected_node"]] <- rv$selected_block
  })
  observeEvent(parent$grid, {
    rv$board[["grid"]] <- parent$grid
  })

  observeEvent(parent$grid_restored, {
    rv$refreshed <- "grid"
  })
  return(NULL)
}

board_restore <- function(parent, rv) {
  board_refresh <- get("board_refresh", parent.frame(1))
  observeEvent(
    board_refresh(),
    {
      rv$refreshed <- "board"
    },
    ignoreInit = TRUE
  )
  return(NULL)
}
