#' Create block icon
#'
#' Blocks are categorized. This function
#' returns the corresponding icon for each category.
#' It may be used in different places like in the nodes
#' or in the contextual menu (scoutbar) ...
#'
#' @param category Block category. See \link{available_blocks}.
#'
#' @keywords internal
blk_icon <- function(category) {
  switch(
    category,
    "data" = "table",
    "file" = "file-import",
    "parse" = "cogs",
    "plot" = "chart-line",
    "transform" = "wand-magic-sparkles"
  )
}

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
                description = attr(choice, "description"),
                icon = blk_icon(cat)
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
    class = "btn-toolbar",
    role = "toolbar",
    `aria-label` = "Toolbar with button groups",
    ...
  )
}

#' Manage board sidebars
#'
#' @param rv Reactive values.
#' @param session Shiny session object.
#' @keywords internal
#' @rdname handlers-utils
manage_sidebars <- function(rv, session) {
  ns <- session$ns

  # Hide the sidebar toggles to avoid accidental clicks by users
  # The switching is handles via below observeEvents
  session$sendCustomMessage("hide-sidebars-toggles", list(ns = ns(NULL)))

  # Toggle sidebars based on the board mode.
  # Since we render the same UI either in the properties sidebar
  # or the dashboard sidebar, they can't be opened at the same time.
  observeEvent(c(rv$mode, rv$selected_block), {
    cond <- if (is.null(rv$selected_block)) {
      FALSE
    } else {
      (rv$mode == "network" && nchar(rv$selected_block) > 0)
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
  observeEvent(rv$removed_block, {
    bslib::toggle_sidebar("properties", open = FALSE)
  })
}

#' Manage blocks visibility
#'
#' @keywords internal
#' @rdname handlers-utils
manage_block_visibility <- function(rv, update, parent, ...) {
  observeEvent(
    {
      req(parent$mode == "network")
      req(parent$selected_block)
    },
    {
      to_hide <- which(names(rv$blocks) != parent$selected_block)

      shinyjs::show(parent$selected_block)
      if (length(to_hide)) {
        lapply(names(rv$blocks)[to_hide], \(el) {
          shinyjs::hide(el)
        })
      }
    }
  )
  return(NULL)
}

board_restore <- function(rv, update, parent, ...) {
  board_refresh <- get("board_refresh", parent.frame(1))
  observeEvent(
    board_refresh(),
    {
      parent$refreshed <- "board"
    },
    ignoreInit = TRUE
  )
  return(NULL)
}

#' Custom board UI
#'
#' @param id Namespace ID.
#' @param x Board.
#' @param plugins UI for board plugins.
#' @param ... Generic consistency.
#' @rdname board_ui
#' @export
board_ui.custom_board <- function(id, x, plugins = list(), ...) {
  plugins <- as_plugins(plugins)

  toolbar_plugins <- c(
    "preserve_board",
    "manage_blocks",
    "manage_links",
    "manage_stacks",
    "generate_code"
  )

  toolbar_plugins <- plugins[intersect(toolbar_plugins, names(plugins))]
  toolbar_ui <- setNames(
    board_ui(id, toolbar_plugins, x),
    names(toolbar_plugins)
  )

  if ("edit_block" %in% names(plugins)) {
    block_plugin <- plugins[["edit_block"]]
  } else {
    block_plugin <- NULL
  }

  list(
    toolbar_ui = toolbar_ui,
    blocks_ui = div(
      id = paste0(id, "_board"),
      do.call(
        div,
        c(
          id = paste0(id, "_blocks"),
          block_ui(id, x, edit_ui = block_plugin)
        )
      )
    ),
    notifications = board_ui(id, plugins[["notify_user"]], x),
    board_options_ui = board_ui(id, board_options(x))
  )
}
