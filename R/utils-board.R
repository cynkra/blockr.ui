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
    "transform" = "wand-magic-sparkles",
    "table" = "table"
  )
}

#' Create block choices for scoutbaR widget
#'
#' Utility to populate the scoutbar with block
#' registry information. Create one page per block category
#'
#' @keywords internal
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

#' Grid sidebar
#'
#' Default grid sidebar.
#'
#' @param ... Sidebar content.
#' @param id Sidebar id.
#' @param width Sidebar width.
#' @param title Sidebar title
#' @rdname board-sidebar
#' @keywords internal
board_grid <- function(..., id, width = "75%", title = "Dashboard") {
  sidebar(
    id = NS(id, "dashboard"),
    title = title,
    position = "right",
    width = width,
    open = FALSE,
    padding = c("0px", "10px"),
    ...
  )
}

#' Block properties sidebar
#'
#' Default block sidebar.
#'
#' @rdname board-sidebar
#' @keywords internal
board_properties <- function(
  ...,
  id,
  width = "40%",
  title = "Block properties"
) {
  sidebar(
    id = NS(id, "properties"),
    title = title,
    open = FALSE,
    width = width,
    position = "right",
    padding = c("0px", "10px"),
    ...
  )
}

#' Board action bar
#'
#' Default action bar.
#'
#' @param ... Extra UI elements.
#' @keywords internal
board_actions <- function(...) {
  div(
    class = "btn-toolbar",
    role = "toolbar",
    `aria-label` = "Toolbar with button groups",
    div(
      class = "btn-group btn-group-sm",
      role = "group",
      ...
    )
  )
}

#' Board extra actions
#'
#' Extra actions dropdown.
#'
#' @rdname board-layout
#' @keywords internal
board_burger <- function(board_ui, grid_ui) {
  dropdown_button(
    icon = icon("bars"),
    tags$li(
      tags$h6(
        class = "dropdown-header",
        "Save and Restore"
      )
    ),
    board_ui$toolbar_ui$preserve_board$restore,
    tags$li(
      tags$h6(
        class = "dropdown-header",
        "Grid options"
      )
    ),
    grid_ui$options
  )
}

#' Board body
#'
#' Body layout
#'
#' @rdname board-layout
#' @keywords internal
board_body <- function(id, board_ui, grid_ui) {
  layout_sidebar(
    border = FALSE,
    class = "p-0",
    sidebar = board_grid(
      id = id,
      # GRID CONTENT
      grid_ui$content
    ),
    layout_sidebar(
      border = FALSE,
      sidebar = board_properties(
        id = id,
        board_ui$blocks_ui,
        grid_ui$add_to_grid,
        board_ui$toolbar_ui$manage_blocks$sidebar
      ),
      board_ui$toolbar_ui$manage_links,
      # Notifications
      board_ui$notifications
    )
  )
}

#' Board header
#'
#' Header layout.
#'
#' @param id Board id.
#' @param board_ui Board ui.
#' @param grid_ui Grid ui.
#' @rdname board-layout
#' @keywords internal
board_header <- function(id, board_ui, grid_ui) {
  div(
    class = "d-flex align-items-center justify-content-around gap-5",
    board_burger(board_ui, grid_ui),
    board_actions(
      board_ui$toolbar_ui$manage_blocks$toolbar,
      board_ui$toolbar_ui$generate_code,
      board_ui$toolbar_ui$preserve_board$buttons,
      actionButton(
        NS(id, "preview"),
        "Preview",
        icon = icon("eye")
      ),
      actionButton(
        NS(id, "mode"),
        "Mode",
        icon = icon("network-wired")
      )
    ),
    board_ui$board_options_ui
  )
}

#' Manage board sidebars
#'
#' @param board Board reactiveValues. Read-only.
#' @param update Update reactiveVal to signal change to the board.
#' @param parent Parent global reactiveValues.
#' @param ... Extra elements.
#'
#' @keywords internal
#' @rdname handlers-utils
manage_sidebars <- function(board, update, parent, ...) {
  session <- get("session", parent.frame(1))
  ns <- session$ns

  # Hide the sidebar toggles to avoid accidental clicks by users
  # The switching is handles via below observeEvents
  session$sendCustomMessage("hide-sidebars-toggles", list(ns = ns(NULL)))

  # Toggle sidebars based on the board mode.
  # Since we render the same UI either in the properties sidebar
  # or the dashboard sidebar, they can't be opened at the same time.
  observeEvent(
    c(parent$mode, parent$selected_block),
    {
      cond <- if (is.null(parent$selected_block)) {
        FALSE
      } else {
        (parent$mode == "network" && nchar(parent$selected_block) > 0)
      }

      toggle_sidebar(
        id = "properties",
        open = cond
      )
      toggle_sidebar(
        id = "dashboard",
        open = (parent$mode == "dashboard")
      )
    },
    ignoreInit = TRUE
  )
}

#' Manage blocks visibility
#'
#' @keywords internal
#' @rdname handlers-utils
manage_block_visibility <- function(board, update, parent, ...) {
  observeEvent(
    {
      req(parent$mode == "network")
      req(parent$selected_block)
    },
    {
      to_hide <- which(names(board$blocks) != parent$selected_block)

      shinyjs::show(paste0("block_", parent$selected_block))
      if (length(to_hide)) {
        lapply(names(board$blocks)[to_hide], \(el) {
          shinyjs::hide(paste0("block_", el))
        })
      }
    }
  )
  return(NULL)
}

#' Manage app mode
#'
#' @keywords internal
#' @rdname handlers-utils
manage_app_mode <- function(board, update, parent, ...) {
  session <- get("session", parent.frame(1))
  input <- session$input

  # App mode
  observeEvent(input$mode, {
    if (input$mode %% 2 == 0) parent$mode <- "network" else
      parent$mode <- "dashboard"

    if (parent$mode == "network" && input$preview %% 2 != 0) {
      shinyjs::click("preview")
    }
    updateActionButton(
      session,
      "mode",
      icon = if (parent$mode == "network") icon("network-wired") else
        icon("table-columns")
    )
  })

  # Viewer mode: maximize dashboard view to save space
  observeEvent(input$preview, {
    toggle_preview(parent, session)
  })

  # Restore correct app mode
  observeEvent(req(parent$refreshed == "grid"), {
    if (parent$mode == "dashboard") {
      shinyjs::click("mode")
    }
    parent$refreshed <- NULL
  })

  # Disable mode or preview when there is no block
  observeEvent(board$blocks, {
    # close sidebar if no remaining block (prevents from getting
    # stuck in the dashboard.
    if (parent$mode == "dashboard" && length(board$blocks) == 0) {
      shinyjs::click("mode")
    }
    shinyjs::toggleState(
      "mode",
      condition = length(board$blocks) > 0
    )
    shinyjs::toggleState(
      "preview",
      condition = length(board$blocks) > 0
    )
  })
}

toggle_preview <- function(vals, session) {
  is_odd <- session$input$preview %% 2 == 0
  vals$preview <- if (is_odd) FALSE else TRUE
  if (vals$mode != "dashboard") shinyjs::click("mode")
  updateActionButton(
    session,
    "preview",
    icon = if (!is_odd) icon("eye-slash") else icon("eye")
  )
  session$sendCustomMessage(
    "toggle-view",
    list(
      id = sprintf("#%s", session$ns("dashboard")),
      val = !is_odd
    )
  )
}

#' Board restoration callback
#'
#' @keywords internal
#' @rdname handlers-utils
board_restore <- function(board, update, parent, ...) {
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
board_ui.dash_board <- function(id, x, plugins = list(), ...) {
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

  my_board_ui <- list(
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

  my_grid <- if (board_option("dashboard_type", x) == "grid") grid_ui(id) else
    dock_ui(id)

  tagList(
    board_header(id, my_board_ui, my_grid),
    board_body(id, my_board_ui, my_grid)
  )
}
