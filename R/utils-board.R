#' Create block icon
#'
#' Blocks are categorized. This function
#' returns the corresponding icon for each category.
#' It may be used in different places like in the nodes
#' or in the contextual menu (scoutbar) ...
#'
#' @param category Block category. See \link[blockr.core]{available_blocks}.
#'
#' @keywords internal
blk_icon <- function(category) {
  if (!length(category)) {
    res <- "alien"
  } else {
    res <- switch(
      category,
      "data" = "table",
      "file" = "download-simple",
      "parse" = "gear",
      "plot" = "chart-line",
      "transform" = "magic-wand",
      "table" = "table",
      "cake"
    )
  }

  # FIXME: We can't use fontawesome in the scoutbaR
  # due to compatibility issue with the g6R toolbar...
  phosphoricons::ph_i(res)
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
                id = sprintf("%s@add_block", attr(choice, "classes")[1]),
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
board_burger <- function(board_ui) {
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
    )
  )
}

#' Board header
#'
#' Header layout.
#'
#' @param id Board id.
#' @param board_ui Board ui.
#' @rdname board-layout
#' @keywords internal
board_header <- function(id, board_ui) {
  div(
    class = "d-flex align-items-center justify-content-around gap-5",
    board_burger(board_ui),
    board_actions(
      board_ui$toolbar_ui$generate_code,
      board_ui$toolbar_ui$preserve_board$buttons
    ),
    board_ui$board_options_ui
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
    notifications = board_ui(id, plugins[["notify_user"]], x),
    board_options_ui = board_ui(id, board_options(x))
  )

  tagList(
    scoutbar(
      sprintf("%s-scoutbar", id),
      placeholder = "What do you want to do?",
      showRecentSearch = TRUE
    ),
    board_header(id, my_board_ui),
    dockViewOutput(
      paste0(id, "-layout"),
      width = "100%",
      height = "100vh"
    )
  )
}

#' App layout
#'
#' @keywords internal
#' @rdname handlers-utils
build_layout <- function(board, update, parent, ...) {
  session <- get("session", parent.frame(1))
  input <- session$input
  output <- session$output
  ns <- session$ns

  # Insert block panel on add
  observeEvent(
    {
      parent$selected_block
    },
    {
      insert_block_panel(parent$selected_block, board$board)
    }
  )

  # Remove block panel on block remove
  # As we can remove multiple blocks at once, we
  # need to loop over the removed blocks.
  observeEvent(parent$removed_block, {
    remove_block_panels(parent$removed_block)
  })

  # TBD: why are outputs not shown?

  output$layout <- renderDockView({
    # Since board$board is reactive, we need to isolate it
    # so we don't re-render the whole layout each time ...
    isolate({
      dock_view(
        panels = list(
          panel(
            id = "dag",
            title = "Pipeline overview",
            content = board_ui(
              session$ns(NULL),
              dash_board_plugins("manage_links")
            )
          ),
          panel(
            id = "dashboard",
            title = "Dashboard",
            content = tagList(
              dashboard_ui(session$ns(NULL), board$board)
            ),
            position = list(
              referencePanel = "dag",
              direction = "right"
            )
          )
        ),
        # TBD (make theme function of board options)
        theme = "light"
      )
    })
  })
}

#' Scoutbar management callback
#'
#' @keywords internal
#' @rdname handlers-utils
manage_scoutbar <- function(board, update, parent, ...) {
  session <- get("session", parent.frame(1))
  input <- session$input
  ns <- session$ns

  # Trigger add block
  observeEvent(
    req(parent$open_scoutbar),
    {
      update_scoutbar(
        session,
        "scoutbar",
        revealScoutbar = TRUE
      )
    }
  )

  # Reset dot_args$parent$append_block is user
  # accidentally close the scoutbar without selecting
  # a block, so that the scoutbar can open again on the
  # next input$append_block or from the links plugin.
  observeEvent(
    input[["scoutbar-open"]],
    {
      if (!input[["scoutbar-open"]]) {
        parent$append_block <- FALSE
        parent$open_scoutbar <- FALSE
        parent$scoutbar <- list()
      }
    }
  )

  # Open the scoutbar when append block
  observeEvent(req(parent$append_block), {
    update_scoutbar(
      session,
      "scoutbar",
      revealScoutbar = TRUE
    )
  })

  # Update scoutbar action with snapshots taken in the serialise module
  observeEvent(
    {
      parent$backup_list
    },
    {
      # TBD: this isn't optimal. scoutbaR should
      # be able to allow to append/remove/modify actions
      # instead of having to re-create the whole list.
      new_actions <- list(
        scout_page(
          label = "Add a block",
          .list = blk_choices()
        ),
        scout_page(
          label = "Restore a snapshot",
          .list = lapply(
            list_snapshot_files(board$board_id),
            \(path) {
              infos <- file.info(path)
              scout_action(
                id = sprintf("%s@restore_board", path),
                label = strsplit(
                  path,
                  path.expand(get_board_option_value("snapshot")$location),
                  ""
                )[[1]][2],
                description = sprintf(
                  "Created by %s. Date: %s. Size: %s KB",
                  infos[["uname"]],
                  round(infos[["mtime"]], units = "secs"),
                  round(infos[["size"]] / 1000, 1)
                ),
                icon = phosphoricons::ph_i("file")
              )
            }
          )
        )
      )
      # We need to avoid to overwrite the existing actions ...
      update_scoutbar(
        session,
        "scoutbar",
        actions = new_actions
      )
    }
  )

  # Sync value for other modules
  observeEvent(input$scoutbar, {
    tmp <- strsplit(input$scoutbar, "@")[[1]]
    parent$scoutbar <- list(
      action = tmp[2],
      value = tmp[1]
    )
  })
}
