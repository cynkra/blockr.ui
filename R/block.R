#' Block custom UI
#'
#' @param id Block module id.
#' @param x Board object.
#' @param block Block to generate the UI for.
#' @param ... Generic consistency.
#'
#' @export
#' @rdname block_ui
block_ui.dag_board <- function(id, x, block = NULL, ...) {
  block_card <- function(x, id, ns) {
    blk_id <- ns(paste0("block_", id))
    blk_info <- get_block_metadata(x)
    card(
      id = ns(id),
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between",
        card_title(
          blk_icon(blk_info$category),
          sprintf(
            "Block: %s (id: %s)",
            blk_info$name,
            gsub("block_", "", id)
          )
        ),
        tooltip(
          icon("info-circle"),
          p(
            icon("lightbulb"),
            "How to use this block?",
          ),
          p(blk_info$description, ".")
        )
      ),
      # subtitle
      div(
        class = "card-subtitle mb-2 text-body-secondary",
        sprintf(
          "Type: %s; Package: %s",
          blk_info$category,
          blk_info$package
        )
      ),
      expr_ui(blk_id, x),
      block_ui(blk_id, x)
    )
  }

  ns <- NS(id)
  id <- names(block)
  stopifnot(is.character(id) && length(id) == 1L)
  block <- block[[1]]
  stopifnot(is_block(block))
  block_card(block, id, ns = ns)
}

#' @keywords internal
remove_block_panels <- function(id) {
  stopifnot(is.character(id))
  lapply(id, \(blk) {
    remove_panel("layout", paste0("block-", blk))
  })
}

#' @param blocks Blocks to insert or remove.
#' @rdname block_ui
#' @export
insert_block_ui.dag_board <- function(
  id,
  x,
  blocks = NULL,
  ...
) {
  session <- getDefaultReactiveDomain()
  stopifnot(
    is.character(id),
    length(id) == 1,
    is_board(x),
    !is.null(session)
  )

  ns <- session$ns

  # Handle startup state when blocks is NULL then we look at the board blocks.
  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  }

  # Loop over blocks.
  # This can happen when we restore a board with multiple blocks.
  # Insert all the UI in the hidden offcanvas. Then we can show them
  # on demand ...
  lapply(seq_along(blocks), \(i) {
    blk <- blocks[i]
    blk_ui <- block_ui(id, x, blk)

    insertUI(
      sprintf(
        "#%s .offcanvas-body",
        ns("offcanvas")
      ),
      ui = blk_ui,
      immediate = TRUE
    )
  })

  invisible(x)
}

#' Show a block panel
#'
#' Move block from offcanvas-body to a panel.
#'
#' @param id Block id to show
#' @param parent Parent reactive values.
#' @param session Shiny session object.
#' @rdname block-panel
show_block_panel <- function(id, parent, session) {
  ns <- session$ns

  # Extract block panels
  block_panels <- chr_ply(
    grep("block", get_panels_ids("layout"), value = TRUE),
    \(pane) {
      strsplit(pane, "block-")[[1]][2]
    }
  )
  # Don't do anything if the block panel is already there
  if (parent$selected_block %in% block_panels) {
    return(NULL)
  }

  add_panel(
    "layout",
    panel = dockViewR::panel(
      id = sprintf("block-%s", id),
      title = sprintf("Block: %s", id),
      content = tagList(),
      # Don't remove if position is "within": by default,
      # only the visible tab is mounted in the DOM,
      # which means updating one block does not update
      # the linked block UIs and causes many issues.
      renderer = "always",
      position = list(
        referencePanel = if (length(get_panels_ids("layout")) == 2) {
          "dag"
        } else {
          get_panels_ids("layout")[length(get_panels_ids("layout"))]
        },
        direction = if (length(get_panels_ids("layout")) == 2) {
          "below"
        } else {
          "within"
        }
      ),
      remove = list(enable = TRUE, mode = "manual")
    )
  )

  # Move UI from offcanvas to the new panel
  session$sendCustomMessage(
    "show-block",
    list(
      block_id = sprintf("#%s", ns(id)),
      panel_id = sprintf("#%s", ns(paste0("layout-block-", id)))
    )
  )
}

#' Hide a block panel
#'
#' Move block from panel to offcanvas-body.
#'
#' @rdname block-panel
hide_block_panel <- function(id, session) {
  ns <- session$ns
  # Remove the block panel when the user clicks on the
  # close button of the panel.
  session$sendCustomMessage(
    "hide-block",
    list(
      offcanvas = sprintf("#%s", ns("offcanvas")),
      block_id = sprintf("#%s", ns(paste0("layout-", id)))
    )
  )
  remove_panel("layout", id)
}

#' @rdname block_ui
#' @export
remove_block_ui.dag_board <- function(id, x, blocks = NULL, ...) {
  NULL
}

#' Get block info in registry
#'
#' @param x Block object
#' @keywords internal
get_block_metadata <- function(x) {
  stopifnot(is_block(x))

  ctor <- attr(x, "ctor")

  if (is_string(ctor)) {
    blk <- sub("^new_", "", ctor)
    blks <- available_blocks()

    if (blk %in% names(blks)) {
      info <- blks[[blk]]

      res <- list(
        category = attr(info, "category"),
        name = attr(info, "name"),
        description = attr(info, "description"),
        package = attr(info, "package")
      )

      return(res)
    }
  }

  list(
    category = "Uncategorized",
    name = block_name(x),
    description = "No description available",
    package = "local"
  )
}

#' Get the state of a block
#'
#' @param rv Board reactiveValues for read-only usage.
#' @keywords internal
get_blocks_state <- function(rv) {
  stopifnot(is_board(rv$board))
  req(length(board_blocks(rv$board)) > 0)
  lapply(rv$blocks, \(blk) {
    blk$server$result()
  })
}
