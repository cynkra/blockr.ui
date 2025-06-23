#' Block custom UI
#'
#' @param id Block module id.
#' @param x Board object.
#' @param block Block to generate the UI for.
#' @param ... Generic consistency.
#'
#' @export
#' @rdname block_ui
block_ui.dash_board <- function(id, x, block = NULL, ...) {
  block_card <- function(x, id, ns) {
    blk_id <- ns(paste0("block_", id))
    blk_info <- get_block_registry(x)
    card(
      id = ns(id),
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between",
        card_title(
          blk_icon(attr(blk_info, "category")),
          sprintf(
            "Block: %s (id: %s)",
            attr(blk_info, "name"),
            gsub("block_", "", id)
          )
        ),
        tooltip(
          icon("info-circle"),
          p(
            icon("lightbulb"),
            "How to use this block?",
          ),
          p(attr(blk_info, "description"), ".")
        )
      ),
      # subtitle
      div(
        class = "card-subtitle mb-2 text-body-secondary",
        sprintf(
          "Type: %s; Package: %s",
          attr(blk_info, "category"),
          attr(blk_info, "package")
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

#' @rdname block_ui
#' @export
insert_block_ui.dash_board <- function(
  id,
  x,
  blocks = NULL,
  create_block_ui = TRUE,
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

  blocks <- blocks[which(
    !(sprintf("block-%s", names(blocks)) %in%
      get_panels_ids("layout"))
  )]
  # Don't re-add the same block panel if in the dock
  if (length(blocks) == 0) {
    return(NULL)
  }

  # Loop over blocks.
  # This can happen when we restore a board with multiple blocks
  lapply(seq_along(blocks), \(i) {
    blk <- blocks[i]
    blk_ui <- block_ui(id, x, blk)

    # For some reasons, we need to add the panel first
    # then add the block UI to the panel.
    add_panel(
      "layout",
      panel = dockViewR::panel(
        id = sprintf("block-%s", names(blk)),
        title = sprintf("Block: %s", names(blk)),
        content = tagList(),
        position = list(
          referencePanel = if (length(get_panels_ids("layout")) == 2) {
            "dag"
          } else {
            get_panels_ids("layout")[length(get_panels_ids("layout"))]
          },
          direction = if (length(get_panels_ids("layout")) == 2) {
            "below"
          } else {
            "right"
          }
        ),
        removable = TRUE
      )
    )

    # We only create the block UI once if it is not already there. Hide and showing
    # it again is done outside of this function.
    if (create_block_ui) {
      insertUI(
        sprintf(
          "#%s",
          session$ns(paste0("layout-", sprintf("block-%s", names(blk))))
        ),
        ui = blk_ui,
        immediate = TRUE
      )
    }
  })

  invisible(x)
}

#' @rdname block_ui
#' @export
remove_block_ui.dash_board <- function(id, x, blocks = NULL, ...) {
  NULL
}

#' Get block info in registry
#'
#' @param x Block object
#' @keywords internal
get_block_registry <- function(x) {
  stopifnot(is_block(x))
  available_blocks()[[strsplit(attr(x, "ctor"), "new_")[[1]][2]]]
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
