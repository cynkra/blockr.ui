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
    blk_info <- get_block_registry(x)
    card(
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
      expr_ui(ns(NULL), x),
      block_ui(ns(NULL), x)
    )
  }

  ns <- NS(id)
  id <- names(block)
  stopifnot(is.character(id) && length(id) == 1L)
  block <- block[[1]]
  stopifnot(is_block(block))
  block_card(block, id, ns = ns)
}

#' @rdname block_ui
#' @export
insert_block_ui.dash_board <- function(id, x, ...) {
  stopifnot(
    is.character(id),
    length(id) == 1,
    is_board(x)
  )
  # Why the hell do I have to do that? If I don't
  # insert_block_ui triggers when there is no block yet ...
  if (length(board_blocks(x)) == 0) {
    return(NULL)
  }
  session <- getDefaultReactiveDomain()
  stopifnot(!is.null(session))
  ns <- session$ns

  # Don't re-add the same block panel if in the dock
  if (sprintf("block-%s", id) %in% get_panels_ids("layout")) {
    return(NULL)
  }

  blk_ui <- block_ui(
    ns(sprintf("block_%s", id)),
    x,
    board_blocks(x)[id]
  )

  # For some reasons, we need to add the panel first
  # then add the block UI to the panel.
  add_panel(
    "layout",
    panel = dockViewR::panel(
      id = sprintf("block-%s", id),
      title = sprintf("Block: %s", id),
      content = tagList()
    ),
    # TBD: should render as a separate group (not tabbed)
    position = list(
      referencePanel = "dag",
      direction = "right"
    )
  )

  insertUI(
    sprintf("#%s", session$ns(paste0("layout-", sprintf("block-%s", id)))),
    "beforeEnd",
    blk_ui,
    immediate = TRUE
  )

  invisible(x)
}

#' @rdname block_ui
#' @export
remove_block_ui.dash_board <- function(id, x, ...) {
  stopifnot(is.character(id), is_board(x))
  # Why do I have to do that too? This thing
  # even triggers when there is no block ...
  if (length(board_blocks(x)) == 0) {
    return(NULL)
  }
  lapply(id, \(blk) {
    remove_panel("layout", paste0("block-", blk))
  })

  invisible(x)
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
