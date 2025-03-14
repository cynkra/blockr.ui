#' Block custom UI
#'
#' @param id Block module id.
#' @param x Block object.
#' @param blocks (Additional) blocks (or IDs) for which to generate the UI.
#' @param ... Generic consistency.
#'
#' @export
#' @rdname custom-board
block_ui.custom_board <- function(id, x, blocks = NULL, ...) {
  block_card <- function(x, id, ns) {
    id <- paste0("block_", id)

    blk_info <- get_block_registry(x)
    div(
      class = "m-2",
      id = ns(id),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between",
          card_title(
            icon(blk_icon(attr(blk_info, "category"))),
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
        expr_ui(ns(id), x),
        block_ui(ns(id), x),
        card_footer(
          "TBD"
        )
      )
    )
  }

  stopifnot(is.character(id) && length(id) == 1L)

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  } else if (is.character(blocks)) {
    blocks <- board_blocks(x)[blocks]
  }

  stopifnot(is_blocks(blocks))

  tagList(
    map(
      block_card,
      blocks,
      names(blocks),
      MoreArgs = list(ns = NS(id))
    )
  )
}

#' @rdname custom-board
#' @export
remove_block_ui.custom_board <- function(id, x, blocks = NULL, ...) {
  if (is.null(blocks)) {
    stopifnot(is.character(id) && length(id) == 1L)

    removeUI(
      paste0("#", id, "_blocks > div"),
      multiple = TRUE,
      immediate = TRUE
    )
  } else {
    stopifnot(is.character(blocks))
    for (block in blocks) {
      removeUI(
        sprintf("#%s-%s", id, paste0("block_", block)),
        immediate = TRUE
      )
    }
  }
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
