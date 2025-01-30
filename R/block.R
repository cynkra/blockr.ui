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
    div(
      class = "m-2",
      id = ns(id),
      card(
        full_screen = TRUE,
        card_title(sprintf("Node %s properties", id)),
        block_ui(ns(id), x),
        card_footer(
          "Custom footer"
        )
      )
    )
  }

  stopifnot(is.character(id) && length(id) == 1L)

  if (is.null(blocks)) {
    blocks <- sort(x)
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
    stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))
    for (block in blocks) {
      removeUI(
        sprintf("#%s-%s", id, block),
        immediate = TRUE
      )
    }
  }
}
