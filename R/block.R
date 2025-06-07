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
        expr_ui(ns(id), x),
        block_ui(ns(id), x)
      )
    )
  }

  id <- names(block)
  stopifnot(is.character(id) && length(id) == 1L)
  block <- block[[1]]
  stopifnot(is_block(block))
  block_card(block, id, ns = NS(id))
}

#' @rdname block_ui
#' @export
remove_block_ui.dash_board <- function(id, x, blocks = NULL, ...) {
  pars <- list(...)

  if (is.null(blocks)) {
    stopifnot(is.character(id) && length(id) == 1L)

    removeUI(
      paste0("#", id, "_blocks > div"),
      multiple = TRUE,
      immediate = TRUE,
      session = if (!is.null(pars$session)) {
        pars$session
      } else {
        getDefaultReactiveDomain()
      }
    )
  } else {
    stopifnot(is.character(blocks))
    for (block in blocks) {
      removeUI(
        sprintf("#%s-%s", id, paste0("block_", block)),
        immediate = TRUE,
        session = if (!is.null(pars$session)) {
          pars$session
        } else {
          getDefaultReactiveDomain()
        }
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
