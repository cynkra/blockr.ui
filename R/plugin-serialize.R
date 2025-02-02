#' Ser/deser module UI
#'
#' @param id module ID.
#' @param board The initial `board` object
#' @rdname ser_deser
#' @export
ser_deser_ui <- function(id, board) {
  tagList(
    downloadButton(
      NS(id, "serialize"),
      "Save",
      class = "btn-light"
    ),
    fileInput(
      NS(id, "restore"),
      "Restore"
    )
  )
}

#' Ser/deser board
#'
#' @param blocks Board blocks.
#' @param ... Generic consistency.
#' @export
#' @rdname blockr_ser
blockr_ser.custom_board <- function(x, blocks = NULL, ...) {
  list(
    object = class(x),
    blocks = blockr_ser(board_blocks(x), blocks),
    links = lapply(board_links(x), blockr_ser),
    nodes = blockr_ser(board_nodes(x)),
    selected_node = board_selected_node(x),
    grid = blockr_ser(board_grid(x)),
    mode = board_mode(x),
    version = as.character(utils::packageVersion(utils::packageName()))
  )
}

#' @export
blockr_ser.data.frame <- function(x, ...) {
  list(
    object = class(x),
    payload = as.list(x)
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.custom_board <- function(x, data, ...) {
  new_board(
    blockr_deser(data[["blocks"]]),
    lapply(data[["links"]], blockr_deser),
    nodes = blockr_deser(data[["nodes"]]),
    selected_node = data[["selected_node"]],
    grid = blockr_deser(data[["grid"]]),
    mode = data[["mode"]],
    class = setdiff(class(x), "board")
  )
}

#' @export
blockr_deser.data.frame <- function(x, data, ...) {
  as.data.frame(data[["payload"]])
}

board_grid <- function(x) {
  stopifnot(is_board(x))
  x[["grid"]]
}

board_nodes <- function(x) {
  stopifnot(is_board(x))
  x[["nodes"]]
}

board_mode <- function(x) {
  stopifnot(is_board(x))
  x[["mode"]]
}

board_selected_node <- function(x) {
  stopifnot(is_board(x))
  x[["selected_node"]]
}
