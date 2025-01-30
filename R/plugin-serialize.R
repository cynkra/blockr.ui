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

#' @export
blockr_ser.custom_board <- function(x, ...) {
  list(
    object = class(x),
    blocks = blockr_ser(board_blocks(x)),
    links = lapply(board_links(x), blockr_ser),
    #nodes = blockr_ser(board_nodes(x)),
    #grid = blockr_ser(board_grid(x)),
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
