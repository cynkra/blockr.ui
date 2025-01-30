chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}

dropNulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}

`%OR%` <- function(x, y) {
  if (is.null(x)) y else x
}

block_uid <- function(x) {
  stopifnot(inherits(x, "block"))
  attr(x, "uid")
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
