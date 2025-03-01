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

map <- function(f, ..., use_names = FALSE) Map(f, ..., USE.NAMES = use_names)

v_rule <- function() {
  shiny::tags$div(class = "vr")
}

lst_xtr <- function(x, ...) {
  for (i in c(...)) {
    x <- lapply(x, `[[`, i)
  }
  x
}

set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}

reval_if <- function(x) if (is.function(x)) x() else x

#' Useful for shinytest2
#' Pre-process reactiveValues results
#'
#' @keywords internal
process_app_state <- function(state) {
  stopifnot(is.list(state))
  stopifnot("nodes" %in% names(state))

  setNames(
    lapply(names(state), \(nme) {
      if (nme == "nodes") {
        # drop the x and y coords as this may not be reproducible
        if (nrow(state[[nme]]) > 0) {
          subset(state[[nme]], select = -c(x, y))
        } else {
          state[[nme]]
        }
      } else {
        state[[nme]]
      }
    }),
    names(state)
  )
}
