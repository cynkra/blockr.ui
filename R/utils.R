chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}

dropNulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}

#' Create a Bootstrap Off-Canvas Element
#'
#' Creates an off-canvas element, which is a sliding panel that appears from the
#' edges of the viewport. This is a wrapper for Bootstrap's off-canvas component.
#'
#' @param id Character string. The ID of the off-canvas element. This ID is used to
#'   show/hide the element via JavaScript.
#' @param title Character string. The title to display in the header of the
#'   off-canvas element.
#' @param ... Additional UI elements to include in the body of the off-canvas
#'   element.
#' @param width Character string. Bootstrap width class to apply to the off-canvas
#'   element. Defaults to "w-100" (100% width). Common values include "w-75",
#'   "w-50", etc.
#' @param position Character string. The position from which the off-canvas element
#'   should appear. Must be one of "start" (left), "end" (right), "top", or
#'   "bottom". Defaults to "start".
#'
#' @return A HTML tag object representing the off-canvas element.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bslib)
#'
#'   ui <- page_fillable(
#'     actionButton(
#'      "toggle",
#'      "Toggle offcanvas",
#'      `data-bs-toggle` = "offcanvas",
#'      `data-bs-target` = "#demo",
#'      `aria-controls` = "demo"
#'     ),
#'     off_canvas(
#'       id = "demo",
#'       title = "Settings",
#'       position = "end",
#'       sliderInput("n", "Number", 1, 100, 50)
#'     )
#'   )
#'
#'   server <- function(input, output) {}
#'
#'   shinyApp(ui, server)
#' }
#'
#' @seealso \url{https://getbootstrap.com/docs/5.0/components/offcanvas/}
#' @seealso \url{https://getbootstrap.com/docs/5.0/utilities/sizing/}
#'
#' @export
off_canvas <- function(
  id,
  title,
  ...,
  width = "w-75",
  position = c("start", "top", "bottom", "end")
) {
  position <- match.arg(position)
  label <- rand_names()
  tags$div(
    class = sprintf("offcanvas offcanvas-%s %s", position, width),
    tabindex = "-1",
    id = id,
    `aria-labelledby` = label,
    `data-bs-scroll` = "true",
    tags$div(
      class = "offcanvas-header",
      tags$h5(class = "offcanvas-title", id = label, title),
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "offcanvas",
        `aria-label` = "Close"
      )
    ),
    tags$div(class = "offcanvas-body", ...)
  )
}

block_uid <- function(blk) {
  stopifnot(inherits(blk, "block"))
  attr(blk, "uid")
}
