#' Code generation module
#'
#' Generate reproducible code from a board.
#' @param id Module id.
#' @param board The initial `board` object
#' @rdname gen_code
#' @export
gen_code_ui <- function(id, board) {
  actionButton(
    NS(id, "code_mod"),
    "Code",
    icon = icon("code")
  )
}
