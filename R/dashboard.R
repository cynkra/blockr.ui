#' @rdname board
#' @export
dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' @rdname board
#' @export
dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      list(
      )
    )
  })
}
