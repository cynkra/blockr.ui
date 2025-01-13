#' Dashboard UI
#'
#' @param id Module id.
#' @rdname dashboard
#' @export
dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' Dashboard server
#'
#' @rdname dashboard
#' @export
dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      list()
    )
  })
}
