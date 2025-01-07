#' @rdname board
#' @export
dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    gs_trash(id = ns("mytrash"), label = "Drag here to remove", height = "50px"),
    gridstackOutput(ns("body")),
    verbatimTextOutput(ns("body_content"))
  )
}

#' @rdname board
#' @export
dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- renderGridstack({
      gridstack(
        margin = "10px",
        cellHeight = "140px",
        resize_handles = "all",
        float = TRUE,
        options = list(
          acceptWidgets = TRUE
        ),
        trash_id = "mytrash"
      )
    })

    # The GridStack layout can be retrieved via the special shiny input ⁠input$<outputId>_layout⁠.
    # This might allow us to know which block is where and restore the correct layout.
    body_content <- reactive({
      if (is.null(input$body_layout)) return(data.frame())
      do.call(rbind.data.frame, input$body_layout$children)
    })

    # Debug only
    output$body_content <- renderPrint(body_content())

    # This piece of info is needed by the bucket UI to know which 
    # block is in the dashboard body (and avoid re-rendering them in the sidebar)
    return(
      list(
        layout = body_content
      )
    )
  })
}
