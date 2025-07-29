#' Code generation plugin module
#'
#' All code necessary for reproducing a data analysis as set up in blockr can
#' be made available to the user. Several ways of providing such a script or
#' code snippet are conceivable and currently implemented, we have a modal
#' with copy-to-clipboard functionality. This is readily extensible, for example
#' by offering a download button, by providing this functionality as a
#' `generate_code` module.
#'
#' @return A plugin container inheriting from `generate_code` is returned by
#' `generate_code()`, while the UI component (e.g. `generate_code_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `generate_code_server()`) is expected to return `NULL`.
#' @param id Namespace ID
#' @param board Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname generate_code
#' @export
generate_code_server <- function(id, board, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      dot_args <- list(...)

      board_code <- reactive(
        export_code(lst_xtr_reval(board$blocks, "server", "expr"), board$board)
      )

      output$code_out <- renderPrint(HTML(board_code()))

      observeEvent(
        req(dot_args$parent$display_code),
        {
          id <- "code_out"

          showModal(
            modalDialog(
              title = "Generated code",
              div(
                class = "text-decoration-none position-relative",
                if (nchar(board_code())) copy_to_clipboard(session, id),
                verbatimTextOutput(session$ns(id))
              ),
              easyClose = TRUE,
              footer = NULL,
              size = "l"
            )
          )
          dot_args$parent$display_code <- FALSE
        }
      )

      NULL
    }
  )
}

#' Code generation module
#'
#' Generate reproducible code from a board.
#' @param id Module id.
#' @param board The initial `board` object
#' @rdname gen_code
#' @export
generate_code_ui <- function(id, board) {
  NULL
}

#' @keywords internal
copy_to_clipboard <- function(session, id) {
  deps <- htmltools::htmlDependency(
    "copy-to-clipboard",
    utils::packageVersion("blockr.core"),
    src = pkg_file("assets", "js"),
    script = "copyToClipboard.js"
  )

  tagList(
    actionButton(
      session$ns("copy_code"),
      "",
      class = paste(
        "btn",
        "btn-outline-secondary",
        "btn-sm",
        "position-absolute",
        "top-0",
        "end-0",
        "m-2"
      ),
      icon = icon("copy", c("fa-solid", "fa-2x")),
      onclick = paste0("copyCode(\"", session$ns(id), "\");")
    ),
    deps
  )
}
