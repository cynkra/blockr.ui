#' Data upload block constructor
#'
#' In order to make user data available to blockr, this block provides file-
#' upload functionality.
#'
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_readcsv_block <- function(sep = ",", quote = "\"", ...) {
  new_parser_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          sp <- reactiveVal(sep)
          qo <- reactiveVal(quote)

          # Update state
          observeEvent(input$sep, sp(input$sep))
          observeEvent(input$quote, qo(input$quote))

          list(
            expr = reactive({
              req(nchar(input$upload$datapath) > 0)
              cat(input$upload$datapath)
              bquote(
                utils::read.table(
                  file = .(file),
                  header = TRUE,
                  sep = .(sp),
                  quote = .(qo),
                  dec = ".",
                  fill = TRUE,
                  comment.char = ""
                ),
                list(
                  file = input$upload$datapath,
                  sp = sp(),
                  qo = qo()
                )
              )
            }),
            state = list(sep = sp, quote = qo)
          )
        }
      )
    },
    function(id) {
      tagList(
        fileInput(
          NS(id, "upload"),
          "Upload data"
        ),
        textInput(
          inputId = NS(id, "sep"),
          label = "Field separator",
          value = sep
        ),
        textInput(
          inputId = NS(id, "quote"),
          label = "Quoting characters",
          value = quote
        )
      )
    },
    class = "readcsv_block",
    ...
  )
}

#' XPT data upload block constructor
#'
#' In order to make user data available to blockr, this block provides xpt file-
#' upload functionality.
#'
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_readxpt_block <- function(col_select = NULL, skip = 0, n_max = Inf, ...) {
  new_parser_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          col_sel <- reactiveVal(col_select)
          skip_lines <- reactiveVal(skip)
          max_lines <- reactiveVal(n_max)

          # Update state
          observeEvent(input$col_select, col_sel(input$col_select))
          observeEvent(input$skip, skip_lines(input$skip))
          observeEvent(input$n_max, max_lines(input$n_max))

          list(
            expr = reactive({
              req(nchar(input$upload$datapath) > 0)
              bquote(
                haven::read_xpt(
                  file = .(file),
                  col_select = .(col_select),
                  skip = .(skip),
                  n_max = .(n_max)
                ),
                list(
                  file = input$upload$datapath,
                  col_select = col_sel(),
                  skip = skip_lines(),
                  n_max = max_lines(),
                )
              )
            }),
            state = list(
              col_select = col_sel,
              skip = skip_lines,
              n_max = max_lines
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        fileInput(
          NS(id, "upload"),
          "Upload data"
        ),
        textInput(
          inputId = NS(id, "col_select"),
          label = "Column selection",
          value = col_select
        ),
        numericInput(
          inputId = NS(id, "skip"),
          label = "Number of lines to skip",
          value = skip,
          min = 0,
          max = NA
        ),
        numericInput(
          inputId = NS(id, "n_max"),
          label = "Max number of lines to read",
          value = n_max,
          min = 0,
          max = NA
        )
      )
    },
    class = "readxpt_block",
    ...
  )
}
