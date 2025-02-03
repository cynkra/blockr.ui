#' Serialization module
#'
#' Object (de)serialization in a board server context.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A [shiny::reactiveVal()] object that evaluates to `NULL` or a
#' `board` obejct.
#'
#' @rdname ser_deser
#' @export
ser_deser_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      res <- reactiveVal()
      vals <- reactiveValues(current_backup = NULL, backup_list = NULL)

      output$serialize <- downloadHandler(
        board_filename(rv),
        write_board_to_disk(rv)
      )

      # Auto save
      observeEvent(
        {
          req(length(rv$blocks) > 0)
          lapply(rv, \(el) {
            el
          })
        },
        {
          file_name <- board_filename(rv)()
          write_board_to_disk(rv)(file_name)
        }
      )

      observeEvent(input$restore, {
        res(
          from_json(input$restore$datapath)
        )
      })

      res
    }
  )
}

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
    ),
    div(
      class = "btn-group",
      role = "group",
      actionButton(
        NS(id, "undo"),
        label = "Undo",
        icon = icon("rotate-right"),
        class = "btn-light btn-sm"
      ),
      actionButton(
        NS(id, "redo"),
        label = "Redo",
        icon = icon("rotate-left"),
        class = "btn-light btn-sm"
      )
    )
  )
}

#' Ser/deser board
#'
#' @param x Board.
#' @param blocks Board blocks.
#' @param ... Generic consistency.
#' @export
#' @rdname blockr_ser
blockr_ser.custom_board <- function(x, blocks = NULL, ...) {
  list(
    object = class(x),
    blocks = blockr_ser(board_blocks(x), blocks),
    links = lapply(board_links(x), blockr_ser),
    nodes = blockr_ser(board_nodes(x)),
    selected_node = board_selected_node(x),
    grid = blockr_ser(board_grid(x)),
    mode = board_mode(x),
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

#' @rdname blockr_ser
#' @param data Data to restore.
#' @export
blockr_deser.custom_board <- function(x, data, ...) {
  new_board(
    blockr_deser(data[["blocks"]]),
    lapply(data[["links"]], blockr_deser),
    nodes = blockr_deser(data[["nodes"]]),
    selected_node = data[["selected_node"]],
    grid = blockr_deser(data[["grid"]]),
    mode = data[["mode"]],
    class = setdiff(class(x), "board")
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

board_mode <- function(x) {
  stopifnot(is_board(x))
  x[["mode"]]
}

board_selected_node <- function(x) {
  stopifnot(is_board(x))
  x[["selected_node"]]
}

board_filename <- function(rv) {
  function() {
    paste0(
      rv$board_id,
      "_",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      ".json"
    )
  }
}

write_board_to_disk <- function(rv) {
  function(con) {
    blocks <- lapply(
      lst_xtr(rv$blocks, "server", "state"),
      lapply,
      reval_if
    )

    json <- jsonlite::prettify(
      to_json(rv$board, blocks)
    )

    writeLines(json, con)
  }
}

check_ser_deser_val <- function(val) {
  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        stop("Expecting a `ser_deser` server to return a reactive value.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      if (!is_board(val())) {
        stop(
          "Expecting the `ser_deser` return value to evaluate to a ",
          "`board` object."
        )
      }

      validate_board(val())
    },
    once = TRUE
  )

  val
}
