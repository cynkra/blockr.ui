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
      vals <- reactiveValues(
        auto_snapshot = FALSE,
        current_backup = NULL,
        backup_list = list.files(pattern = ".json$")
      )

      # Manual state saving. Use this to share the
      # app state with another group.
      output$serialize <- downloadHandler(
        board_filename(rv),
        write_board_to_disk(rv)
      )

      # Cleanup old files on start
      observeEvent(
        vals$backup_list,
        {
          if (length(vals$backup_list)) {
            lapply(vals$backup_list, file.remove)
          }
          vals$backup_list <- list()
        },
        once = TRUE
      )

      # Capture any block state change (input change, ...)
      blk_state <- reactive({
        req(length(board_blocks(rv$board)) > 0)
        lapply(rv$blocks, \(blk) {
          blk$server$result()
        })
      })

      # Debounce so that we don't record too
      # many json files per change. This also leaves
      # enough time for the network to stabilize properly
      # and have the correct node coordinates.
      snapshot_trigger <- reactive({
        list(
          board_links(rv$board),
          board_grid(rv$board),
          blk_state()
        )
      }) |>
        debounce(1000)

      # Auto save
      observeEvent(
        {
          snapshot_trigger()
        },
        {
          # Prevents undo/redo from triggering new snapshot
          # after the previous or next state are restored.
          # The vals$auto_snapshot is release so that any other
          # change can retrigger a new snapshot round
          if (vals$auto_snapshot) {
            vals$auto_snapshot <- FALSE
            return(NULL)
          }

          file_name <- board_filename(rv)()
          write_board_to_disk(rv)(file_name)
          vals$backup_list <- list.files(pattern = ".json$")
          vals$current_backup <- length(vals$backup_list)
        }
      )

      observeEvent(rv$blocks, {
        shinyjs::toggleState("snapshot", condition = length(rv$blocks) > 0)
      })

      observeEvent(
        c(vals$current_backup, vals$backup_list),
        {
          undo_cond <- if (!length(vals$backup_list)) {
            FALSE
          } else {
            vals$current_backup > 1
          }

          redo_cond <- if (!length(vals$backup_list)) {
            FALSE
          } else {
            vals$current_backup < length(vals$backup_list)
          }

          shinyjs::toggleState(
            "undo",
            cond = undo_cond
          )

          shinyjs::toggleState(
            "redo",
            cond = redo_cond
          )
        },
        ignoreNULL = TRUE
      )

      observeEvent(input$undo, {
        vals$current_backup <- vals$current_backup - 1
      })

      observeEvent(input$redo, {
        vals$current_backup <- vals$current_backup + 1
      })

      # Move from one snapshot to another
      observeEvent(
        c(input$undo, input$redo),
        {
          vals$auto_snapshot <- TRUE
          res(
            from_json(vals$backup_list[[vals$current_backup]])
          )
        },
        ignoreInit = TRUE
      )

      # Restore workspace from json file
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
  list(
    buttons = tagList(
      shinyjs::disabled(
        actionButton(
          NS(id, "undo"),
          label = "Undo",
          icon = icon("rotate-left"),
          class = "btn-danger btn-sm"
        )
      ),
      shinyjs::disabled(
        actionButton(
          NS(id, "redo"),
          label = "Redo",
          icon = icon("rotate-right"),
          class = "btn-sm"
        )
      )
    ),
    restore = tagList(
      downloadButton(
        NS(id, "serialize"),
        "Export",
        icon = icon("file-export"),
      ),
      fileInput(
        NS(id, "restore"),
        label = "",
        buttonLabel = "Import",
        placeholder = "Select file to restore"
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
  # null becomes NA ...
  data[["payload"]] <- lapply(data[["payload"]], \(el) {
    if (is.null(el)) el <- NA
    el
  })
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
