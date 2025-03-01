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
        backup_list = list.files(
          pattern = paste0("^", isolate(rv$board_id), ".*\\.json$")
        )
      )

      dot_args <- list(...)

      # Manual state saving. Use this to share the
      # app state with another group.
      output$serialize <- downloadHandler(
        board_filename(rv),
        write_board_to_disk(rv, dot_args$parent, session)
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

      # Debounce so that we don't record too
      # many intermediate states as json. This also leaves
      # enough time for the network to stabilize properly
      # and have the correct node coordinates.
      snapshot_trigger <- reactive({
        list(
          board_links(rv$board),
          dot_args$parent$grid,
          get_blocks_state(rv) # Capture any block state change (input change, ...)
        )
      }) |>
        debounce(2000)

      # Auto save
      observeEvent(
        {
          snapshot_trigger()
        },
        {
          snapshot_board(vals, rv, dot_args$parent, session)
        }
      )

      observeEvent(
        c(vals$current_backup, vals$backup_list),
        {
          toggle_undo_redo(vals)
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
          restore_board(
            vals$backup_list[[vals$current_backup]],
            res,
            dot_args$parent
          )
        },
        ignoreInit = TRUE
      )

      # Restore workspace from json file
      observeEvent(input$restore, {
        restore_board(input$restore$datapath, res, dot_args$parent)
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
          class = "btn-danger"
        )
      ),
      shinyjs::disabled(
        actionButton(
          NS(id, "redo"),
          label = "Redo",
          icon = icon("rotate-right")
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
