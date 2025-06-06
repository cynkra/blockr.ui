#' Serialization module
#'
#' Object (de)serialization in a board server context.
#'
#' @param id Namespace ID
#' @param board Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A \link[shiny]{reactiveVal} object that evaluates to `NULL` or a
#' `board` obejct.
#'
#' @rdname ser_deser
#' @export
ser_deser_server <- function(id, board, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      res <- reactiveVal()
      vals <- reactiveValues(
        auto_snapshot = FALSE,
        current_backup = NULL
      )

      dot_args <- list(...)

      # Manual state saving. Use this to share the
      # app state with another group.
      output$serialize <- downloadHandler(
        board_filename(board),
        write_board_to_disk(board, dot_args$parent, session)
      )

      # Init backup list
      observeEvent(TRUE, {
        dot_args$parent$backup_list <- list_snapshot_files(board$board_id)
      })

      # Trigger open scoutbar
      observeEvent(input$browse_snapshots, {
        dot_args$parent$open_scoutbar <- TRUE
      })

      # TBD -> add board option for auto_snapshot

      if (isTRUE(isolate(get_board_option_value("snapshot")$auto))) {
        # Debounce so that we don't record too
        # many intermediate states as json. This also leaves
        # enough time for the network to stabilize properly
        # and have the correct node coordinates.
        snapshot_trigger <- reactive({
          list(
            board_links(board$board),
            dot_args$parent$grid,
            get_blocks_state(board) # Capture any block state change (input change, ...)
          )
        }) |>
          debounce(2000)

        # Auto save
        observeEvent(
          {
            snapshot_trigger()
          },
          {
            snapshot_board(vals, board, dot_args$parent, session)
          }
        )

        observeEvent(
          c(vals$current_backup, dot_args$parent$backup_list),
          {
            toggle_undo_redo(vals, parent)
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
              dot_args$parent$backup_list[[vals$current_backup]],
              res,
              dot_args$parent
            )
          },
          ignoreInit = TRUE
        )
      }

      # Manual save
      observeEvent(input$save, {
        snapshot_board(vals, board, dot_args$parent, session)
      })

      # Probably useful to save something if the user disconnects
      # by accident or the app crashes ...
      session$onSessionEnded(function() {
        # we need isolate to avoid reactive context error.
        isolate({
          snapshot_board(vals, board, dot_args$parent, session)
        })
      })

      # Restore workspace from json file
      observeEvent(input$restore, {
        restore_board(input$restore$datapath, res, dot_args$parent)
      })

      # Restore from scoutbar choice
      observeEvent(
        {
          dot_args$parent$scoutbar
          req(dot_args$parent$scoutbar$action == "restore_board")
        },
        {
          restore_board(
            dot_args$parent$scoutbar$value,
            res,
            dot_args$parent
          )
        }
      )

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
      actionButton(
        NS(id, "save"),
        label = "Save",
        icon = icon("floppy-disk")
      ),
      actionButton(
        NS(id, "browse_snapshots"),
        label = "Restore from",
        icon = icon("file")
      ),
      if (isTRUE(board_option("snapshot", board)$auto)) {
        tagList(
          actionButton(
            NS(id, "undo"),
            label = "Undo",
            icon = icon("rotate-left")
          ),
          actionButton(
            NS(id, "redo"),
            label = "Redo",
            icon = icon("rotate-right")
          )
        )
      }
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
