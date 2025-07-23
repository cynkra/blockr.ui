#' Ser/deser board
#'
#' @param x Board.
#' @param blocks Board blocks.
#' @param options Board options.
#' @param network visNetwork data.
#' @param selected Selected node.
#' @param modules Module state.
#' @param ... Generic consistency.
#' @export
#' @rdname blockr_ser
blockr_ser.dag_board <- function(
  x,
  blocks = NULL,
  options = NULL,
  network = NULL,
  selected = NULL,
  modules = NULL,
  ...
) {
  list(
    object = class(x),
    board = NextMethod(),
    network = network,
    selected_block = selected,
    modules = modules,
    version = as.character(utils::packageVersion(utils::packageName()))
  )
}

#' @rdname blockr_ser
#' @param data Data to restore.
#' @export
blockr_deser.dag_board <- function(x, data, ...) {
  list(
    board = NextMethod(data = data[["board"]]),
    # Other elements that are not part of the board
    # and need to be restored at the top level
    network = data[["network"]],
    selected_block = data[["selected_block"]],
    modules = data[["modules"]]
  )
}

#' Create board filename
#'
#' @param rv Internal reactiveValues for read-only usage.
#' @keywords internal
#' @rdname save-board
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

#' @keywords internal
#' @note Needed since g6R does not like auto_unbox = TRUE ...
to_json <- function(x, ...) {
  jsonlite::toJSON(blockr_ser(x, ...), null = "null")
}

#' Save board to disk
#'
#' @param rv Internal reactiveValues for read-only usage.
#' @param parent Parent reactiveValues to communicate to other modules.
#' @param session Shiny session object.
#' @keywords internal
#' @rdname save-board
write_board_to_disk <- function(rv, parent, session) {
  function(con) {
    blocks <- lapply(
      lst_xtr(rv$blocks, "server", "state"),
      lapply,
      reval_if
    )

    opts <- lapply(
      set_names(nm = list_board_options(rv$board)),
      board_option_from_userdata,
      session
    )

    json <- jsonlite::prettify(
      to_json(
        rv$board,
        blocks,
        opts,
        parent$network,
        parent$selected_block,
        lapply(parent$module_state, reval)
      )
    )

    writeLines(json, con)
  }
}

board_option_from_userdata <- function(name, session) {
  rv <- get0(name, envir = session$userData, inherits = FALSE)

  if (is.null(rv)) {
    return(NULL)
  }

  res <- rv()

  if (is.null(res)) {
    return(NULL)
  }

  if (identical(name, "page_size")) {
    res <- as.integer(res)
  }

  res
}

#' @keywords internal
list_snapshot_files <- function(board_id) {
  list.files(
    path = get_board_option_value("snapshot")$location,
    pattern = paste0("^", board_id, ".*\\.json$"),
    full.names = TRUE
  )
}

#' Capture board snapshot
#'
#' This is used to autosnapshot the board.
#'
#' @param vals Local reactiveValues.
#' @param rv Internal reactiveValues for read-only usage.
#' @param parent Parent reactiveValues to communicate to other modules.
#' @param session Shiny session object.
#' @keywords internal
snapshot_board <- function(vals, rv, parent, session) {
  tryCatch(
    {
      # Don't save board if no blocks are present.
      if (length(board_block_ids(rv$board)) == 0) {
        stop()
      }
      # Prevents undo/redo from triggering new snapshot
      # after the previous or next state are restored.
      # The vals$auto_snapshot is release so that any other
      # change can retrigger a new snapshot round
      if (vals$auto_snapshot) {
        vals$auto_snapshot <- FALSE
        return(NULL)
      }

      file_name <- board_filename(rv)()
      write_board_to_disk(rv, parent, session)(file_name)
      parent$backup_list <- list_snapshot_files(rv$board_id)
      vals$current_backup <- length(parent$backup_list)
    },
    error = function(e) {
      if (shiny::isRunning()) {
        showNotification(
          "Error saving board state.",
          tags$details(
            tags$summary("Details"),
            tags$small(e$message)
          ),
          duration = NA,
          type = "error"
        )
      }
    }
  )
}

#' Restore board from snapshot
#'
#' @param path JSON snapshot path.
#' @param res reactiveVal containing the module returned value.
#' @param parent Parent reactiveValues to communicate to other modules.
#' @keywords internal
restore_board <- function(path, res, parent) {
  tryCatch(
    {
      tmp_res <- from_json(path)
      res(tmp_res$board)
      # Update parent node, grid, selected, mode
      # that were stored in the JSON but not part of the board object.
      parent$network <- tmp_res$network
      parent$selected_block <- tmp_res$selected_block

      mods <- intersect(names(parent$module_state), names(tmp_res$modules))

      miss <- setdiff(names(parent$module_state), names(tmp_res$modules))
      xtra <- setdiff(names(tmp_res$modules), names(parent$module_state))

      if (length(miss)) {
        showNotification(
          paste0(
            "Attempting to restore a board with missing module settings for: ",
            paste_enum(miss), ". These will be reset."
          ),
          duration = NA,
          type = "warning"
        )
      }

      if (length(xtra)) {
        showNotification(
          paste0(
            "Attempting to restore a board with extra module settings for: ",
            paste_enum(xtra), ". These will be ignored."
          ),
          duration = NA,
          type = "warning"
        )
      }

      for (mod in mods) {
        parent$module_state[[mod]](tmp_res$modules[[mod]])
      }
    },
    error = function(e) {
      showNotification(
        paste0(
          "Error restoring snapshot. It is possible that you are trying to ",
          "restore an old state that is not compatible with the current ",
          "version."
        ),
        tags$details(
          tags$summary("Details"),
          tags$small(e$message)
        ),
        duration = NA,
        type = "error"
      )
    }
  )
}

#' Toggle undo/redo
#'
#' Toggle state of undo/redo buttons.
#'
#' @param vals Local module reactive Values.
#' @param parent Global reactive Values.
#' @keywords internal
toggle_undo_redo <- function(vals, parent) {
  undo_cond <- if (!length(parent$backup_list)) {
    FALSE
  } else {
    vals$current_backup > 1
  }

  redo_cond <- if (!length(parent$backup_list)) {
    FALSE
  } else {
    vals$current_backup < length(parent$backup_list)
  }

  shinyjs::toggleState(
    "undo",
    cond = undo_cond
  )

  shinyjs::toggleState(
    "redo",
    cond = redo_cond
  )
}
