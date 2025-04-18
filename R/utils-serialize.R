#' Ser/deser board
#'
#' @param x Board.
#' @param blocks Board blocks.
#' @param network visNetwork data.
#' @param grid gridstack data.
#' @param selected Selected node.
#' @param mode App mode.
#' @param options Board options.
#' @param ... Generic consistency.
#' @export
#' @rdname blockr_ser
blockr_ser.dash_board <- function(
  x,
  blocks = NULL,
  network = NULL,
  grid = NULL,
  selected = NULL,
  mode = NULL,
  options = NULL,
  ...
) {
  list(
    object = class(x),
    blocks = blockr_ser(board_blocks(x), blocks),
    links = lapply(board_links(x), blockr_ser),
    stacks = lapply(board_stacks(x), blockr_ser),
    options = blockr_ser(board_options(x), options),
    nodes = blockr_ser(network),
    selected_block = selected,
    grid = blockr_ser(grid),
    mode = mode,
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
blockr_deser.dash_board <- function(x, data, ...) {
  list(
    board = new_board(
      blocks = blockr_deser(data[["blocks"]]),
      links = lapply(data[["links"]], blockr_deser),
      stacks = lapply(data[["stacks"]], blockr_deser),
      options = blockr_deser(data[["options"]]),
      class = setdiff(class(x), "board")
    ),
    # Other elements that are not part of the board
    # and need to be restored at the top level
    nodes = blockr_deser(data[["nodes"]]),
    selected_block = data[["selected_block"]],
    grid = blockr_deser(data[["grid"]]),
    mode = data[["mode"]]
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.data.frame <- function(x, data, ...) {
  # null becomes NA ...
  data[["payload"]] <- lapply(data[["payload"]], \(el) {
    if (is.null(el)) el <- NA
    el
  })
  as.data.frame(data[["payload"]])
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
        parent$nodes,
        parent$grid,
        parent$selected_block,
        parent$mode,
        opts
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
  vals$backup_list <- list.files(
    pattern = paste0("^", rv$board_id, ".*\\.json$")
  )
  vals$current_backup <- length(vals$backup_list)
}

#' Restore board from snapshot
#'
#' @param path JSON snapshot path.
#' @param res reactiveVal containing the module returned value.
#' @param parent Parent reactiveValues to communicate to other modules.
#' @keywords internal
restore_board <- function(path, res, parent) {
  tmp_res <- from_json(path)
  res(tmp_res$board)
  # Update parent node, grid, selected, mode
  # that were stored in the JSON but not part of the board object.
  parent$nodes <- tmp_res$nodes
  parent$grid <- tmp_res$grid
  parent$selected_block <- tmp_res$selected_block
  parent$mode <- tmp_res$mode
}

#' Toggle undo/redo
#'
#' Toggle state of undo/redo buttons.
#'
#' @param vals Local module reactive Values.
#' @keywords internal
toggle_undo_redo <- function(vals) {
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
}
