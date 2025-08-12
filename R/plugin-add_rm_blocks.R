#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param id Namespace ID.
#' @param board Reactive values object, containing board informations.
#' @param update Reactive value object to initiate board updates.
#' @param ... Extra arguments passed from parent scope. Useful to communicate
#' between plugins and surface information at the top level (for testing ...).
#'
#' @return NULL.
#'
#' @rdname add_rm_block
#' @export
add_rm_block_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      dot_args <- list(...)

      # Adding a block, we update the rv$added so the graph is updated
      # in the links plugin
      observeEvent(
        {
          req(dot_args$parent$scoutbar$action == "add_block")
          dot_args$parent$scoutbar$value
        },
        {
          # Allow to create block with custom parameters
          if (is.list(dot_args$parent$scoutbar$value)) {
            new_blk <- as_blocks(
              do.call(
                create_block,
                c(
                  list(id = dot_args$parent$scoutbar$value$name),
                  dot_args$parent$scoutbar$value$parms
                )
              )
            )
          } else {
            new_blk <- as_blocks(create_block(dot_args$parent$scoutbar$value))
          }

          update(
            list(blocks = list(add = new_blk))
          )
          dot_args$parent$added_block <- new_blk[[1]]
          attr(dot_args$parent$added_block, "uid") <- names(new_blk)
        }
      )

      # Remove block (triggered from the links module or from this module)
      observeEvent(dot_args$parent$removed_block, {
        update(
          list(blocks = list(rm = dot_args$parent$removed_block))
        )
      })

      # When a edge creation was cancelled
      observeEvent(dot_args$parent$cancelled_edge, {
        update(
          list(blocks = list(rm = dot_args$parent$cancelled_edge))
        )
      })

      # Reset added_block (no need to keep some old state)
      observeEvent(update()$blocks$rm, {
        dot_args$parent$added_block <- NULL
      })

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_block
#' @export
add_rm_block_ui <- function(id, board) {
  NULL
}
