#' Add/remove block stacks module
#'
#' Customizable logic for adding/removing stacks grouping blocks together on
#' the board.
#'
#' @param id Namespace ID.
#' @param board Reactive values object.
#' @param update Reactive value object to initiate board updates.
#' @param ... Extra arguments passed from parent scope.
#'
#' @return A reactive value that evaluates to `NULL` or a list with components
#' `add` and `rm`, where `add` is either `NULL` or a `stacks` object and `rm`
#' is either `NULL` or a character vector of link IDs.
#'
#' @rdname add_rm_g6_stack
#' @export
add_rm_stack_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      dot_args <- list(...)

      # Add new stack from node selection
      observeEvent(
        dot_args$parent$added_stack,
        {
          tmp_stack <- if (
            length(dot_args$parent$added_stack) == 1 &&
              nchar(dot_args$parent$added_stack) == 0
          ) {
            new_stack()
          } else {
            new_stack(blocks = dot_args$parent$added_stack)
          }
          update(
            list(
              stacks = list(
                add = stacks(tmp_stack)
              )
            )
          )
          dot_args$parent$added_stack <- NULL
        }
      )

      # Remove stack from node selection
      observeEvent(dot_args$parent$removed_stack, {
        update(
          list(
            stacks = list(
              rm = dot_args$parent$removed_stack
            )
          )
        )
        dot_args$parent$removed_stack <- NULL
      })

      # Callback from links module
      observeEvent(dot_args$parent$stack_removed_node, {
        # Update stacks callback
        stack_id <- dot_args$parent$stack_removed_node$stack_id
        node_id <- dot_args$parent$stack_removed_node$node_id
        tmp_stack <- board_stacks(board$board)[[stack_id]]
        stack_blocks(tmp_stack) <- stack_blocks(tmp_stack)[
          stack_blocks(tmp_stack) != node_id
        ]
        update(
          list(
            stacks = list(
              mod = as_stacks(setNames(
                list(tmp_stack),
                stack_id
              ))
            )
          )
        )
        dot_args$parent$stack_removed_node <- NULL
      })

      # Callback from links module
      observeEvent(dot_args$parent$stack_added_node, {
        stack_id <- dot_args$parent$stack_added_node$stack_id
        node_id <- dot_args$parent$stack_added_node$node_id
        tmp_stack <- board_stacks(board$board)[[stack_id]]
        stack_blocks(tmp_stack) <- c(stack_blocks(tmp_stack), node_id)
        update(
          list(
            stacks = list(
              mod = as_stacks(setNames(
                list(tmp_stack),
                stack_id
              ))
            )
          )
        )
        dot_args$parent$stack_added_node <- NULL
      })

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_g6_stack
#' @export
add_rm_stack_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "stacks_mod"),
      "Edit stacks",
      icon = icon("stack-overflow")
    )
  )
}
