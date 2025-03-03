#' Add/remove block stacks module
#'
#' Customizable logic for adding/removing stacks grouping blocks together on
#' the board.
#'
#' @param id Namespace ID.
#' @param rv Reactive values object.
#' @param update Reactive value object to initiate board updates.
#' @param ... Extra arguments passed from parent scope.
#'
#' @return A reactive value that evaluates to `NULL` or a list with components
#' `add` and `rm`, where `add` is either `NULL` or a `stacks` object and `rm`
#' is either `NULL` or a character vector of link IDs.
#'
#' @rdname add_rm_stack
#' @export
add_rm_stack_server <- add_rm_stack_server <- function(id, rv, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      dot_args <- list(...)
      observeEvent(dot_args$parent$new_stack, {
        update(
          list(
            stacks = list(
              add = stacks(new_stack(blocks = dot_args$parent$new_stack))
            )
          )
        )
      })

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_stack
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
