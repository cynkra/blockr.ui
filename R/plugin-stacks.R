#' Add/remove block stacks module
#'
#' Customizable logic for adding/removing stacks grouping blocks together on
#' the board.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A reactive value that evaluates to `NULL` or a list with components
#' `add` and `rm`, where `add` is either `NULL` or a `stacks` object and `rm`
#' is either `NULL` or a character vector of link IDs.
#'
#' @rdname add_rm_stack
#' @export
add_rm_stack_server <- add_rm_stack_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      res <- reactiveVal(
        list(add = NULL, rm = NULL)
      )

      observeEvent(rv$new_stack, {
        res(
          list(
            add = stacks(new_stack(blocks = rv$new_stack))
          )
        )
      })

      res

      #res(
      #  list(
      #    add = if (length(upd$add)) upd$add else stacks(),
      #    rm = if (length(upd$rm)) upd$rm else character()
      #  )
      #)
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
