#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#'
#' @return A [shiny::reactiveValues()] object with components `add` and `rm`,
#' where `add` may be `NULL` or a `block` object and `rm` be `NULL` or a string
#' (block ID).
#'
#' @rdname add_rm_block
#' @export
app_add_rm_block_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      res <- reactiveValues(add = NULL, rm = NULL)

      observeEvent(
        input$add_block,
        {
          update_scoutbar(
            session,
            "scoutbar",
            revealScoutbar = TRUE
          )
        }
      )

      observeEvent(input$scoutbar, {
        res$add <- as_blocks(create_block(input$scoutbar))
      })

      observe(
        updateSelectInput(
          session,
          inputId = "block_select",
          choices = board_block_ids(rv$board)
        )
      )

      observeEvent(
        input$rm_block,
        {
          req(input$block_select)
          res$rm <- input$block_select
        }
      )

      res
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_block
#' @export
app_add_rm_block_ui <- function(id, board) {
  tagList(
    scoutbar(
      NS(id, "scoutbar"),
      placeholder = "Search for a block",
      actions = blk_choices()
    ),
    actionButton(
      NS(id, "add_block"),
      "New block",
      icon = icon("circle-plus"),
      class = "btn-light"
    ),
    selectInput(
      NS(id, "block_select"),
      "Select block from board",
      choices = c("", board_block_ids(board))
    ),
    actionButton(
      NS(id, "rm_block"),
      "Remove block",
      icon = icon("minus"),
      class = "btn-danger"
    )
  )
}
