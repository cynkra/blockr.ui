#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param id Namespace ID.
#' @param rv Reactive values object.
#' @param ... Extra arguments passed from parent scope
#'
#' @return A [shiny::reactiveValues()] object with components `add` and `rm`,
#' where `add` may be `NULL` or a `block` object and `rm` be `NULL` or a string
#' (block ID).
#'
#' @rdname add_rm_block
#' @export
add_rm_block_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      res <- reactiveValues(add = NULL, rm = NULL)
      parent <- list(...)

      # Hide add block in dashboard mode
      observeEvent(parent$mode, {
        shinyjs::toggle("add_block", condition = parent$mode == "network")
      })

      # Trigger add block
      observeEvent(
        input$add_block,
        {
          rv$added_block <- NULL
          # Reset add_block_to
          rv$append_block <- FALSE
          update_scoutbar(
            session,
            "scoutbar",
            revealScoutbar = TRUE
          )
        }
      )

      # TBD: implement add_block_to -> add a block after the selected one
      # We need a contextual registry and update the scoutbar with relevant
      # choices. I think we can use the same scoutbar as for the classic
      # add block with all choices.
      observeEvent(input$append_block, {
        rv$added_block <- NULL
        rv$append_block <- TRUE
        update_scoutbar(
          session,
          "scoutbar",
          revealScoutbar = TRUE
        )
      })

      # Adding a block, we update the rv$added so the graph is updated
      # in the links plugin
      observeEvent(input$scoutbar, {
        res$add <- as_blocks(create_block(input$scoutbar))
        # TODO: maybe a better way to get the block uid ...
        rv$added_block <- res$add[[1]]
        attr(rv$added_block, "uid") <- names(res$add)
      })

      observe(
        updateSelectInput(
          session,
          inputId = "block_select",
          choices = board_block_ids(rv$board)
        )
      )

      # Remove block and node:
      # - rv$removed_block is used to remove the node element.
      # - res$rm is used to remove the block module.
      observeEvent(
        input$remove_block,
        {
          res$rm <- rv$removed_block <- rv$selected_block
        }
      )

      # When a edge creation was cancelled
      observeEvent(rv$cancelled_edge, {
        res$rm <- rv$cancelled_edge
      })

      res
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_block
#' @export
add_rm_block_ui <- function(id, board) {
  list(
    toolbar = tagList(
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
      )
    ),
    sidebar = div(
      class = "btn-group",
      role = "group",
      actionButton(
        NS(id, "append_block"),
        "Append block",
        icon = icon("circle-plus"),
        class = "btn-light"
      ),
      actionButton(
        NS(id, "remove_block"),
        "Remove block",
        icon = icon("trash"),
        class = "btn-light"
      )
    )
  )
}
