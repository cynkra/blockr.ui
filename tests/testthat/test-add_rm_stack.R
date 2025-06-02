library(blockr.core)
library(blockr.dplyr)

mock_add_block <- function(blk, rv, parent, session) {
  board_blocks(rv$board) <- c(board_blocks(rv$board), as_blocks(blk))
  attr(blk, "uid") <- tail(board_block_ids(rv$board), n = 1)
  messages <- rv$msgs()
  messages[[attr(blk, "uid")]] <- NULL
  #list(
  #  state = list(error = NULL),
  #  data = list(error = NULL),
  #  eval = list(error = NULL)
  #)

  rv$msgs(messages)

  rv$blocks[[attr(blk, "uid")]]$block <- blk
  rv$inputs[[attr(blk, "uid")]] <- if (!length(block_inputs(blk))) {
    list()
  } else {
    setNames(
      list(reactiveVal()),
      block_inputs(blk)
    )
  }
  create_node(blk, parent, rv, TRUE, session)
  session$flushReact()
}


testServer(
  blockr.ui::add_rm_stack_server,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_board(
        class = "dash_board",
        options = new_board_options(
          dark_mode = "light",
          stacks_colors = hcl.colors(20, palette = "spectral")
        )
      ),
      board_id = "board",
      inputs = list(),
      links = list(),
      msgs = reactiveVal(),
      stacks = list()
    ),
    update = reactiveVal(),
    # dot_args
    parent = reactiveValues(
      nodes = data.frame(),
      edges = data.frame(),
      added_stack = NULL,
      stack_added_block = NULL,
      stack_removed_block = NULL,
      removed_stack = NULL
    )
  ),
  {
    # Add stack
    mock_add_block(new_dataset_block(), board, dot_args$parent, session)
    mock_add_block(new_dataset_block(), board, dot_args$parent, session)
    dot_args$parent$added_stack <- board_block_ids(board$board)
    session$flushReact()
    expect_s3_class(update()$stacks$add, "stacks")
    board_stacks(board$board) <- update()$stacks$add

    # TBD: remove and add node from/to stack

    # Remove stack
    dot_args$parent$removed_stack <- tail(board_stack_ids(board$board), n = 1)
    session$flushReact()
    expect_identical(
      update()$stacks$rm,
      tail(board_stack_ids(board$board), n = 1)
    )
    expect_null(dot_args$parent$removed_stack)
    board_stacks(board$board) <- stacks()
  }
)

test_that("add_rm_stack_ui works", {
  ui <- blockr.ui::add_rm_stack_ui("mod", new_board())
  expect_s3_class(ui, "shiny.tag.list")
})
