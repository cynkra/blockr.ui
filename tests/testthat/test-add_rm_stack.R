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
      board = new_dag_board(),
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
    expect_length(dot_args$parent$added_stack, 2)
    session$flushReact()
    expect_s3_class(update()$stacks$add, "stacks")
    expect_null(dot_args$parent$added_stack)
    board_stacks(board$board) <- update()$stacks$add

    # Remove block from stack
    dot_args$parent$stack_removed_node <- list(
      stack_id = names(board_stacks(board$board)),
      node_id = board_block_ids(board$board)[1]
    )
    session$flushReact()
    expect_s3_class(update()$stacks$mod, "stacks")
    expect_identical(
      stack_blocks(update()$stacks$mod[[1]]),
      board_block_ids(board$board)[2]
    )
    board_stacks(board$board) <- update()$stacks$mod # manually update stack
    expect_null(dot_args$parent$stack_removed_node)

    ## Add block (again) to stack
    dot_args$parent$stack_added_node <- list(
      stack_id = names(board_stacks(board$board)),
      node_id = board_block_ids(board$board)[1]
    )
    session$flushReact()
    expect_s3_class(update()$stacks$mod, "stacks")
    expect_identical(
      stack_blocks(update()$stacks$mod[[1]]),
      rev(board_block_ids(board$board))
    )
    expect_null(dot_args$parent$stack_added_node)
    board_stacks(board$board) <- update()$stacks$mod # manually update stack

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
