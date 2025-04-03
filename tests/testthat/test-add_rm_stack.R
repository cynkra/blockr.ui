library(blockr.core)
library(blockr.dplyr)

mock_add_block <- function(blk, rv, parent, obs, session) {
  board_blocks(rv$board) <- c(board_blocks(rv$board), as_blocks(blk))
  attr(blk, "uid") <- tail(board_block_ids(rv$board), n = 1)
  rv$msgs(c(
    rv$msgs(),
    setNames(
      list(
        state = list(error = NULL),
        data = list(error = NULL),
        eval = list(error = NULL)
      ),
      attr(blk, "uid")
    )
  ))
  rv$blocks[[attr(blk, "uid")]]$block <- blk
  rv$inputs[[attr(blk, "uid")]] <- if (!length(block_inputs(blk))) {
    list()
  } else {
    setNames(
      list(reactiveVal()),
      block_inputs(blk)
    )
  }
  create_node(blk, parent, rv, TRUE, obs, session)
  session$flushReact()
}

mock_stack_nodes <- function(
  stack_id,
  color,
  update,
  rv,
  parent,
  session
) {
  board_stacks(rv$board) <- update()$stacks$add
  vals <- reactiveValues(
    stacks = list()
  )
  session$setInputs(selected_nodes = board_block_ids(rv$board))
  stack_nodes(stack_id, color, vals, rv, parent, session)
}

mock_cleanup_state <- function(state) {
  board_blocks(state$board) <- blocks()
  state$blocks <- list()
  state$inputs <- list()
}

testServer(
  blockr.ui::add_rm_stack_server,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_board(
        class = "custom_board",
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
    obs <- list()
    # Add stack
    mock_add_block(new_dataset_block(), board, dot_args$parent, obs, session)
    mock_add_block(new_dataset_block(), board, dot_args$parent, obs, session)
    dot_args$parent$added_stack <- board_block_ids(board$board)
    session$flushReact()
    expect_s3_class(update()$stacks$add, "stacks")
    board_stacks(board$board) <- update()$stacks$add
    mock_stack_nodes(
      board_stack_ids(board$board),
      color = "#A71B4B",
      update,
      board,
      dot_args$parent,
      session
    )

    # Remove node from stack
    remove_node_from_stack(
      board_block_ids(board$board)[1],
      dot_args$parent,
      standalone = TRUE,
      session
    )
    session$flushReact()
    expect_equal(
      stack_blocks(update()$stacks$mod[[1]]),
      board_block_ids(board$board)[2]
    )
    expect_null(dot_args$parent$stack_removed_node)
    board_stacks(board$board) <- update()$stacks$mod

    # Add block to stack
    mock_add_block(new_dataset_block(), board, dot_args$parent, obs, session)
    dot_args$parent$stack_added_node <- list(
      stack_id = board_stack_ids(board$board),
      node_id = tail(board_block_ids(board$board), n = 1)
    )
    session$flushReact()
    expect_equal(
      tail(stack_blocks(update()$stacks$mod[[1]]), n = 1),
      tail(board_block_ids(board$board), n = 1)
    )
    board_stacks(board$board) <- update()$stacks$mod

    # Remove stack
    dot_args$parent$removed_stack <- board_stack_ids(board$board)
    session$flushReact()
    expect_identical(update()$stacks$rm, dot_args$parent$removed_stack)
    board_stacks(board$board) <- stacks()
  }
)

test_that("add_rm_stack_ui works", {
  ui <- blockr.ui::add_rm_stack_ui("mod", new_board())
  expect_s3_class(ui, "shiny.tag.list")
})
