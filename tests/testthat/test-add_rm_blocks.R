library(blockr.core)
library(blockr.dplyr)

testServer(
  blockr.ui::add_rm_block_server,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_board(
        class = "custom_board"
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
      preview = FALSE,
      append_block = FALSE,
      added_block = NULL,
      removed_block = NULL,
      selected_block = NULL,
      cancelled_edge = NULL
    )
  ),
  {
    expect_false(dot_args$parent$append_block)
    # Append block
    session$setInputs(append_block = 0)
    expect_true(dot_args$parent$append_block)
    # Close scoutbar without adding block: should reset append_block
    session$setInputs(`scoutbar-open` = FALSE)
    expect_false(dot_args$parent$append_block)
    # Add new block
    session$setInputs(scoutbar = "dataset_block")
    expect_s3_class(dot_args$parent$added_block, "dataset_block")
    expect_s3_class(update()$blocks$add, "blocks")
    # Remove block
    dot_args$parent$selected_block <- attr(dot_args$parent$added_block, "uid")
    session$setInputs(remove_block = 0)
    expect_identical(
      dot_args$parent$removed_block,
      dot_args$parent$selected_block
    )
    expect_identical(update()$blocks$rm, dot_args$parent$removed_block)
    expect_null(dot_args$parent$added_block)

    # Edge cancellation: expect to remove block
    session$setInputs(scoutbar = "dataset_block")
    dot_args$parent$cancelled_edge <- attr(dot_args$parent$added_block, "uid")
    session$flushReact()
    expect_identical(update()$block$rm, dot_args$parent$cancelled_edge)
  }
)
