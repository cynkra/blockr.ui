library(blockr.core)
library(blockr.dplyr)

mock_add_block <- function(blk, rv, parent, session) {
  board_blocks(rv$board) <- c(board_blocks(rv$board), as_blocks(blk))
  attr(blk, "uid") <- tail(board_block_ids(rv$board), n = 1)
  rv$blocks[[attr(blk, "uid")]] <- list(
    block = blk,
    # Need server part for serialisation
    server = block_server(attr(blk, "uid"), blk)
  )
  create_node(blk, parent, rv, FALSE, session)
}

testServer(
  blockr.ui::ser_deser_server,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_board(
        class = "custom_board"
      ),
      board_id = "board" #,
      #inputs = list(),
      #links = list(),
      #msgs = reactiveVal(),
      #stacks = list()
    ),
    # dot_args
    parent = reactiveValues(
      grid = data.frame(),
      mode = "network",
      refreshed = NULL,
      nodes = data.frame(),
      selected_block = NULL,
      edges = data.frame()
    )
  ),
  {
    expect_null(vals$current_backup)

    # Add new block
    mock_add_block(
      new_dataset_block(dataset = "BOD"),
      board,
      dot_args$parent,
      session
    )
    dot_args$parent$selected_block <- board_block_ids(board$board)
    session$flushReact()
    # We now have 1 snapshot
    expect_identical(vals$current_backup, 1L)

    # Add another block
    mock_add_block(
      new_dataset_block(dataset = "CO2"),
      board,
      dot_args$parent,
      session
    )
    dot_args$parent$selected_block <- board_block_ids(board$board)[2]
    session$flushReact()
    Sys.sleep(1)
    session$elapse(2000) # for debounce
    # We should have 2 snaps
    expect_identical(vals$current_backup, 2L)
    expect_true(nrow(dot_args$parent$nodes) == 2)

    # Restore previous snapshot
    session$setInputs(undo = 0)
    expect_identical(vals$current_backup, 1)
    expect_true(nrow(dot_args$parent$nodes) == 1)
    expect_identical(
      dot_args$parent$selected_block,
      board_block_ids(board$board)[1]
    )

    # Restore latest
    session$setInputs(redo = 0)
    expect_identical(vals$current_backup, 2)
    expect_true(nrow(dot_args$parent$nodes) == 2)
    expect_identical(
      dot_args$parent$selected_block,
      board_block_ids(board$board)[2]
    )

    # Manual restore
    session$setInputs(
      restore = list(datapath = vals$backup_list[[1]])
    )
    expect_true(nrow(dot_args$parent$nodes) == 1)
    expect_identical(
      dot_args$parent$selected_block,
      board_block_ids(board$board)[1]
    )

    # Manual serialize
    output$serialize

    # cleanup
    if (length(vals$backup_list)) {
      lapply(vals$backup_list, file.remove)
    }
  }
)

test_that("ser_deser_ui works", {
  ui <- blockr.ui::ser_deser_ui("mod", new_board())
  expect_length(ui, 2)
  expect_named(ui, c("buttons", "restore"))
})
