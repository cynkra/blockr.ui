library(blockr.core)
library(blockr.dplyr)

test_blk <- new_dataset_block()
attr(test_blk, "uid") <- "test"

test_session <- shiny::MockShinySession$new()

mock_add_block <- function(blk, rv, parent, session) {
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
  parent$added_block <- blk
  session$flushReact()
}

mock_cleanup_state <- function(state) {
  board_blocks(state$board) <- blocks()
  state$blocks <- list()
  state$inputs <- list()
}

mocked_network_state <- list(
  nodes = list(),
  edges = list(),
  combos = list()
)

testServer(
  blockr.ui::add_rm_link_server,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_dash_board(
        blocks = c(
          a = new_dataset_block("BOD"),
          b = new_dataset_block("ChickWeight"),
          c = new_merge_block("Time")
        ),
        links = c(
          ac = new_link("a", "c", "x"),
          bc = new_link("b", "c", "y")
        ),
        stacks = list(ac = c("a", "c"))
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
      network = NULL,
      refreshed = NULL,
      # Blocks
      append_block = FALSE,
      added_block = NULL,
      removed_block = NULL,
      selected_block = NULL,
      # Edges
      cancelled_edge = NULL,
      added_edge = NULL,
      removed_edge = NULL,
      # stacks
      added_stack = NULL,
      stack_added_block = NULL,
      stack_removed_block = NULL,
      removed_stack = NULL
    )
  ),
  {
    # TBD
    session$setInputs("network-initialized" = TRUE)
    dot_args$parent$cold_start <- FALSE
    session$flushReact()
    browser()
  }
)

test_that("network ui works", {
  ui <- blockr.ui::add_rm_link_ui("mod", new_board())
  expect_s3_class(ui, "shiny.tag.list")
})
