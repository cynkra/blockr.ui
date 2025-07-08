library(blockr.core)
library(blockr.dplyr)

test_blk <- new_dataset_block()
attr(test_blk, "uid") <- "test"

mock_add_block <- function(blk, rv, parent, session) {
  board_blocks(rv$board) <- c(
    board_blocks(rv$board),
    setNames(as_blocks(blk), attr(blk, "uid"))
  )
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

  # Also update board links
  board_links(rv$board) <- c(
    board_links(rv$board),
    parent$added_edge
  )

  parent$added_block <- blk
  session$flushReact()
}

mock_remove_block <- function(id, rv, parent, session) {
  # Remove link
  links <- board_links(rv$board)
  links <- links[!links$from %in% id & !links$to %in% id, ]
  board_links(rv$board) <- links

  stacks <- board_stacks(rv$board)
  # Remove from stacks
  board_stacks(rv$board) <- as_stacks(lapply(stacks, function(stack) {
    stack_blocks(stack) <- stack_blocks(stack)[
      stack_blocks(stack) != id
    ]
    stack
  }))

  # Remove block from board
  board_blocks(rv$board) <- board_blocks(rv$board)[
    -which(
      board_block_ids(rv$board) == id
    )
  ]
  rv$blocks[[id]] <- NULL
  rv$inputs[[id]] <- NULL
  #rv$msgs[[id]] <- NULL

  session$flushReact()
}

mock_add_stack <- function(stack_id, nodes, rv, session) {
  # Add stack
  board_stacks(rv$board) <- board_stacks(rv$board) |>
    c(
      setNames(
        stacks(new_stack(blocks = nodes)),
        stack_id
      )
    )

  # Update UI
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
  add_rm_link_server,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_dash_board(
        #blocks = c(
        #  a = new_dataset_block("BOD"),
        #  b = new_dataset_block("ChickWeight"),
        #  c = new_merge_block("Time")
        #),
        #links = c(
        #  ac = new_link("a", "c", "x"),
        #  bc = new_link("b", "c", "y")
        #),
        #stacks = list(ac = c("a", "c"))
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
      removed_stack = NULL,
      # scoutbar
      open_scoutbar = FALSE,
      scoutbar = list(
        trigger = NULL,
        action = NULL,
        value = NULL,
        is_open = FALSE
      ),
      # display_code
      display_code = FALSE,
      # Snapshots
      save_board = FALSE,
      # grid
      in_grid = list()
    )
  ),
  {
    # Init
    session$setInputs("network-initialized" = TRUE)
    session$flushReact()

    # Cold start
    dot_args$parent$cold_start <- FALSE
    session$flushReact()
    expect_identical(dot_args$parent$refreshed, "network")
    # TBD does this even do something?

    # TBD mock: g6_state to get input[["network-state"]]
    session$setInputs("network-state" = list())
    expect_s3_class(dot_args$parent$network, "network")

    # Check output
    output$network

    # Display code
    expect_false(dot_args$parent$display_code)
    session$setInputs("show_code" = TRUE)
    expect_true(dot_args$parent$display_code)

    # Tigger add block
    expect_null(dot_args$parent$scoutbar$trigger)
    expect_false(dot_args$parent$open_scoutbar)
    session$setInputs("add_block" = TRUE)
    expect_true(dot_args$parent$open_scoutbar)
    expect_identical(
      dot_args$parent$scoutbar$trigger,
      "links"
    )

    # Trigger save board
    expect_false(dot_args$parent$save_board)
    session$setInputs("save_board" = TRUE)
    expect_true(dot_args$parent$save_board)

    # Trigger browse snapshots
    session$setInputs("browse_snapshots" = TRUE)
    expect_true(dot_args$parent$open_scoutbar)
    expect_identical(
      dot_args$parent$scoutbar$trigger,
      "serialize"
    )

    dot_args$parent$scoutbar$is_open <- TRUE
    session$flushReact()

    # Added block
    dot_args$parent$append_block <- TRUE
    session$setInputs("network-selected_node" = "test")
    mock_add_block(
      test_blk,
      board,
      dot_args$parent,
      session
    )
    expect_null(update())
    expect_null(dot_args$parent$added_edge)
    expect_identical(dot_args$parent$cancelled_edge, "test")

    # Append block
    dot_args$parent$append_block <- FALSE
    session$setInputs("append_node" = TRUE)
    expect_true(dot_args$parent$append_block)
    expect_identical(dot_args$parent$scoutbar$trigger, "links")

    # Mock add other block (+ append)
    select_blk <- new_select_block()
    attr(select_blk, "uid") <- "select_test"
    mock_add_block(
      select_blk,
      board,
      dot_args$parent,
      session
    )
    expect_s3_class(dot_args$parent$added_edge, "links")
    link <- as.data.frame(dot_args$parent$added_edge[[1]])
    expect_identical(link$from, "test")
    expect_identical(link$to, "select_test")
    expect_identical(link$input, "data")
    expect_identical(update()$links$add, dot_args$parent$added_edge)
    expect_false(dot_args$parent$append_block)

    # TBD Add edge
    head_blk <- new_head_block(n = 2)
    attr(head_blk, "uid") <- "head_test"
    mock_add_block(
      head_blk,
      board,
      dot_args$parent,
      session
    )
    session$setInputs(
      "added_edge" = list(
        source = "select_test",
        target = "head_test",
        id = "dummy_edge_id"
      )
    )
    expect_identical(update()$links$add, dot_args$parent$added_edge)
    link <- as.data.frame(dot_args$parent$added_edge[[1]])
    expect_identical(link$from, "select_test")
    expect_identical(link$to, "head_test")
    expect_identical(link$input, "data")

    # Remove last added edge
    expect_null(update()$links$rm)
    session$setInputs("removed_edge" = dot_args$parent$added_edge$id)
    expect_identical(
      update()$links$rm,
      input$removed_edge
    )

    # Create stack
    expect_null(vals$stacks)
    session$setInputs("create_stack" = TRUE)
    session$setInputs(
      "new_stack" = 1,
      "new_stack_nodes" = board_block_ids(board$board),
      "stack_color" = "#DF5A21"
    )
    expect_identical(dot_args$parent$added_stack, input$new_stack_nodes)
    mock_add_stack(
      "new_stack",
      dot_args$parent$added_stack,
      board,
      session
    )
    expect_identical(board_stack_ids(board$board), "new_stack")
    expect_identical(vals$stacks, "combo-new_stack")

    # Remove stack
    expect_null(dot_args$parent$removed_stack)
    session$setInputs("remove_stack" = "combo-new_stack")
    expect_identical(dot_args$parent$removed_stack, "new_stack")
    expect_length(vals$stacks, 0)

    # TBD: how to do stack_added_block/stack_removed_block?

    # Add/Remove to/from dashboard
    expect_null(dot_args$parent$in_grid[[dot_args$parent$selected_block]])
    session$setInputs("add_to_dashboard" = TRUE)
    expect_true(dot_args$parent$in_grid[[dot_args$parent$selected_block]])
    session$setInputs("remove_from_dashboard" = TRUE)
    expect_false(dot_args$parent$in_grid[[dot_args$parent$selected_block]])

    # Removed node and block
    session$setInputs("removed_node" = "select_test")
    expect_identical(dot_args$parent$removed_block, "select_test")
    expect_identical(dot_args$parent$removed_edge, board_links(board$board)$id)
    mock_remove_block(
      "select_test",
      board,
      dot_args$parent,
      session
    )

    # Selected block
    expect_identical(dot_args$parent$selected_block, "test")

    # Restore state: maybe TBD setup a mocked network from json object
    dot_args$parent$refreshed <- "board"
    session$flushReact()
  }
)

test_that("network ui works", {
  ui <- add_rm_link_ui("mod", new_board())
  expect_s3_class(ui, "shiny.tag.list")
})
