library(blockr.core)
library(blockr.dplyr)

test_blk <- new_dataset_block()
attr(test_blk, "uid") <- "test"

test_session <- shiny::MockShinySession$new()

test_that("Add nodes works", {
  rv <- list(nodes = data.frame())
  expect_error(add_node(list(), rv))
  expect_error(add_node(test_blk, list()))
  rv <- add_node(test_blk, rv)

  expect_s3_class(rv$nodes, "data.frame")
  expect_identical(rv$nodes$id, block_uid(test_blk))
})

test_that("Remove node works", {
  expect_error(remove_node("www", list(), test_session))
  rv <- list(nodes = data.frame())
  rv <- add_node(test_blk, rv)
  expect_error(
    remove_node(character(), rv, test_session)
  )
  expect_error(remove_node("www", rv, test_session))
  rv <- remove_node(rv$nodes[1, "id"], rv, test_session)
  expect_true(nrow(rv$nodes) == 0)
})

test_that("Add edge works", {
  expect_error(add_edge(from = "1", to = "2", label = "plop", vals = list()))
  expect_error(add_edge(
    from = 1,
    to = "2",
    label = "plop",
    vals = list(edges = data.frame())
  ))
  expect_error(add_edge(
    from = "1",
    to = 2,
    label = "plop",
    vals = list(edges = data.frame())
  ))
  rv <- list(edges = data.frame())
  expect_error(add_edge(
    from = "1",
    to = "2",
    label = "plop",
    vals = rv,
    create_link = FALSE
  ))
  rv <- add_edge(from = "1", to = "2", label = "plop", vals = rv)
  expect_type(rv, "list")
  expect_length(rv, 2)
  expect_s3_class(rv$edges, "data.frame")
  expect_identical(nrow(rv$edges), 1L)
  expect_s3_class(rv$added_edge, "links")
  expect_identical(rv$edges$id, names(rv$added_edge))
})

test_that("Remove edge works", {
  expect_error(remove_edge("1", list(edges = NULL), test_session))
  expect_error(remove_edge("1", list(edges = data.frame()), test_session))

  rv <- list(edges = data.frame())
  rv <- add_edge(from = "1", to = "2", label = "test", vals = rv)
  expect_error(remove_edge("", rv, test_session))
  expect_error(remove_edge(2, rv, test_session))
  expect_error(remove_edge("1_2", rv, test_session))
  old_rv <- rv
  rv <- remove_edge(rv$edges$id, rv, test_session)
  expect_true(nrow(rv$edges) == 0)
  expect_identical(rv$removed_edge, old_rv$edges$id)
})

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

testServer(
  blockr.ui::add_rm_link_server,
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
      refreshed = NULL,
      nodes = data.frame(),
      append_block = FALSE,
      added_block = NULL,
      removed_block = NULL,
      selected_block = NULL,
      edges = data.frame(),
      cancelled_edge = NULL,
      added_edge = NULL,
      removed_edge = NULL,
      added_stack = NULL,
      stack_added_block = NULL,
      stack_removed_block = NULL,
      removed_stack = NULL
    )
  ),
  {
    # Init state
    expect_true(nrow(dot_args$parent$nodes) == 0)
    expect_true(nrow(dot_args$parent$edges) == 0)
    expect_length(vals$stacks, 0)
    # Init for click
    session$setInputs(new_stack = 0)

    output$network
    session$setInputs(network_initialized = TRUE, stabilized = TRUE)

    # Simulate add a node
    mock_add_block(new_dataset_block(), board, dot_args$parent, session)
    expect_true(nrow(dot_args$parent$nodes) == 1)

    # Add select block
    mock_add_block(new_select_block(), board, dot_args$parent, session)
    expect_true(nrow(dot_args$parent$nodes) == 2)
    expect_true(nrow(dot_args$parent$edges) == 0)
    expect_null(update())
    # Connect data and select
    session$setInputs(
      new_edge = list(
        from = board_block_ids(board$board)[[1]],
        to = board_block_ids(board$board)[[2]]
      )
    )
    expect_true(nrow(dot_args$parent$edges) == 1)
    expect_identical(names(update()$links$add), dot_args$parent$edges$id)

    # Select data block
    session$setInputs(network_selected = board_block_ids(board$board)[[2]])
    expect_identical(
      dot_args$parent$selected_block,
      board_block_ids(board$board)[[2]]
    )

    # Add a stack
    session$setInputs(
      selected_nodes = board_block_ids(board$board),
      new_stack = 1
    )
    expect_identical(dot_args$parent$added_stack, board_block_ids(board$board))

    board_stacks(board$board) <- stacks(new_stack(
      blocks = dot_args$parent$added_stack
    ))
    session$flushReact()

    expect_identical(
      unique(dot_args$parent$nodes$group),
      board_stack_ids(board$board)
    )

    # Add block to existing stack (should belong to the current stack)
    dot_args$parent$append_block <- TRUE
    mock_add_block(new_select_block(), board, dot_args$parent, session)
    expect_identical(
      tail(dot_args$parent$nodes$group, n = 1),
      board_stack_ids(board$board)
    )

    # Sadly this does not work due to a bug in Shiny: remove stack
    session$setInputs(
      selected_nodes = board_block_ids(board$board),
      remove_stack = 0
    )
    expect_null(dot_args$parent$removed_stack)
    #expect_true(all(is.na(dot_args$parent$nodes$group)))
    board_stacks(board$board) <- stacks()

    # Remove connection
    session$setInputs(remove_edge = 0)
    for (i in seq_along(dot_args$parent$edges$id)) {
      session$setInputs(
        selected_edge = dot_args$parent$edges$id[1],
        remove_edge = i
      )
    }
    expect_true(nrow(dot_args$parent$edges) == 0)
    expect_identical(update()$links$rm, input$selected_edge)

    # Remove blocks
    session$setInputs(remove_blocks = 0)
    expect_identical(dot_args$parent$removed_block, input$selected_nodes)
    expect_true(nrow(dot_args$parent$nodes) == 0)
    mock_cleanup_state(board)

    # Invalid connections: let's add 2 data block and try to connect them
    # this will fail
    mock_add_block(new_dataset_block(), board, dot_args$parent, session)
    mock_add_block(new_dataset_block(), board, dot_args$parent, session)
    session$setInputs(
      new_edge = list(
        from = board_block_ids(board$board)[[1]],
        to = board_block_ids(board$board)[[2]]
      )
    )
    expect_true(nrow(dot_args$parent$edges) == 0)
    mock_cleanup_state(board)

    ## Invalid append -> cancelling edge
    mock_add_block(new_dataset_block(), board, dot_args$parent, session)
    dot_args$parent$append_block <- TRUE
    mock_add_block(new_dataset_block(), board, dot_args$parent, session)
    expect_identical(
      dot_args$parent$cancelled_edge,
      board_block_ids(board$board)[2]
    )

    # Restore
    dot_args$parent$refreshed <- "board"
    session$flushReact()
  }
)

# TBD test network restore?

test_that("network ui works", {
  ui <- blockr.ui::add_rm_link_ui("mod", new_board())
  expect_s3_class(ui, "shiny.tag.list")
})
