testServer(
  add_rm_block_server,
  args = list(
    board = reactiveValues(),
    update = reactiveVal(),
    parent = reactiveValues(
      scoutbar = NULL,
      added_block = NULL,
      removed_block = NULL,
      cancelled_edge = NULL
    )
  ),
  {
    # Test adding new block
    dot_args$parent$scoutbar <- list(
      action = "add_block",
      value = "dataset_block"
    )
    session$flushReact()
    expect_identical(dot_args$parent$scoutbar$value, "dataset_block")
    expect_s3_class(dot_args$parent$added_block, "dataset_block")
    expect_type(attr(dot_args$parent$added_block, "uid"), "character")
    expect_s3_class(update()$blocks$add, "blocks")

    # Test removing block
    dot_args$parent$removed_block <- attr(dot_args$parent$added_block, "uid")
    session$flushReact()
    expect_identical(update()$blocks$rm, dot_args$parent$removed_block)

    # Test edge cancellation
    dot_args$parent$cancelled_edge <- "test2"
    session$flushReact()
    expect_identical(update()$blocks$rm, "test2")

    # Verify added_block is reset after removal
    expect_null(dot_args$parent$added_block)
  }
)

test_that("add_rm_bloc_ui works", {
  ui <- add_rm_block_ui("mod", new_board())
  expect_null(ui)
})
