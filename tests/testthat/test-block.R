test_that("get_block_registry works", {
  expect_error(get_block_metadata(character()))
  res <- get_block_metadata(new_dataset_block())
  expect_type(res, "list")
  expect_named(res, c("category", "name", "description", "package"))

  new_local_block <- function(...) {
    new_block(
      server = function(id) {},
      ui = function(id) {
        div()
      },
      class = "local_block",
      ...
    )
  }
  res <- get_block_metadata(new_local_block())
  expect_identical(res$category, "Uncategorized")
})


my_board <- new_dag_board(blocks = new_dataset_block())

test_that("block ui works", {
  ui <- block_ui(
    "my_ui",
    my_board,
    board_blocks(my_board)
  )
  expect_s3_class(ui, "shiny.tag")
  ui_query <- htmltools::tagQuery(ui)
  expect_true(grepl(board_block_ids(my_board), ui_query$allTags()$attribs$id))
})

test_that("remove_block_ui works", {
  expect_null(remove_block_ui(character(), my_board))
})
