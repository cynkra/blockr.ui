test_that("get_block_registry works", {
  expect_error(get_block_registry(character()))
  res <- get_block_registry(new_dataset_block())
  expect_s3_class(res, "registry_entry")
  expect_identical(attr(res, "ctor_name"), "new_dataset_block")
})


my_board <- new_dash_board(blocks = new_dataset_block())

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
  session <- MockShinySession$new()
  expect_null(remove_block_ui(character(), my_board, session = session))
})
