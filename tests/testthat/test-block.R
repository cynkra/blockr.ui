test_that("get_block_registry works", {
  expect_error(get_block_registry(character()))
  res <- get_block_registry(new_dataset_block())
  expect_s3_class(res, "registry_entry")
  expect_identical(attr(res, "ctor_name"), "new_dataset_block")
})


my_board <- new_board(
  blocks = new_dataset_block(),
  class = "dash_board"
)

test_that("block ui works", {
  ui <- block_ui(
    "my_ui",
    my_board
  )
  expect_s3_class(ui, "shiny.tag.list")
  ui_query <- htmltools::tagQuery(ui)
  expect_true(grepl(board_block_ids(my_board), ui_query$allTags()$attribs$id))
})

test_that("remove_block_ui works", {
  session <- as.environment(
    list(
      ns = identity,
      sendRemoveUI = function(selector = selector, multiple = multiple) {
        session$lastRemovedUI <- list(
          selector = selector,
          multiple = multiple
        )
      }
    )
  )

  expect_error(remove_block_ui(character(), my_board, session = session))
  remove_block_ui("my_ui", my_board, session = session)
  expect_identical(session$lastRemovedUI$selector, "#my_ui_blocks > div")

  remove_block_ui(
    "my_ui",
    my_board,
    blocks = board_block_ids(my_board),
    session = session
  )
  expect_true(grepl(board_block_ids(my_board), session$lastRemovedUI$selector))
})
