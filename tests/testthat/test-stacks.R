my_board <- new_dag_board()

test_session <- shiny::MockShinySession$new()

test_that("add_block_to_stack works", {
  expect_null(add_block_to_stack(
    my_board,
    "block_id",
    "stack_id",
    session = test_session
  ))
})

test_that("remove_block_from_stack works", {
  expect_null(remove_block_from_stack(
    my_board,
    "block_id",
    "board_id",
    session = test_session
  ))
})

test_that("stack_ui works", {
  expect_null(stack_ui("stack_id", my_board, "block_id"))
})
