test_that("process_app_state works", {
  expect_error(process_app_state(""))
  expect_error(process_app_state(list(grid = data.frame())))
  state <- list(nodes = data.frame(x = NA, y = NA))
  res <- process_app_state(state)
  expect_null(colnames(res))
})
