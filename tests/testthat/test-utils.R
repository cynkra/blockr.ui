test_that("process_app_state works", {
  expect_error(process_app_state(""))
  state <- list(network = list(nodes = list(list(id = 1, x = 1, y = 2))))
  res <- process_app_state(state)
  expect_null(res$network$nodes[[1]]$x)
  expect_null(res$network$nodes[[1]]$y)
})
