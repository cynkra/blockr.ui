test_that("grid_ui works", {
  ui <- dashboard_ui("mod", new_board(class = "grid_board"))
  expect_length(ui, 3)
  expect_named(ui, c("add_to_grid", "options", "content"))
})
