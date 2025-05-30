test_that("dock_ui works", {
  ui <- dashboard_ui("mod", new_board(class = "dock_board"))
  expect_length(ui, 3)
  expect_named(ui, c("add_to_dashboard", "options", "content"))
})
