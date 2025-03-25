test_that("grid_ui works", {
  ui <- grid_ui("mod")
  expect_length(ui, 3)
  expect_named(ui, c("add_to_grid", "options", "content"))
})
