test_that("dock_ui works", {
  ui <- dashboard_ui("mod", new_dag_board())
  expect_s3_class(ui, "shiny.tag")
})
