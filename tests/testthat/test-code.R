test_that("multiplication works", {
  ui <- blockr.ui::gen_code_ui("mod", new_board())
  expect_s3_class(ui, "shiny.tag")
})
