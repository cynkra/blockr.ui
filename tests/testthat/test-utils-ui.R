test_that("dropdown_button works", {
  expect_s3_class(dropdown_button(div(), icon = icon("cogs")), "shiny.tag.list")
})

test_that("offcanvas works", {
  el <- off_canvas("test", "title", div())
  expect_s3_class(el, "shiny.tag")
})
