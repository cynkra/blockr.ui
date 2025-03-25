test_that("add_block.ui_deps works", {
  res <- add_blockr.ui_deps(div())
  expect_s3_class(res, "shiny.tag.list")
  expect_s3_class(res[[1]], "shiny.tag")
  expect_s3_class(res[[2]], "html_dependency")
})
