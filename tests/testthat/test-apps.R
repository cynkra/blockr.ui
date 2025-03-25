test_that("demo app works", {
  app <- run_demo_app()
  expect_s3_class(app, "shiny.appobj")
})
