library(shinytest2)

test_that("Board works", {
  skip_on_cran()
  demo_app <- run_demo_app()
  app <- AppDriver$new(
    demo_app,
    name = "demo-app",
    seed = 4323
  )

  #app$expect_values()

  # Add block
  #app$click("main-board-manage_blocks-add_block")
  #app$click(
  #  selector = ".scout__bar-wrapper button[aria-label=\"dataset_block\"]"
  #)
  #app$wait_for_idle()
  #app$expect_values()
  app$stop()
})
