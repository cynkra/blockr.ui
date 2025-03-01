library(shinytest2)
library(blockr.dplyr)
#library(blockr.ai)

test_that("Board works", {
  skip_on_cran()
  demo_app <- run_demo_app()
  app <- AppDriver$new(
    demo_app,
    name = "demo-app",
    seed = 4323
  )

  inputs <- c(
    "main-board-manage_blocks-scoutbar-configuration",
    "main-board-manage_links-network_initialized",
    "main-board-lock",
    "main-board-properties",
    "main-board-dashboard"
  )

  app$expect_values(input = inputs, export = TRUE)

  # Add block
  app$click("main-board-manage_blocks-add_block")
  app$wait_for_idle()
  app$click(
    selector = ".scout__bar-wrapper button[aria-label=\"dataset_block\"]"
  )
  app$wait_for_idle()
  app$expect_values(input = inputs, export = TRUE)
  app$click("main-board-manage_blocks-remove_block")
  app$wait_for_idle()
  app$expect_values(input = inputs, export = TRUE)
  app$stop()
})
