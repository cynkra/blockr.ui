library(shinytest2)

test_that("Board works", {
  skip_on_cran()
  shiny_app_path <- system.file("examples/demo/app.R", package = "blockr.ui")
  app <- AppDriver$new(
    shiny_app_path,
    name = "demo-app",
    seed = 4323
  )
  app$expect_values()

  # Add block
  app$click("board-dag-add_block")
  app$click(selector = ".scout__bar-wrapper button[aria-label=\"dataset_block\"]")
  app$expect_values()

  # Select a block: does not have input binding
  blks <- app$get_value(export = "board-blocks")
  app$set_inputs(
    `board-dag-network_selected` = names(blks)[[1]],
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()
  app$expect_values()

  # Update data fields
  dataset_nme <- paste0("board-", names(blks)[[1]],"-expression-dataset")
  fields <- setNames("CO2", dataset_nme)
  app$set_inputs(!!!fields)
  app$wait_for_idle()
  app$expect_values()

  # Remove
  app$click("board-dag-remove")
  app$wait_for_idle()
  app$expect_values()

  app$stop()
})
