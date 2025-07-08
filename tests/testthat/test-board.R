library(shinytest2)
library(blockr.dplyr)
library(blockr.sdtm)
library(blockr.ai)
library(blockr.io)

mock_add_block <- function(blk, board_update, parent, session) {
  new_blk <- as_blocks(blk)
  board_update(
    list(blocks = list(add = new_blk))
  )
  parent$added_block <- new_blk[[1]]
  attr(parent$added_block, "uid") <- names(new_blk)
  session$flushReact()
}

create_mock_params <- function(board = new_dash_board()) {
  list(
    x = board,
    plugins = dash_board_plugins(
      c(
        "preserve_board",
        "manage_blocks",
        "manage_links",
        "manage_stacks",
        "generate_code",
        "notify_user"
      )
    ),
    callbacks = list(
      grid = dashboard_server,
      # Callback to signal other modules that the restore is done.
      # This allows to restore each part in the correct order.
      on_board_restore = board_restore,
      manage_scoutbar = manage_scoutbar,
      layout = build_layout
    ),
    parent = create_app_state(board)
  )
}

test_board_server <- function(board = new_dash_board()) {
  testServer(
    board_server,
    args = create_mock_params(noard),
    {
      # # Test app modes toggle
      # expect_identical(dot_args$parent$mode, "network")
      # expect_false(dot_args$parent$preview)
      # session$setInputs(preview = 0, mode = 0)
      # session$setInputs(mode = 1)
      # expect_identical(dot_args$parent$mode, "dashboard")
      # session$setInputs(preview = 1)
      # expect_true(dot_args$parent$preview)
      # session$setInputs(mode = 2)
      # expect_identical(dot_args$parent$mode, "network")
      # # Add a block
      # mock_add_block(
      #   new_dataset_block(),
      #   board_update,
      #   dot_args$parent,
      #   session
      # )
      # mock_add_block(new_select_block(), board_update, dot_args$parent, session)
      # dot_args$parent$selected_block <- board_block_ids(rv$board)[[1]]
      # session$flushReact()
      # # Grid
      # lapply(board_block_ids(rv$board), \(blk_id) {
      #   expect_false(dot_args$parent$in_grid[[blk_id]])
      # })
      # session$setInputs(add_to_dashboard = TRUE)
      # expect_true(dot_args$parent$in_grid[[dot_args$parent$selected_block]])
      # session$setInputs(mode = 3)
      # dot_args$parent$selected_block <- board_block_ids(rv$board)[[2]]
      # session$flushReact()
      # session$setInputs(add_to_dashboard = TRUE)
      # lapply(board_block_ids(rv$board), \(blk_id) {
      #   expect_true(dot_args$parent$in_grid[[blk_id]])
      # })
      # session$setInputs(add_to_dashboard = FALSE)
      # dot_args$parent$selected_block <- board_block_ids(rv$board)[[1]]
      # session$flushReact()
      # session$setInputs(add_to_dashboard = FALSE)
      # lapply(board_block_ids(rv$board), \(blk_id) {
      #   expect_false(dot_args$parent$in_grid[[blk_id]])
      # })
      # output[[type]]
      # # Remove block (see if vals$in_grid is updated)
      # session$setInputs(add_to_dashboard = TRUE)
      # dot_args$parent$removed_block <- board_block_ids(rv$board)[[1]]
      # session$flushReact()
      # expect_named(dot_args$parent$in_grid, board_block_ids(rv$board))
      # # Grid zoom
      # session$setInputs(dashboard_zoom = 1)
      # # Lock grid
      # session$setInputs(lock = TRUE)
      # # Restore
      # dot_args$parent$refreshed <- "network"
      # session$flushReact()
    }
  )
}

#test_board_server("dock")

# test_that("Board dock app works", {
#   skip_on_cran()

#   app <- AppDriver$new(
#     system.file(package = "blockr.ui", "examples/dashboard/dock"),
#     name = "dashboard-dock-app",
#     seed = 4323
#   )

#   inputs <- c(
#     "main-board-manage_blocks-scoutbar-configuration",
#     "main-board-manage_links-network_initialized",
#     "main-board-lock",
#     "main-board-properties",
#     "main-board-dashboard"
#   )

#   app$expect_values(input = inputs, export = TRUE)

#   # Add block
#   app$click(selector = ".g6-toolbar-item[value=\"add-block\"")
#   app$wait_for_idle()
#   app$click(
#     selector = ".scout__bar-wrapper button[aria-label=\"dataset_block\"]"
#   )
#   app$wait_for_idle()
#   app$expect_values(input = inputs, export = TRUE)
#   app$click("main-board-manage_blocks-remove_block")
#   app$wait_for_idle()
#   app$expect_values(input = inputs, export = TRUE)
#   app$stop()
# })
