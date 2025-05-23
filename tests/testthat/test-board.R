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

create_mock_board <- function(type = c("grid", "dock")) {
  type <- match.arg(type)
  new_board(
    class = c(sprintf("%s_board", type), "dash_board"),
    options = new_board_options(
      dark_mode = "light",
      stacks_colors = hcl.colors(20, palette = "spectral")
    )
  )
}

create_mock_params <- function(type = c("grid", "dock")) {
  type <- match.arg(type)
  board <- create_mock_board(type)
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
      app_mod = manage_app_mode,
      manage_sidebars = manage_sidebars,
      # Only one block can be visible at a time in the sidebar,
      # as only one block can be selected at a time in the network
      block_visibility = manage_block_visibility,
      # Callback to signal other modules that the restore is done.
      # This allows to restore each part in the correct order.
      on_board_restore = board_restore
    ),
    parent = create_app_state(board)
  )
}

test_board_server <- function(type = c("grid", "dock")) {
  type <- match.arg(type)
  testServer(
    board_server,
    args = create_mock_params(type),
    {
      # Test app modes toggle
      expect_identical(dot_args$parent$mode, "network")
      expect_false(dot_args$parent$preview)
      session$setInputs(preview = 0, mode = 0)
      session$setInputs(mode = 1)
      expect_identical(dot_args$parent$mode, "dashboard")
      session$setInputs(preview = 1)
      expect_true(dot_args$parent$preview)
      session$setInputs(mode = 2)
      expect_identical(dot_args$parent$mode, "network")

      # Add a block
      mock_add_block(
        new_dataset_block(),
        board_update,
        dot_args$parent,
        session
      )
      mock_add_block(new_select_block(), board_update, dot_args$parent, session)
      dot_args$parent$selected_block <- board_block_ids(rv$board)[[1]]
      session$flushReact()

      # Grid
      lapply(board_block_ids(rv$board), \(blk_id) {
        expect_false(dot_args$parent$in_grid[[blk_id]])
      })
      session$setInputs(add_to_grid = TRUE)
      expect_true(dot_args$parent$in_grid[[dot_args$parent$selected_block]])
      session$setInputs(mode = 3)
      dot_args$parent$selected_block <- board_block_ids(rv$board)[[2]]
      session$flushReact()
      session$setInputs(add_to_grid = TRUE)
      lapply(board_block_ids(rv$board), \(blk_id) {
        expect_true(dot_args$parent$in_grid[[blk_id]])
      })

      session$setInputs(add_to_grid = FALSE)
      dot_args$parent$selected_block <- board_block_ids(rv$board)[[1]]
      session$flushReact()
      session$setInputs(add_to_grid = FALSE)
      lapply(board_block_ids(rv$board), \(blk_id) {
        expect_false(dot_args$parent$in_grid[[blk_id]])
      })

      output[[type]]

      # Remove block (see if vals$in_grid is updated)
      session$setInputs(add_to_grid = TRUE)
      dot_args$parent$removed_block <- board_block_ids(rv$board)[[1]]
      session$flushReact()
      expect_named(dot_args$parent$in_grid, board_block_ids(rv$board))

      # Grid zoom
      session$setInputs(grid_zoom = 1)

      # Lock grid
      session$setInputs(lock = TRUE)

      # Restore
      dot_args$parent$refreshed <- "network"
      session$flushReact()
    }
  )
}

# Test grid board
test_board_server("grid")

test_that("Board grid app works", {
  skip_on_cran()
  app <- AppDriver$new(
    system.file(package = "blockr.ui", "examples/dashboard/grid"),
    name = "dashboard-grid-app",
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

  # # Dashboard
  # app$click("main-board-mode")
  # app$expect_values(input = inputs, export = TRUE)
  # # Preview
  # app$click("main-board-preview")
  # app$expect_values(input = inputs, export = TRUE)
  # # Back to dashboard + network
  # app$click("main-board-preview")
  # app$expect_values(input = inputs, export = TRUE)
  # # Back to network
  # app$click("main-board-mode")
  # app$expect_values(input = inputs, export = TRUE)

  # # Add to dashboard
  # app$set_inputs("main-board-add_to_grid" = TRUE)
  # app$expect_values(input = inputs, export = TRUE)
  # app$click("main-board-mode")
  # app$expect_values(input = inputs, export = TRUE)
  # app$set_inputs("main-board-grid_zoom" = 1)
  # app$expect_values(input = inputs, export = TRUE)
  # app$set_inputs("main-board-grid_zoom" = 0.5, "main-board-lock" = TRUE)

  # # Add new node
  # app$click("main-board-manage_blocks-add_block")
  # app$wait_for_idle()
  # app$click(
  #   selector = ".scout__bar-wrapper button[aria-label=\"select_block\"]"
  # )
  # app$expect_values(input = inputs, export = TRUE)
  # app$click("main-board-mode")
  # app$set_inputs("main-board-add_to_grid" = TRUE)
  # app$click("main-board-mode")
  # app$click("main-board-preview")
  # app$expect_values(input = inputs, export = TRUE)

  # # Remove block
  # app$click("main-board-mode")

  # app$click("main-board-manage_blocks-remove_block")
  # app$wait_for_idle()
  # app$expect_values(input = inputs, export = TRUE)
  # app$stop()
})

###### DOCK ######
# Test grid board
test_board_server("dock")

test_that("Board dock app works", {
  skip_on_cran()

  app <- AppDriver$new(
    system.file(package = "blockr.ui", "examples/dashboard/dock"),
    name = "dashboard-dock-app",
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
