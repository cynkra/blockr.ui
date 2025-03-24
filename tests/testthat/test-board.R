library(shinytest2)
library(blockr.dplyr)
#library(blockr.ai)

mock_add_block <- function(blk, board_update, session) {
  board_update(
    list(blocks = list(add = as_blocks(blk)))
  )
  session$flushReact()
}

testServer(
  board_server,
  args = list(
    x = new_board(class = "custom_board"),
    plugins = custom_board_plugins(
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
      grid = grid_server,
      app_mod = manage_app_mode,
      manage_sidebars = manage_sidebars,
      # Only one block can be visible at a time in the sidebar,
      # as only one block can be selected at a time in the network
      block_visibility = manage_block_visibility,
      # Callback to signal other modules that the restore is done.
      # This allows to restore each part in the correct order.
      on_board_restore = board_restore
    ),
    parent = reactiveValues(
      mode = "network",
      preview = FALSE,
      grid = data.frame(),
      in_grid = list(),
      refreshed = NULL,
      nodes = data.frame(),
      append_block = FALSE,
      added_block = NULL,
      removed_block = NULL,
      selected_block = NULL,
      edges = data.frame(),
      cancelled_edge = NULL,
      added_edge = NULL,
      removed_edge = NULL,
      added_stack = NULL,
      stack_added_block = NULL,
      stack_removed_block = NULL,
      removed_stack = NULL
    )
  ),
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
    mock_add_block(new_dataset_block(), board_update, session)
    mock_add_block(new_select_block(), board_update, session)
    dot_args$parent$selected_block <- board_block_ids(rv$board)[[1]]
    session$flushReact()
  }
)

test_that("Board works", {
  skip_on_cran()

  chromote::local_chrome_version(
    "latest-stable",
    binary = "chrome-headless-shell"
  )

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
