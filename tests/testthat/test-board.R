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

mock_remove_block <- function(id, parent, session) {
  parent$removed_block <- id
  session$flushReact()
}

create_mock_params <- function(board = new_dag_board()) {
  modules <- board_modules(board)

  ctx_menu_items <- unlst(
    c(
      list(
        list(
          create_edge_ctxm,
          remove_node_ctxm,
          remove_edge_ctxm,
          append_node_ctxm,
          create_stack_ctxm,
          remove_stack_ctxm,
          add_block_ctxm
        )
      ),
      lapply(modules, board_module_context_menu)
    )
  )

  plugins <- plugins(
    preserve_board(server = ser_deser_server, ui = ser_deser_ui),
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
    manage_links(
      server = gen_add_rm_link_server(ctx_menu_items),
      ui = add_rm_link_ui
    ),
    manage_stacks(server = add_rm_stack_server, ui = add_rm_stack_ui),
    generate_code(server = generate_code_server, ui = generate_code_ui),
    notify_user()
  )

  list(
    x = board,
    plugins = plugins,
    callbacks = c(
      lapply(modules, board_module_server),
      list(
        # Callback to signal other modules that the restore is done.
        # This allows to restore each part in the correct order.
        on_board_restore = board_restore,
        manage_scoutbar = manage_scoutbar,
        layout = build_layout(modules, plugins)
      )
    ),
    parent = create_app_state(board)
  )
}

testServer(
  board_server,
  args = create_mock_params(),
  {
    # Init
    expect_length(dot_args$parent$in_grid, 0)
    expect_s3_class(dot_args$parent$grid, "dock")

    # Add block
    mock_add_block(
      new_dataset_block("mtcars"),
      board_update,
      dot_args$parent,
      session
    )

    test_dock <- list()
    test_dock[["panels"]] <- setNames(
      list(id = "dashboard"),
      "dashboard"
    )
    session$setInputs(layout_state = test_dock)

    expect_true(is_block(dot_args$parent$added_block))
    expect_false(dot_args$parent$in_grid[[block_uid(
      dot_args$parent$added_block
    )]])

    # Select block
    dot_args$parent$selected_block <- block_uid(
      dot_args$parent$added_block
    )

    # Add to dashboard
    dot_args$parent$added_to_dashboard <- block_uid(dot_args$parent$added_block)
    dot_args$parent$in_grid[[dot_args$parent$added_to_dashboard]] <- TRUE
    session$flushReact()
    output[[sprintf("dock-%s-result", block_uid(dot_args$parent$added_block))]]
    expect_null(dot_args$parent$added_to_dashboard)

    # To be able to remove panels later, we need to mock the dock state
    test_dock <- list()
    test_dock[["panels"]] <- setNames(
      list(id = block_uid(dot_args$parent$added_block)),
      sprintf("block-%s", block_uid(dot_args$parent$added_block))
    )
    session$setInputs(dock_state = test_dock, layout_state = test_dock)

    output$dock

    # Change dashboard zoom
    session$userData$dashboard_zoom <- 0.5
    session$flushReact()

    # Remove from dashboard
    dot_args$parent$removed_from_dashboard <- block_uid(
      dot_args$parent$added_block
    )
    dot_args$parent$in_grid[[dot_args$parent$removed_from_dashboard]] <- FALSE
    session$flushReact()
    # This does not work, but it should ...
    #expect_null(output[[sprintf(
    #  "dock-%s-result",
    #  block_uid(dot_args$parent$added_block)
    #)]])
    expect_null(dot_args$parent$removed_from_dashboard)

    # Remove block: returns a warning, no idea why ...
    mock_remove_block(
      block_uid(dot_args$parent$added_block),
      dot_args$parent,
      session
    )
    expect_null(dot_args$parent$in_grid[[dot_args$parent$removed_block]])

    # Refresh
    dot_args$parent$refreshed <- "network"
    session$flushReact()
    expect_identical(dot_args$parent$refreshed, "grid")

    # Scoutbar
    session$setInputs("scoutbar" = "dataset_block@add_block")
    expect_identical(dot_args$parent$scoutbar$action, "add_block")
    expect_identical(dot_args$parent$scoutbar$value, "dataset_block")

    dot_args$parent$open_scoutbar <- TRUE
    session$flushReact()

    dot_args$parent$append_block <- TRUE

    session$setInputs("scoutbar-open" = FALSE)
    expect_false(dot_args$parent$append_block)
    expect_false(dot_args$parent$open_scoutbar)
    expect_false(dot_args$parent$scoutbar$is_open)

    # Hide block panel in app layout
    mock_add_block(
      new_dataset_block("mtcars"),
      board_update,
      dot_args$parent,
      session
    )
    test_dock <- list()
    test_dock[["panels"]] <- setNames(
      list(id = block_uid(dot_args$parent$added_block)),
      sprintf("block-%s", block_uid(dot_args$parent$added_block))
    )
    session$setInputs(layout_state = test_dock)
    session$setInputs(
      "layout_panel-to-remove" = sprintf(
        "block-%s",
        block_uid(dot_args$parent$added_block)
      )
    )
  }
)

test_that("Board dock app works", {
  skip_on_cran()

  # We test from an existing dock so that we can fix block, stack and link IDs
  # to avoid randomness failure
  app <- AppDriver$new(
    system.file(package = "blockr.ui", "examples/dashboard/non-empty"),
    name = "dashboard-non-empty-app",
    seed = 4323
  )

  Sys.sleep(2)

  inputs <- c(
    "main-board-manage_blocks-scoutbar-configuration",
    "main-board-manage_links-network-initialized"
  )

  app$expect_values(input = inputs, export = TRUE)

  # Add block: is there a way to fix the block ID?
  #app$click(selector = ".g6-toolbar-item[value=\"add-block\"")
  #app$wait_for_idle()
  #app$click(
  #  selector = ".scout__bar-wrapper button[aria-label=\"dataset_block@add_block\"]"
  #)
  #app$wait_for_idle()
  #app$expect_values(input = inputs, export = TRUE)
  app$stop()
})
