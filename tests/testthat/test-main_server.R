board <- new_dag_board()

test_that("main_server works", {
  expect_error(main_server("id", NULL, NULL, NULL))
  expect_error(main_server("id", board, NULL, list()))
})

board <- new_dag_board(
  blocks = c(
    a = new_dataset_block("BOD"),
    b = new_dataset_block("ChickWeight"),
    c = new_merge_block("Time")
  ),
  links = c(
    ac = new_link("a", "c", "x"),
    bc = new_link("b", "c", "y")
  ),
  stacks = list(ac = c("a", "c"))
)

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

testServer(
  main_server,
  args = list(
    board = board,
    plugins = plugins(
      preserve_board(server = ser_deser_server, ui = ser_deser_ui),
      manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
      manage_links(
        server = gen_add_rm_link_server(ctx_menu_items),
        ui = add_rm_link_ui
      ),
      manage_stacks(server = add_rm_stack_server, ui = add_rm_stack_ui),
      generate_code(server = generate_code_server, ui = generate_code_ui),
      notify_user()
    ),
    modules = modules
  ),
  {
    expect_true(app_state$cold_start)
    expect_null(app_state$refreshed)
    expect_s3_class(app_state$network, "network")
    expect_false(app_state$append_block)
    expect_null(app_state$added_block)
    expect_null(app_state$removed_block)
    expect_null(app_state$selected_block)
    expect_null(app_state$cancelled_edge)
    expect_null(app_state$added_edge)
    expect_null(app_state$removed_edge)
    expect_null(app_state$added_stack)
    expect_null(app_state$stack_added_block)
    expect_null(app_state$stack_removed_block)
    expect_null(app_state$removed_stack)
    expect_null(app_state$stacks)
    expect_false(app_state$open_scoutbar)
    expect_null(app_state$scoutbar$trigger)
    expect_null(app_state$scoutbar$action)
    expect_null(app_state$scoutbar$value)
    expect_false(app_state$scoutbar$is_open)
    expect_false(app_state$save_board)
    expect_length(app_state$backup_list, 0)
    expect_false(app_state$display_code)
    expect_null(app_state$added_to_dashboard)
    expect_null(app_state$removed_from_dashboard)

    session$flushReact()
    expect_false(app_state$cold_start)
  }
)
