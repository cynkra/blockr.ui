testServer(
  main_server,
  args = list(board = new_board(class = c("dock_board", "dash_board"))),
  {
    expect_named(
      app_state,
      c(
        "mode",
        "cold_start",
        "preview",
        "grid",
        "in_grid",
        "refreshed",
        "network",
        "append_block",
        "added_block",
        "removed_block",
        "selected_block",
        "cancelled_edge",
        "added_edge",
        "removed_edge",
        "added_stack",
        "stack_added_block",
        "stack_removed_block",
        "removed_stack"
      )
    )
  }
)

test_that("main ui works", {
  ui <- main_ui("main", new_board())
  expect_s3_class(ui, "shiny.tag.list")
})
