test_that("get_block_registry works", {
  expect_error(get_block_metadata(character()))
  res <- get_block_metadata(new_dataset_block())
  expect_type(res, "list")
  expect_named(res, c("category", "name", "description", "package"))
})


my_board <- new_dag_board(blocks = new_dataset_block())

test_that("block ui works", {
  ui <- block_ui(
    "my_ui",
    my_board,
    board_blocks(my_board)
  )
  expect_s3_class(ui, "shiny.tag")
  ui_query <- htmltools::tagQuery(ui)
  expect_true(grepl(board_block_ids(my_board), ui_query$allTags()$attribs$id))
})

test_that("remove_block_ui works", {
  expect_null(remove_block_ui(character(), my_board))
})

# session <- as.environment(
#   list(
#     ns = identity,
#     input = list(
#       "layout_state" = test_dock
#     ),
#     sendCustomMessage = function(type, message) {
#       session$lastCustomMessage <- list(
#         type = type,
#         message = message
#       )
#     },
#     sendRemoveUI = function(selector = selector, multiple = multiple) {
#       session$lastRemovedUI <- list(
#         selector = selector,
#         multiple = multiple
#       )
#     },
#     sendInsertUI = function(selector, ui, multiple = FALSE) {
#       session$lastInsertedUI <- list(
#         selector = selector,
#         multiple = multiple
#       )
#     }
#   )
# )

# # Helper function to temporarily set .globals
# with_test_domain <- function(domain, expr) {
#   old <- get(".globals", envir = asNamespace("shiny"))
#   on.exit(assign(".globals", old, envir = asNamespace("shiny")))
#   assign(".globals", list(domain = domain), envir = asNamespace("shiny"))
#   force(expr)
# }

# test_that("insert_block_ui works", {
#   with_test_domain(session, {
#     insert_block_ui(
#       "my_block",
#       my_board,
#       session = session
#     )
#     expect_true(
#       is.list(session$lastCustomMessage) &&
#         session$lastCustomMessage$type == "show-block"
#     )
#     expect_true(
#       grepl("layout-block-my_block", session$lastCustomMessage$message$panel_id)
#     )
#   })
# })

# test_that("hide_block_panel works", {
#   parent <- list(
#     offcanvas = "offcanvas",
#     selected_block = "block4"
#   )
#   hide_block_panel("my_block", parent, session)
#   expect_true(
#     is.list(session$lastCustomMessage) &&
#       session$lastCustomMessage$type == "hide-block"
#   )
#   expect_true(
#     grepl("layout-block-my_block", session$lastCustomMessage$message$panel_id)
#   )
# })

# test_that("show_block_panel works", {
#   parent <- list(
#     selected_block = "block4"
#   )
#   show_block_panel("my_block", parent, session)
#   expect_true(
#     is.list(session$lastCustomMessage) &&
#       session$lastCustomMessage$type == "show-block"
#   )
#   expect_true(
#     grepl("layout-block-my_block", session$lastCustomMessage$message$panel_id)
#   )
# })
