library(blockr.core)
library(blockr.dplyr)

testServer(network_server, {
  # Init state
  expect_s3_class(rv$edges, "data.frame")
  expect_s3_class(rv$nodes, "data.frame")
  expect_true(nrow(rv$edges) == 0)
  expect_true(nrow(rv$nodes) == 0)
  session$setInputs(add_block = 0)
  expect_null(rv$new_block)
  output$network

  # Add data block
  session$setInputs(add_block = 1)
  session$setInputs(scoutbar = "dataset_block")
  expect_s3_class(rv$new_block, c("dataset_block", "data_block", "block"))
  expect_true(nrow(rv$nodes) == 1)

  # Add new block again
  session$setInputs(add_block = 2)
  expect_null(rv$new_block)
  session$setInputs(scoutbar = "select_block")
  expect_s3_class(rv$new_block, c("select_block", "transform_block", "block"))
  expect_true(nrow(rv$nodes) == 2)

  # Remove last added block
  session$setInputs(network_selected = tail(rv$nodes$id, n = 1), remove = 1)
  expect_true(nrow(rv$nodes) == 1)

  # Inspect returned values
  expect_equal(
    names(session$returned),
    c("edges", "nodes", "selected_node", "added_block", "removed_block")
  )
  invisible(
    lapply(names(session$returned), \(val) {
      expect_true(is.reactive(session$returned[[val]]))
    })
  )

  expect_equal(session$returned$added_block(), rv$new_block)
  expect_equal(session$returned$removed_block(), input$network_selected)
})

test_that("Add nodes works", {
  nodes <- data.frame()
  expect_error(add_node(list(), nodes))

  blk <- new_dataset_block()
  expect_error((add_node(blk, list())))
  nodes <- add_node(blk, nodes)

  expect_s3_class(nodes, "data.frame")
  expect_identical(nodes$id, block_uid(blk))
})

test_that("Remove block works", {
  expect_error(remove_node("www", data.frame()))
  expect_error(
    remove_node(character(),
    add_node(new_dataset_block(), data.frame()))
  )
  nodes <- add_node(new_dataset_block(), data.frame())
  nodes <- remove_node(nodes[1, "id"], nodes)
  expect_error(remove_node("www", nodes))
  expect_true(nrow(nodes) == 0)
})

test_that("Network ui", {
  expect_type(network_ui("plop"), "list")
  expect_named(network_ui("plop"), c("action_bar", "sidebar", "canvas"))
})