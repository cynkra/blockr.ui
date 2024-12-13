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
  session$setInputs(scoutbar = "dataset_block")
  expect_s3_class(rv$new_block, c("dataset_block", "data_block", "block"))
  expect_true(nrow(rv$nodes) == 1)

  # Add new block again
  session$setInputs(add_block = 1)
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
