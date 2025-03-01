library(blockr.core)
library(blockr.dplyr)

test_blk <- new_dataset_block()
attr(test_blk, "uid") <- "test"

test_session <- shiny::MockShinySession$new()

test_that("Add nodes works", {
  rv <- list(nodes = data.frame())
  expect_error(add_node(list(), rv))
  expect_error(add_node(test_blk, list()))
  rv <- add_node(test_blk, rv)

  expect_s3_class(rv$nodes, "data.frame")
  expect_identical(rv$nodes$id, block_uid(test_blk))
})

test_that("Remove block works", {
  expect_error(remove_node("www", list(), test_session))
  rv <- list(nodes = data.frame())
  rv <- add_node(test_blk, rv)
  expect_error(
    remove_node(character(), rv, test_session)
  )
  expect_error(remove_node("www", rv, test_session))
  rv <- remove_node(rv$nodes[1, "id"], rv, test_session)
  expect_true(nrow(rv$nodes) == 0)
})

test_that("Add edge works", {
  expect_error(add_edge(from = "1", to = "2", label = "plop", vals = list()))
  expect_error(add_edge(
    from = 1,
    to = "2",
    label = "plop",
    vals = list(edges = data.frame())
  ))
  expect_error(add_edge(
    from = "1",
    to = 2,
    label = "plop",
    vals = list(edges = data.frame())
  ))
  rv <- list(edges = data.frame())
  expect_error(add_edge(
    from = "1",
    to = "2",
    label = "plop",
    vals = rv,
    create_link = FALSE
  ))
  rv <- add_edge(from = "1", to = "2", label = "plop", vals = rv)
  expect_type(rv, "list")
  expect_length(rv, 2)
  expect_s3_class(rv$edges, "data.frame")
  expect_identical(nrow(rv$edges), 1L)
  expect_s3_class(rv$added_edge, "links")
  expect_identical(rv$edges$id, names(rv$added_edge))
})

test_that("Remove edge works", {
  expect_error(remove_edge("1", list(edges = NULL), test_session))
  expect_error(remove_edge("1", list(edges = data.frame()), test_session))

  rv <- list(edges = data.frame())
  rv <- add_edge(from = "1", to = "2", label = "test", vals = rv)
  expect_error(remove_edge("", rv, test_session))
  expect_error(remove_edge(2, rv, test_session))
  expect_error(remove_edge("1_2", rv, test_session))
  old_rv <- rv
  rv <- remove_edge(rv$edges$id, rv, test_session)
  expect_true(nrow(rv$edges) == 0)
  expect_identical(rv$removed_edge, old_rv$edges$id)
})
