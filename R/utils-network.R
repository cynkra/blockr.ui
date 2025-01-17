#' Add a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param new New block to insert. Must be a valid
#' block registry entry
#' @param rv Reactive values with dataframe representing network data.
#' @keywords internal
add_node <- function(new, rv) {
  stopifnot(
    is_block(new),
    is.data.frame(rv$nodes)
  )

  node_data <- data.frame(
    id = block_uid(new),
    label = paste(attr(new, "class")[1], "\n id:", block_uid(new)),
    title = block_uid(new),
    shape = "circle",
    color = "#D2E5FF",
    stack = NA,
    icon.code = NA,
    x = NA,
    y = NA
  )

  rv$nodes <- if (nrow(rv$nodes) == 0) {
    node_data
  } else {
    rbind(
      rv$nodes,
      node_data
    )
  }
  rv
}

#' Add a edge
#'
#' Update dataframe for visNetwork graph
#'
#' @param from Node id.
#' @param to Node it.
#' @param label Edge label. This is useful to map the existing connected
#' nodes to the input slots of the receiving node (for instance a join block).
#' @param rv Reactive values, containing elements such as edges data.
#' @keywords internal
add_edge <- function(from, to, label, rv) {
  stopifnot(
    is.character(from),
    is.character(to),
    is.data.frame(rv$edges)
  )

  id <- sprintf("%s_%s", from, to)
  edge_data <- data.frame(
    # ID is necessary to avoid duplicating edges with visUpdateEdges
    id = id,
    from = from,
    to = to,
    label = label,
    arrows = "to"
  )

  if (nrow(rv$edges) == 0) {
    rv$edges <- edge_data
  } else {
    rv$edges <- rbind(
      rv$edges,
      edge_data
    )
  }
  rv$added_edge <- id
  rv
}

#' Remove a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param selected UID (character string) of node to remove.
#' @param rv Reactive values with dataframe representing nodes data.
#' @keywords internal
remove_node <- function(selected, rv) {
  stopifnot(
    is.data.frame(rv$nodes),
    nrow(rv$nodes) > 0,
    is.character(selected),
    nchar(selected) > 0
  )

  to_remove <- which(rv$nodes$id == selected)
  if (length(to_remove) == 0) {
    stop(sprintf("Can't find node with id %s in the data", selected))
  }
  rv$nodes <- rv$nodes[-to_remove, ]
  rv
}

#' Remove an edge
#'
#' Update dataframe for visNetwork graph
#'
#' @param selected UID (character string) of edge to remove.
#' @param rv Reactive values containing dataframe representing edges data.
#' @keywords internal
remove_edge <- function(selected, rv) {
  stopifnot(
    is.data.frame(rv$edges),
    nrow(rv$edges) > 0,
    is.character(selected),
    nchar(selected) > 0
  )
  to_remove <- grep(selected, rv$edges$id)
  if (length(to_remove) == 0) {
    stop(sprintf("Can't find edge with id %s in the data", selected))
  }

  rv$removed_edge <- rv$edges[to_remove, "id"]
  rv$edges <- rv$edges[-to_remove, ]
  rv
}

#' List node connections
#'
#' @param x block.
#' @param vals Reactive values.
#'
list_empty_connections <- function(x, vals) {
  UseMethod("list_empty_connections", x)
}

#' @export
list_empty_connections.data_block <- function(x, vals) {
  NULL
}

#' @export
list_empty_connections.block <- function(x, vals) {
  lgl_ply(vals$connections[[block_uid(x)]], \(slot) is.null(slot()))
}

#' Check node connection
#'
#' @param x block.
#' @param vals Reactive values.
#'
#' @export
check_connections <- function(x, vals) {
  UseMethod("check_connections", x)
}

#' @export
check_connections.data_block <- function(x, vals) {
  FALSE
}

#' @export
check_connections.block <- function(x, vals) {
  n_active_connections <- sum(!list_empty_connections(x, vals))
  isTRUE(n_active_connections < length(block_inputs(x)))
}

#' Check whether the node can receive connection
#'
#' @param x Block object.
#' @param con Connection. List containing from and to data.
#' @param vals Reactive values
#' @export
can_connect <- function(x, con, vals) {
  UseMethod("can_connect")
}

check_invalid_target <- function(con) {
  if (is.null(con$to)) {
    showNotification(
      "Unable to connect node. Please select a valid target.",
      type = "error"
    )
    FALSE
  } else {
    TRUE
  }
}

check_loop <- function(con) {
  if (con$from == con$to) {
    showNotification(
      "Can't create a connection on the same block.",
      type = "error"
    )
    FALSE
  } else {
    TRUE
  }
}

#' @export
can_connect.data_block <- function(x, con, vals) {
  showNotification(
    "Data blocks don't accept any incoming connection (data input).",
    type = "error"
  )
  FALSE
}

#' @export
can_connect.block <- function(x, con, vals) {
  if (!check_connections(x, vals)) {
    showNotification(
      "The target block can't receive anymore data input.",
      type = "error"
    )
    FALSE
  } else {
    TRUE
  }
}

validate_edge_creation <- function(con, vals) {
  res <- list()
  res$check_target <- check_invalid_target(con)
  # Rule 2
  res$check_loop <- check_loop(con)
  # Rule 3
  res$check_cons <- can_connect(
    vals$blocks[[con$to]]$block,
    con,
    vals
  )

  if (length(which(res == FALSE)) > 0) FALSE else TRUE
}

#' Create an edge and add it to the network
#'
#' This is different from \link{add_edge}, the later
#' is just involved to add a row in a dataframe.
#' There is a validation layer prior to knowing whether
#' we can add the edge. Then rv are updated and the graph
#' proxy is also updated.
#'
#' @param rv Local reactive values.
#' @param vals Parent reactive values.
#' @param session Shiny session object.
#' @export
create_edge <- function(rv, vals, session) {
  input <- session$input
  ns <- session$ns
  con <- input$new_edge

  rv$added_edge <- NULL
  if (!validate_edge_creation(con, vals)) return(NULL)

  # Create the connection
  to_blk <- vals$blocks[[con$to]]$block
  con_idx <- which(list_empty_connections(to_blk, vals) == TRUE)[[1]]
  rv <- add_edge(
    from = con$from,
    to = con$to,
    # TO DO: the connection must be made to the latest available input slot
    label = block_inputs(to_blk)[[con_idx]],
    rv = rv
  )

  visNetworkProxy(ns("network")) |>
    visUpdateEdges(rv$edges)
  rv
}

#' Create a new node it to the network
#'
#' This is different from \link{add_node}, the later
#' is just involved to add a row in a dataframe.
#' The rv are updated and the graph
#' proxy is also updated. We handle either adding new node
#' or append new node to an existing one. The later case,
#' involves some edge creation (connection). No validation is
#' required in practice as a node can theoretically feed
#' as many children nodes as required.
#'
#' @param rv Local reactive values.
#' @param session Shiny session object.
#' @export
create_node <- function(rv, session) {
  input <- session$input
  ns <- session$ns
  # Construct block with empty defaults
  # TODO: maybe we want to provide more choices
  # but that would require more UI elements
  rv$new_block <- create_block(input$scoutbar)

  # Update node vals for the network rendering
  rv <- add_node(rv$new_block, rv)

  visNetworkProxy(ns("network")) |>
    visUpdateNodes(rv$nodes) |>
    visSelectNodes(id = utils::tail(rv$nodes$id, n = 1))

  # Handle add_block_to where we also setup the connections
  if (rv$append_block) {
    rv <- add_edge(
      from = input$network_selected,
      to = block_uid(rv$new_block),
      label = block_inputs(rv$new_block)[[1]],
      rv = rv
    )

    visNetworkProxy(ns("network")) |>
      visUpdateEdges(rv$edges)
  }
  rv
}
