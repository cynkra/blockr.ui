#' Add a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param new New block to add.
#' @param vals Local reactive values.
#' @param rv Global reactive values.
#' @keywords internal
add_node <- function(new, vals, rv) {
  stopifnot(
    is_block(new),
    is.data.frame(vals$nodes)
  )

  node_data <- data.frame(
    id = block_uid(new),
    label = paste(
      attr(new, "class")[1],
      "\n id:",
      block_uid(new)
    ),
    title = block_uid(new),
    shape = "circle",
    color = "#D2E5FF",
    stack = NA,
    icon.code = NA,
    x = NA,
    y = NA
  )

  vals$nodes <- if (nrow(vals$nodes) == 0) {
    node_data
  } else {
    rbind(
      vals$nodes,
      node_data
    )
  }
  vals
}

#' Add a edge
#'
#' Update dataframe for visNetwork graph
#'
#' @param from Node id.
#' @param to Node it.
#' @param label Edge label. This is useful to map the existing connected
#' nodes to the input slots of the receiving node (for instance a join block).
#' @param vals Reactive values, containing elements such as edges data.
#' @keywords internal
add_edge <- function(from, to, label, vals) {
  stopifnot(
    is.character(from),
    is.character(to),
    is.data.frame(vals$edges)
  )

  edge_data <- data.frame(
    from = from,
    to = to,
    label = label,
    arrows = "to",
    width = 4
  )

  # Create link
  vals$added_edge <- as_links(
    new_link(
      from = edge_data$from,
      to = edge_data$to,
      input = edge_data$label
    )
  )

  # Add link id for edge id to be able to remove it later ...
  edge_data$id <- vals$added_edge$id

  if (nrow(vals$edges) == 0) {
    vals$edges <- edge_data
  } else {
    vals$edges <- rbind(
      vals$edges,
      edge_data
    )
  }

  vals
}

#' Remove a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param selected UID (character string) of node to remove.
#' @param rv Reactive values with dataframe representing nodes data.
#' @keywords internal
remove_node <- function(selected, rv, session) {
  stopifnot(
    is.data.frame(rv$nodes),
    nrow(rv$nodes) > 0,
    is.character(selected),
    nchar(selected) > 0
  )

  ns <- session$ns

  to_remove <- which(rv$nodes$id == selected)
  if (length(to_remove) == 0) {
    stop(sprintf("Can't find node with id %s in the data", selected))
  }

  visNetworkProxy(ns("network")) |>
    visRemoveNodes(selected)

  rv$nodes <- rv$nodes[-to_remove, ]
  rv
}

#' Remove an edge
#'
#' Update dataframe for visNetwork graph
#'
#' @param selected UID (character string) of edge to remove.
#' @param rv Reactive values containing dataframe representing edges data.
#' @param session Shiny session object.
#' @keywords internal
remove_edge <- function(selected, rv, session) {
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

  ns <- session$ns

  rv$removed_edge <- rv$edges[to_remove, "id"]
  rv$edges <- rv$edges[-to_remove, ]

  visNetworkProxy(ns("network")) |>
    visRemoveEdges(rv$removed_edge)

  rv
}

#' List node connections
#'
#' @param x block.
#' @param target Connection target id.
#' @param rv Reactive values.
#'
list_empty_connections <- function(x, target, rv) {
  UseMethod("list_empty_connections", x)
}

#' @export
list_empty_connections.data_block <- function(x, target, rv) {
  NULL
}

#' @export
list_empty_connections.block <- function(x, target, rv) {
  lgl_ply(rv$inputs[[target]], \(slot) is.null(slot()))
}

#' Check node connection
#'
#' @param x block.
#' @param target Connection target id.
#' @param rv Reactive values.
#'
#' @export
check_connections <- function(x, target, rv) {
  UseMethod("check_connections", x)
}

#' @export
check_connections.data_block <- function(x, target, rv) {
  FALSE
}

#' @export
check_connections.block <- function(x, target, rv) {
  n_active_connections <- sum(!list_empty_connections(x, target, rv))
  isTRUE(n_active_connections < length(block_inputs(x)))
}

#' Check whether the node can receive connection
#'
#' @param x Block object.
#' @param target Connection target id.
#' @param rv Reactive values
#' @export
can_connect <- function(x, target, rv) {
  UseMethod("can_connect")
}

#' @export
can_connect.data_block <- function(x, target, rv) {
  showNotification(
    "Data blocks don't accept any incoming connection (data input).",
    type = "error"
  )
  FALSE
}

#' @export
can_connect.block <- function(x, target, rv) {
  if (!check_connections(x, target, rv)) {
    showNotification(
      "The target block can't receive anymore data input.",
      type = "error"
    )
    FALSE
  } else {
    TRUE
  }
}

validate_edge_creation <- function(target, rv) {
  check_cons <- can_connect(
    rv$blocks[[target]]$block,
    target,
    rv
  )
  if (!check_cons) return(FALSE)

  return(TRUE)
}

#' Create an edge and add it to the network
#'
#' This is different from \link{add_edge}, the later
#' is just involved to add a row in a dataframe.
#' There is a validation layer prior to knowing whether
#' we can add the edge. Then rv are updated and the graph
#' proxy is also updated.
#'
#' @param new Edge data. A list like
#' \code{list(from = "from_node_ID", to = "to_node_ID")}.
#' @param vals Local reactive values.
#' @param rv Parent reactive values.
#' @param session Shiny session object.
#' @export
create_edge <- function(new, vals, rv, session) {
  ns <- session$ns
  stopifnot(is.list(new))

  if (!validate_edge_creation(new$to, rv)) {
    if (rv$append_block) {
      remove_node(new$to, vals, session)
      rv$cancelled_edge <- new$to
    }
    stop()
  }

  to_blk <- rv$blocks[[new$to]]$block
  con_idx <- which(list_empty_connections(to_blk, new$to, rv) == TRUE)[[1]]

  # Create the connection
  add_edge(
    from = new$from,
    to = new$to,
    # The connection is be made with the latest available input slot
    label = block_inputs(to_blk)[[con_idx]],
    vals
  )

  visNetworkProxy(ns("network")) |>
    visUpdateEdges(vals$edges)
  vals
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
#' @param new New block to add.
#' @param vals Local reactive values.
#' @param rv Global reactive values.
#' @param session Shiny session object.
#' @export
create_node <- function(new, vals, rv, session) {
  input <- session$input
  ns <- session$ns

  # Update node vals for the network rendering
  add_node(new, vals, rv)
  # Handle add_block_to where we also setup the connections
  if (rv$append_block) {
    create_edge(
      new = list(
        from = input$network_selected,
        to = block_uid(new)
      ),
      vals,
      rv,
      session
    )
  }

  visNetworkProxy(ns("network")) |>
    visUpdateNodes(vals$nodes) |>
    visSelectNodes(id = utils::tail(vals$nodes$id, n = 1))
  vals
}
