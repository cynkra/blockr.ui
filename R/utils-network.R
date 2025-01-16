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
    stack = NA,
    icon.code = NA
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

  rv$removed_edge <- to_remove
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
