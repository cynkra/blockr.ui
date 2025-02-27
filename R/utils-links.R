#' Add a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param new New block to add.
#' @param vals Local reactive values.
#' @keywords internal
add_node <- function(new, vals) {
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
    title = NA,
    shape = "dot",
    shapeProperties.borderDashes = NA,
    color.background = "#dbebff",
    color.border = "#dbebff",
    color.highlight.background = "#dbebff",
    color.highlight.border = "#dbebff",
    borderWidth = 4,
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
#' @param id Edge id. If create_link is FALSE, id cannot be not NULL.
#' @param from Node id.
#' @param to Node it.
#' @param label Edge label. This is useful to map the existing connected
#' nodes to the input slots of the receiving node (for instance a join block).
#' @param vals Global vals reactive values. Read-write access.
#' @param create_link Create a link in the board?
#' @keywords internal
add_edge <- function(id = NULL, from, to, label, vals, create_link = TRUE) {
  stopifnot(
    is.character(from),
    is.character(to),
    is.data.frame(vals$edges)
  )
  if (
    isTRUE(create_link) && !is.null(id) || isFALSE(create_link) && is.null(id)
  ) {
    stop(
      "When create_link is TRUE, id must be NULL.
      When create_link is FALSE, id can't be NULL."
    )
  }

  edge_data <- data.frame(
    from = from,
    to = to,
    label = label,
    arrows.to.enabled = TRUE,
    arrows.to.type = "arrow",
    width = 2,
    color = "#9db5cc",
    dashes = FALSE
  )

  if (!is.null(id)) {
    edge_data <- cbind(id = id, edge_data)
  }

  # Create link
  if (create_link) {
    vals$added_edge <- as_links(
      new_link(
        from = edge_data$from,
        to = edge_data$to,
        input = edge_data$label
      )
    )
    # Add link id for edge id to be able to remove it later ...
    edge_data$id <- vals$added_edge$id
  }

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
#' @param vals Reactive values with dataframe representing nodes data.
#' @param session Shiny session object.
#' @keywords internal
remove_node <- function(selected, vals, session) {
  stopifnot(
    is.data.frame(vals$nodes),
    nrow(vals$nodes) > 0,
    is.character(selected),
    nchar(selected) > 0
  )

  ns <- session$ns

  to_remove <- which(vals$nodes$id == selected)
  if (length(to_remove) == 0) {
    stop(sprintf("Can't find node with id %s in the data", selected))
  }

  visNetworkProxy(ns("network"), session = session) |>
    visRemoveNodes(selected)

  vals$nodes <- vals$nodes[-to_remove, ]
  vals
}

#' Remove a node and associated edges
#'
#' Combine \link{remove_node} with \link{remove_edge}.
#'
#' @param selected UID (character string) of node to remove.
#' @param vals Reactive values with dataframe representing nodes data.
#' @param session Shiny session object.
#' @keywords internal
cleanup_node <- function(selected, vals, session) {
  remove_node(selected, vals, session)
  # Need to cleanup any edge associated with this node
  if (nrow(vals$edges) > 0) {
    # loop over all edges where the target node is part
    edges_to_remove <- c(
      vals$edges[
        vals$edges$from == selected,
        "id"
      ],
      vals$edges[
        vals$edges$to == selected,
        "id"
      ]
    )
    for (edge in edges_to_remove) {
      remove_edge(edge, vals, session)
    }
  }
  vals
}

#' Remove an edge
#'
#' Update dataframe for visNetwork graph
#'
#' @param selected UID (character string) of edge to remove.
#' @param vals Reactive values containing dataframe representing edges data.
#' @param session Shiny session object.
#' @keywords internal
remove_edge <- function(selected, vals, session) {
  stopifnot(
    is.data.frame(vals$edges),
    nrow(vals$edges) > 0,
    is.character(selected),
    nchar(selected) > 0
  )
  to_remove <- grep(selected, vals$edges$id)
  if (length(to_remove) == 0) {
    stop(sprintf("Can't find edge with id %s in the data", selected))
  }

  ns <- session$ns

  vals$removed_edge <- vals$edges[to_remove, "id"]
  vals$edges <- vals$edges[-to_remove, ]

  visNetworkProxy(ns("network"), session = session) |>
    visRemoveEdges(vals$removed_edge)

  vals
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

#' @export
check_connections.rbind_block <- function(x, target, rv) {
  TRUE
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

#' Define connection label
#'
#' @param x Block object.
#' @param target Connection target id.
#' @param rv Reactive values
#' @export
define_conlabel <- function(x, target, rv) {
  UseMethod("define_conlabel")
}

#' @export
define_conlabel.block <- function(x, target, rv) {
  con_idx <- which(list_empty_connections(x, target, rv) == TRUE)[[1]]
  block_inputs(x)[[con_idx]]
}

#' @export
define_conlabel.rbind_block <- function(x, target, rv) {
  links <- names(rv$inputs[[target]]$...args)
  res <- if (!length(links)) 1 else length(links) + 1
  as.character(res)
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
#' @param vals Global reactive values. Read-write.
#' @param rv Board reactive values. Read-only.
#' @param session Shiny session object.
#' @export
create_edge <- function(new, vals, rv, session) {
  ns <- session$ns
  stopifnot(is.list(new))

  if (!validate_edge_creation(new$to, rv)) {
    if (vals$append_block) {
      remove_node(new$to, vals, session)
      vals$cancelled_edge <- new$to
    }
    stop()
  }

  to_blk <- rv$blocks[[new$to]]$block

  # Create the connection
  add_edge(
    from = new$from,
    to = new$to,
    # The connection is be made with the latest available input slot
    label = define_conlabel(to_blk, new$to, rv),
    vals = vals
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
#' @param rv Global vals reactive values.
#' @param session Shiny session object.
#' @export
create_node <- function(new, vals, rv, session) {
  input <- session$input
  ns <- session$ns

  # Update node vals for the network rendering
  add_node(new, vals)
  # Handle add_block_to where we also setup the connections
  if (isTRUE(vals$append_block)) {
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

#' Register block validation observer
#'
#' For each block we register an observer that
#' captures only the messages related to this block validation
#' status. The observer is destroyed when the node is cleaned
#' by cleanup_node.
#'
#' @param vals Global reactive values. Read-write.
#' @param rv Global reactive values. Read-only.
#' @param session Shiny session object.
#' @export
register_node_validation <- function(vals, rv, session) {
  id <- block_uid(vals$added_block)
  # We don't need to store the observers
  # as we need them again while restoring
  # a previous state where a removed block was
  observeEvent(
    {
      req(
        length(board_block_ids(rv$board)) > 0,
        nrow(vals$nodes) == length(board_block_ids(rv$board)),
        id %in% board_block_ids(rv$board)
      )
      rv$msgs()[[id]]
    },
    {
      apply_validation(rv$msgs()[[id]], id, vals, session)
    },
    ignoreNULL = FALSE
  )
}

#' Apply block validation to network elements
#'
#' Block validation is made by the backend
#' this function only updates the node color
#' based on the valid status.
#'
#' @param message Message.
#' @param id Node id.
#' @param vals Global reactive values. Read-write.
#' @param session Shiny session object.
#' @export
apply_validation <- function(message, id, vals, session) {
  ns <- session$ns
  # Restore blue color if valid
  selected_color <- vals$nodes[vals$nodes$id == id, "color.border"]
  connected_edges <- vals$edges[vals$edges$to == id, ]

  if (is.null(message)) {
    if (selected_color == "#dbebff") return(NULL)
    if (nrow(connected_edges) > 0) {
      vals$edges[vals$edges$to == id, "color"] <- "#9db5cc"
      vals$edges[vals$edges$to == id, "dashes"] <- FALSE
      vals$edges[vals$edges$to == id, "arrows.to.type"] <- "arrow"

      visNetworkProxy(ns("network")) |>
        visUpdateEdges(vals$edges)
    }
    vals$nodes[vals$nodes$id == id, "color.border"] <- "#dbebff"
    vals$nodes[vals$nodes$id == id, "color.highlight.border"] <- "#dbebff"
    vals$nodes[vals$nodes$id == id, "shapeProperties.borderDashes"] <- FALSE
    vals$nodes[
      vals$nodes$id == id,
      "title"
    ] <- "State errors: 0 <br> Data errors: 0 <br> Eval errors: 0"
  }

  # Color invalid nodes in red
  if (
    length(message$state$error) ||
      length(message$data$error) ||
      length(message$eval$error)
  ) {
    if (selected_color == "#ff0018") return(NULL)
    # Any linked edge would also get styled
    if (nrow(connected_edges) > 0) {
      vals$edges[vals$edges$to == id, "color"] <- "#ff0018"
      vals$edges[vals$edges$to == id, "dashes"] <- TRUE
      vals$edges[vals$edges$to == id, "arrows.to.type"] <- "image"
      vals$edges[
        vals$edges$to == id,
        "arrows.to.src"
      ] <- "www/images/cross-sign.svg"
      vals$edges[
        vals$edges$to == id,
        "arrows.to.imageWidth"
      ] <- 15
      vals$edges[
        vals$edges$to == id,
        "arrows.to.imageHeight"
      ] <- 15

      visNetworkProxy(ns("network")) |>
        visUpdateEdges(vals$edges)
    }
    vals$nodes[vals$nodes$id == id, "color.border"] <- "#ff0018"
    vals$nodes[vals$nodes$id == id, "color.highlight.border"] <- "#ff0018"
    vals$nodes[vals$nodes$id == id, "shapeProperties.borderDashes"] <- TRUE
    vals$nodes[vals$nodes$id == id, "title"] <- sprintf(
      "State errors: %s <br> Data errors: %s <br> Eval errors: %s",
      if (length(message$state$error)) message$state$error else "0.",
      if (length(message$data$error)) message$data$error else "0.",
      if (length(message$eval$error)) message$eval$error else "0."
    )
  }

  visNetworkProxy(ns("network")) |>
    visUpdateNodes(vals$nodes)
}

#' Build configuration parameter list
#'
#' Used by all network utilities for default options
#'
#' @param func Function applied.
#' @param defaults List of default parameters.
#' @param ... Extra parameters not in defaults accepted by func.
#' @keywords internal
default_network <- function(func, defaults, ...) {
  stopifnot(is.list(defaults))
  pars <- list(...)
  if (length(pars) == 1) pars <- unlist(pars)

  incorrect_parm <- which(
    !(names(pars) %in% names(formals(func))[-1])
  )

  if (length(incorrect_parm)) {
    stop(
      paste0(
        "Params {",
        paste(names(pars)[incorrect_parm], collapse = ", "),
        "} are not part of ",
        deparse(substitute(func)),
        " API parameters."
      )
    )
  }

  duplicated <- which(names(pars) %in% names(defaults))

  if (length(duplicated)) {
    stop(
      paste0(
        "Params {",
        paste(names(pars)[duplicated], collapse = ", "),
        "} are duplicated"
      )
    )
  }

  c(
    defaults,
    pars
  )
}

#' Default network interactions
#'
#' What interaction to support
#'
#' @param ... Extra parameters not in defaults accepted by \link[visNetwork]{visInteraction}.
#' @keywords internal
default_network_interactions <- function(...) {
  default_network(
    visInteraction,
    list(
      hover = FALSE,
      multiselect = TRUE,
      # avoid to select edge when selecting node ...
      # since we have a select edge callback
      selectConnectedEdges = FALSE
    ),
    ...
  )
}

#' Default network options
#'
#' What options to support
#'
#' @param ... Extra parameters not in defaults accepted by \link[visNetwork]{visOptions}.
#' @keywords internal
default_network_options <- function(...) {
  default_network(
    visOptions,
    list(
      # To get currently selected node
      nodesIdSelection = TRUE,
      manipulation = list(
        enabled = TRUE,
        initiallyActive = TRUE,
        addNode = FALSE,
        deleteNode = FALSE,
        deleteEdge = FALSE,
        editEdge = FALSE
      ),
      collapse = TRUE
    ),
    ...
  )
}

#' Default edges options
#'
#' What options to support
#'
#' @param ... Extra parameters not in defaults accepted by \link[visNetwork]{visEdges}.
#' @keywords internal
default_edges_options <- function(...) {
  default_network(
    visEdges,
    list(
      length = 300,
      smooth = FALSE
    ),
    ...
  )
}

#' Default physics options
#'
#' What options to support
#'
#' @param ... Extra parameters not in defaults accepted by \link[visNetwork]{visPhysics}.
#' @keywords internal
default_network_physics <- function(...) {
  default_network(
    visPhysics,
    list(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -500,
        damping = 1,
        avoidOverlap = 1
      )
    ),
    ...
  )
}

#' Default events options
#'
#' What options to support
#'
#' @param ... Extra parameters not in defaults accepted by \link[visNetwork]{visEvents}.
#' @keywords internal
default_network_events <- function(ns, ...) {
  default_network(
    visEvents,
    list(
      # within the event 'this' refers to the network instance
      select = sprintf(
        "function(e) {
          if (e.nodes.length > 1) {
            Shiny.setInputValue('%s', e.nodes, {priority: 'event'});
          }
        }",
        ns("selected_nodes")
      ),
      stabilized = sprintf(
        "function(e) {
          Shiny.setInputValue('%s', true, {priority: 'event'});
        }",
        ns("stabilized")
      ),
      oncontext = sprintf(
        "function(e) {
          e.event.preventDefault(); // avoid showing web inspector ...
          Shiny.setInputValue('%s', e.nodes, {priority: 'event'});
        }",
        ns("node_right_clicked")
      ),
      selectEdge = sprintf(
        "function(e) {
          Shiny.setInputValue('%s', e.edges[0], {priority: 'event'});
        }",
        ns("selected_edge")
      ),
      controlNodeDragEnd = sprintf(
        "function(e) {
          Shiny.setInputValue('%s', e.controlEdge, {priority: 'event'});
          let target = $(`.${e.event.target.offsetvals.className}`)
            .closest('.visNetwork')
            .attr('id');
          // Re-enable add edge mode
          setTimeout(() => {
            window.HTMLWidgets.find(`#${target}`).network.addEdgeMode();
          }, 500);
        ;}",
        ns("new_edge")
      ) #,
      #hoverNode = sprintf(
      #  "function(e) {
      #  Shiny.setInputValue('%s', e.node, {priority: 'event'});
      #;}",
      #  ns("hovered_node")
      #)#,
      #blurNode = sprintf(
      #  "function(e) {
      #  Shiny.setInputValue('%s', e.node, {priority: 'event'});
      #;}",
      #  ns("hovered_node")
      #)
    ),
    ...
  )
}

#' Create network widget
#'
#' Network is populated via a proxy. This function initialises an empty
#' network with the right setup.
#'
#' @param ns Namespace.
#' @param height Network height.
#' @param width Network width.
#' @param interactions See \link{default_network_interactions}.
#' @param options See \link{default_network_options}.
#' @param edges See \link{default_edges_options}.
#' @param physics See \link{default_network_physics}.
#' @param events See \link{default_network_events}.
#'
#' @return A visNetwork object.
#' @keywords internal
create_network_widget <- function(
  ns,
  height = "100vh",
  width = "100%",
  interactions = default_network_interactions(),
  options = default_network_options(),
  edges = default_edges_options(),
  physics = default_network_physics(),
  events = default_network_events(ns)
) {
  vis_network <- visNetwork(
    data.frame(),
    data.frame(),
    height = height,
    width = width
  )

  vis_network <- do.call(
    visInteraction,
    c(graph = quote(vis_network), interactions)
  )

  vis_network <- do.call(visOptions, c(graph = quote(vis_network), options))
  vis_network <- do.call(visEdges, c(graph = quote(vis_network), edges))
  vis_network <- do.call(visPhysics, c(graph = quote(vis_network), physics))
  vis_network <- do.call(visEvents, c(graph = quote(vis_network), events))

  vis_network
}

#' Restore network from saved snapshot
#'
#' Network is updated via a proxy.
#'
#' @param links Board links.
#' @param vals Global vals reactive values. Read-write access.
#' @param session Shiny session object
#'
#' @return A reactiveValues object.
#' @keywords internal
restore_network <- function(links, vals, session) {
  ns <- session$ns
  # Cleanup old setup
  if (length(session$input$network_nodes)) {
    visNetworkProxy(ns("network")) |>
      visRemoveNodes(names(session$input$network_nodes))
  }

  if (nrow(vals$edges) > 0) {
    vals$edges <- data.frame()
  }

  # Restore nodes
  visNetworkProxy(ns("network")) |>
    visUpdateNodes(vals$nodes)

  if (!is.null(vals$selected_block)) {
    visNetworkProxy(ns("network")) |>
      visSelectNodes(id = vals$selected_block)
  }

  # For each link re-creates the edges
  lapply(names(links), \(nme) {
    link <- as.data.frame(links[[nme]])
    add_edge(
      id = nme,
      link$from,
      link$to,
      link$input,
      vals,
      create_link = FALSE
    )
  })
  visNetworkProxy(ns("network")) |>
    visUpdateEdges(vals$edges)

  vals$refreshed <- "network"

  vals
}

#' Create and show node menu
#'
#' Node menu contains shortcut to some actions
#' like adding a node to a grid...
#'
#' @param value Initial state of the grid switch. Depends
#' on the value set in the grid vals module.
#' @param session Shiny session object
#'
#' @keywords internal
show_node_menu <- function(value, session) {
  ns <- session$ns
  input <- session$input
  session$sendCustomMessage(
    "show-node-menu",
    list(
      id = ns(input$node_right_clicked),
      ns = ns(""),
      value = value,
      coords = input$mouse_location
    )
  )
}

#' Register observers related to the node menu
#'
#' Observer to maintain the state between the 2 switches +
#' an observer to handle serialisation/restoration, to remove
#' nodes and to append to a given node.
#'
#' @param blocks_ids Board block ids.
#' @param vals Global scope reactive values.
#' @param obs Plugin observers list.
#' @param session Shiny session object.
#'
#' @keywords internal
register_node_menu_obs <- function(blocks_ids, vals, obs, session) {
  input <- session$input

  lapply(blocks_ids, \(id) {
    if (is.null(obs[[sprintf("%s-add_to_grid", id)]])) {
      # Send callback to grid module to maintain the grid switch state
      obs[[sprintf("%s-add_to_grid", id)]] <- observeEvent(
        input[[sprintf("%s-add_to_grid", id)]],
        {
          vals$in_grid[[id]] <- input[[
            sprintf("%s-add_to_grid", id)
          ]]
        }
      )

      # Receive callback from grid to maintain the block option
      # card switch
      obs[[sprintf("update-%s-add_to_grid", id)]] <- observeEvent(
        vals$in_grid[[id]],
        {
          update_switch(
            sprintf("%s-add_to_grid", id),
            value = vals$in_grid[[id]]
          )
        }
      )

      # Update the in_grid switch inputs to handle serialisation
      # restoration
      obs[[sprintf("restore-%s-add_to_grid", id)]] <- observeEvent(
        req(vals$refreshed == "grid"),
        {
          update_switch(
            sprintf("%s-add_to_grid", id),
            value = vals$in_grid[[id]]
          )
        }
      )

      # Remove node and block
      obs[[sprintf("%s-remove_block", id)]] <- observeEvent(
        input[[sprintf("%s-remove_block", id)]],
        {
          # Avoid triggering too many times (wait until next flush cycle)
          freezeReactiveValue(input, sprintf("%s-remove_block", id))
          vals$removed_block <- id
        }
      )

      # Append block
      obs[[sprintf("%s-append_block", id)]] <- observeEvent(
        input[[sprintf("%s-append_block", id)]],
        {
          # Avoid triggering too many times (wait until next flush cycle)
          freezeReactiveValue(input, sprintf("%s-append_block", id))
          if (isFALSE(vals$append_block)) vals$append_block <- TRUE
        }
      )
    }
  })
}
