#' Add a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param new New block to add.
#' @param vals Global reactive values. To communicate between modules
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
    group = NA,
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
    arrows.to.src = NA,
    arrows.to.imageWidth = NA,
    arrows.to.imageHeight = NA,
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

  # Unselect all nodes
  session$sendCustomMessage(
    "reset-node-selection",
    list(id = sprintf("#%s", ns("network")))
  )
  vals$selected_block <- NULL

  # Cleanup any associated stack
  # If this node was the last member of an existing stack

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
#' @param rv Board reactive values.
#' @export
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
#' @param rv Board reactive values.
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

#' @export
check_connections.llm_plot_block <- function(x, target, rv) {
  TRUE
}

#' @export
check_connections.llm_transform_block <- function(x, target, rv) {
  TRUE
}

#' Check whether the node can receive connection
#'
#' @param x Block object.
#' @param target Connection target id.
#' @param rv Board reactive values
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

#' Validate edge creation for a block
#'
#' @param rv Board reactive values. Read-only.
#' @param target Target block id
#' @keywords internal
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
#' @param rv Board reactive values
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

#' @export
define_conlabel.llm_plot_block <- define_conlabel.rbind_block

#' @export
define_conlabel.llm_transform_block <- define_conlabel.rbind_block

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
#' @keywords internal
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
#' @param vals Global reactive values. To communicate between modules.
#' @param rv Board reactive values. Read-only.
#' @param session Shiny session object.
#' @keywords internal
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

    from_node <- vals$nodes[vals$nodes$id == input$network_selected, ]
    if (!is.na(from_node[["group"]])) {
      add_node_to_stack(
        block_uid(new),
        from_node[["group"]],
        from_node[["color"]],
        vals
      )
    }
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
#' status.
#'
#' @param id Block id for which to register the validation.
#' @param rv Board reactive values. Read-only.
#' @param vals Global reactive values. Read-write.
#' @param session Shiny session object.
#' @rdname node-validation
#' @keywords internal
register_node_validation <- function(id, rv, vals, session) {
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
#' @rdname node-validation
#' @keywords internal
apply_validation <- function(message, id, vals, session) {
  ns <- session$ns
  # Restore blue color if valid
  selected_color <- vals$nodes[vals$nodes$id == id, "color.border"]
  connected_edges <- vals$edges[vals$edges$to == id, ]

  if (is.null(message)) {
    if (selected_color != "#ff0018") return(NULL)
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
          let target = $(`.${e.event.target.offsetParent.className}`)
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
#' @param rv Board internal reactive values. Read-only
#' @param vals Global vals reactive values. Read-write access.
#' @param session Shiny session object
#'
#' @return A reactiveValues object.
#' @keywords internal
restore_network <- function(rv, vals, session) {
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
  links <- board_links(rv$board)
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

  # Re apply node validation
  lapply(
    vals$nodes$id,
    register_node_validation,
    vals = vals,
    rv = rv,
    session = session
  )

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
#' @param stacks Stack ids.
#' @param has_stack Whether the current block belongs to a stack.
#' @param session Shiny session object
#'
#' @keywords internal
show_node_menu <- function(value, stacks, has_stack, session) {
  ns <- session$ns
  input <- session$input
  session$sendCustomMessage(
    "show-node-menu",
    list(
      id = ns(input$node_right_clicked),
      ns = ns(""),
      value = value,
      stacks = as.list(stacks),
      has_stack = has_stack,
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
#' @param vals Local scope reactive values.
#' @param parent Global scope reactive values.
#' @param obs Plugin observers list.
#' @param session Shiny session object.
#'
#' @keywords internal
register_node_menu_obs <- function(blocks_ids, vals, parent, obs, session) {
  input <- session$input

  lapply(blocks_ids, \(id) {
    if (is.null(obs[[sprintf("%s-add_to_grid", id)]])) {
      # Send callback to grid module to maintain the grid switch state
      obs[[sprintf("%s-add_to_grid", id)]] <- observeEvent(
        input[[sprintf("%s-add_to_grid", id)]],
        {
          parent$in_grid[[id]] <- input[[
            sprintf("%s-add_to_grid", id)
          ]]
        }
      )

      # Receive callback from grid to maintain the block option
      # card switch
      obs[[sprintf("update-%s-add_to_grid", id)]] <- observeEvent(
        parent$in_grid[[id]],
        {
          update_switch(
            sprintf("%s-add_to_grid", id),
            value = parent$in_grid[[id]]
          )
        }
      )

      # Update the in_grid switch inputs to handle serialisation
      # restoration
      obs[[sprintf("restore-%s-add_to_grid", id)]] <- observeEvent(
        req(parent$refreshed == "grid"),
        {
          update_switch(
            sprintf("%s-add_to_grid", id),
            value = parent$in_grid[[id]]
          )
        }
      )

      # Remove node and block
      obs[[sprintf("%s-remove_block", id)]] <- observeEvent(
        input[[sprintf("%s-remove_block", id)]],
        {
          # Avoid triggering too many times (wait until next flush cycle)
          freezeReactiveValue(input, sprintf("%s-remove_block", id))
          parent$removed_block <- id
        }
      )

      # Append block
      obs[[sprintf("%s-append_block", id)]] <- observeEvent(
        input[[sprintf("%s-append_block", id)]],
        {
          # Avoid triggering too many times (wait until next flush cycle)
          freezeReactiveValue(input, sprintf("%s-append_block", id))
          if (isFALSE(parent$append_block)) parent$append_block <- TRUE
        }
      )

      # Remove from stack
      obs[[sprintf("%s-remove_from_stack", id)]] <- observeEvent(
        input[[sprintf("%s-remove_from_stack", id)]],
        {
          # Avoid triggering too many times (wait until next flush cycle)
          freezeReactiveValue(input, sprintf("%s-remove_from_stack", id))
          remove_node_from_stack(id, parent, standalone = TRUE, session)
        }
      )

      # Add to stack
      obs[[sprintf("%s-add_to_stack", id)]] <- observeEvent(
        input[[sprintf("%s-add_to_stack", id)]],
        {
          # Avoid triggering too many times (wait until next flush cycle)
          freezeReactiveValue(input, sprintf("%s-add_to_stack", id))
          # TBD: user should be able to choose any stack within a menu
          add_node_to_stack(
            id,
            input[[sprintf("%s-add_to_stack_selected", id)]],
            attr(vals$stacks, "palette")[which(
              vals$stacks == input[[sprintf("%s-add_to_stack_selected", id)]]
            )],
            parent,
            standalone = TRUE,
            session
          )
        }
      )
    }
  })
}

#' Dynamically group nodes
#'
#' Given a set of selected nodes, add them to a unique group
#' and apply unique color and labels.
#'
#' This function must be called after \link{trigger_create_stack}.
#'
#' @param vals Local scope (links module) reactive values.
#' @param rv Board reactive values.
#' @param parent Global scope (entire app) reactive values.
#' @param session Shiny session object.
#'
#' @keywords internal
stack_nodes <- function(vals, rv, parent, session) {
  ns <- session$ns
  input <- session$input

  stack_id <- tail(board_stack_ids(rv$board), n = 1)

  vals$stacks[[length(vals$stacks) + 1]] <- stack_id
  stack_color <- attr(vals$stacks, "palette")[length(vals$stacks)]
  lapply(input$selected_nodes, \(id) {
    add_node_to_stack(id, stack_id, stack_color, parent, session = session)
  })

  # Unselect all nodes
  session$sendCustomMessage(
    "reset-node-selection",
    list(id = sprintf("#%s", ns("network")))
  )
  parent$selected_block <- NULL

  parent
}

#' Dynamically ungroup nodes
#'
#' Given a set of selected nodes,
#' ungroup them by removing the associated stack.
#'
#' This function must be called after \link{trigger_remove_stack}.
#'
#' @param vals Local scope (links module) reactive values.
#' @param rv Board reactive values.
#' @param parent Global scope (entire app) reactive values.
#' @param session Shiny session object.
#'
#' @keywords internal
unstack_nodes <- function(vals, rv, parent, session) {
  ns <- session$ns
  input <- session$input

  stack_id <- parent$removed_stack
  nodes_to_reset <- parent$nodes[parent$nodes$group == stack_id, ]

  for (row in seq_len(nrow(nodes_to_reset))) {
    tmp <- nodes_to_reset[row, ]
    remove_node_from_stack(tmp$id, parent, session = session)
  }

  vals$stacks[[stack_id]] <- NULL

  # Unselect all nodes
  session$sendCustomMessage(
    "reset-node-selection",
    list(id = sprintf("#%s", ns("network")))
  )
  parent$selected_block <- NULL

  parent
}

#' Add node to a group
#'
#' Given a node, update its data to add it to a group (color, group, ...)
#'
#' @param id Node id.
#' @param stack_id Stack id.
#' @param color Stack color.
#' @param parent Global reactive values to update data.
#' @param standalone Whether this function is called directly or from
#' \link{stack_nodes}.
#' @param session Shiny session object.
#'
#' @keywords internal
add_node_to_stack <- function(
  id,
  stack_id,
  color,
  parent,
  standalone = FALSE,
  session
) {
  parent$nodes[parent$nodes$id == id, "group"] <- stack_id
  parent$nodes[parent$nodes$id == id, "color.background"] <- color
  parent$nodes[parent$nodes$id == id, "color.border"] <- color
  parent$nodes[
    parent$nodes$id == id,
    "color.highlight.background"
  ] <- color
  parent$nodes[
    parent$nodes$id == id,
    "color.highlight.border"
  ] <- color
  parent$nodes[parent$nodes$id == id, "label"] <- paste(
    parent$nodes[parent$nodes$id == id, "label"],
    sprintf("\n Stack: %s", stack_id)
  )

  if (standalone)
    parent$stack_added_node <- list(
      node_id = id,
      stack_id = stack_id
    )

  # There is a conflict between group and visGetNodes():
  # https://github.com/datastorm-open/visNetwork/issues/429
  visNetworkProxy(session$ns("network")) |>
    visUpdateNodes(parent$nodes)

  parent
}

#' Remove node from group
#'
#' Given a node, update its data to remove it from a group (color, group, ...)
#'
#' @param id Node id.
#' @param parent Global reactive values to update data.
#' @param standalone Whether this function is called directly or from
#' \link{unstack_nodes}.
#' @param session Shiny session object.
#'
#' @keywords internal
remove_node_from_stack <- function(id, parent, standalone = FALSE, session) {
  # Reset to factory
  stack_id <- parent$nodes[parent$nodes$id == id, "group"]
  if (is.na(stack_id)) {
    showNotification(
      sprintf("Error: block %s does not belong to any stack.", id),
      type = "error"
    )
    return(NULL)
  }

  parent$nodes[parent$nodes$id == id, "group"] <- NA
  # TBD: this is also used in the validation color
  # Maybe we can create a helper for this ...
  parent$nodes[parent$nodes$id == id, "color.border"] <- "#dbebff"
  parent$nodes[parent$nodes$id == id, "color.background"] <- "#dbebff"
  parent$nodes[parent$nodes$id == id, "color.highlight.border"] <- "#dbebff"
  parent$nodes[parent$nodes$id == id, "color.highlight.background"] <- "#dbebff"
  parent$nodes[parent$nodes$id == id, "label"] <- gsub(
    "\n Stack.*",
    "",
    parent$nodes[parent$nodes$id == id, "label"]
  )

  if (standalone) parent$stack_removed_node <- stack_id

  # There is a conflict between group and visGetNodes():
  # https://github.com/datastorm-open/visNetwork/issues/429
  visNetworkProxy(session$ns("network")) |>
    visUpdateNodes(parent$nodes)

  parent
}

can_remove_stack <- function(selected, parent) {
  stack_id <- parent$nodes[parent$nodes$id == selected, "group"]
  if (is.na(stack_id)) FALSE else TRUE
}

can_create_stack <- function(selected, parent) {
  has_stack <- parent$nodes[parent$nodes$id %in% selected, "group"]
  if (any(!is.na(has_stack))) FALSE else TRUE
}

#' Signal for stack removal
#'
#' Given a selected node, find whether a stack can be removed.
#' If successful, then the parent$removed_stack will be captured
#' in the stacks plugin.
#'
#' @param selected Selected node id.
#' @param parent Global reactive values to update data.
#'
#' @keywords internal
trigger_remove_stack <- function(selected, parent) {
  if (!can_remove_stack(selected, parent)) {
    showNotification(
      sprintf("Error: block %s does not belong to any stack.", selected),
      type = "error"
    )
    return(NULL)
  }
  parent$removed_stack <- parent$nodes[
    parent$nodes$id == selected,
    "group"
  ]
  parent
}

#' Signal for stack creation
#'
#' Given a selected node, find whether a stack can be created.
#' If succesful, the stack is created from the stacks plugin.
#'
#' @param selected Selected node id.
#' @param parent Global reactive values to update data.
#'
#' @keywords internal
trigger_create_stack <- function(selected, parent) {
  if (!can_create_stack(selected, parent)) {
    showNotification(
      sprintf(
        "Error: blocks %s are already bound to a stack.",
        paste(selected, collapse = ", ")
      ),
      type = "error"
    )
    return(NULL)
  }
  parent$added_stack <- selected
  parent
}

#' Show stack actions
#'
#' A modal window triggered when multiple nodes are selected, for instance.
#' So far we support adding a stack and removing multiple blocks from a stack.
#'
#' @param selected Set of selected nodes.
#' @param parent Global reactive values.
#' @param session Shiny session object.
#'
#' @keywords internal
show_stack_actions <- function(selected, parent, session) {
  ns <- session$ns

  showModal(
    modalDialog(
      title = "Node multi action",
      size = "s",
      div(
        class = "d-grid gap-2 mx-auto",
        role = "group",
        actionButton(
          ns("new_stack"),
          "New stack",
          icon = icon("object-group")
        ),
        actionButton(
          ns("remove_stack"),
          "Remove stack",
          icon = icon("object-ungroup")
        ),
        actionButton(
          ns("remove_blocks"),
          "Remove selected",
          icon = icon("trash")
        )
      )
    )
  )
}
