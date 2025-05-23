#' Initialize a G6 ANTV Network Visualization
#'
#' Creates a G6 ANTV network visualization with pre-configured options for nodes, edges,
#' layout, behaviors, and plugins.
#'
#' @param nodes A data frame or list containing node information. Each node should
#'   typically have at least an `id` field, and optionally other attributes like `label`.
#'   Default is `NULL`.
#' @param edges A data frame or list containing edge information. Each edge typically
#'   needs `source` and `target` fields to define connections. Default is `NULL`.
#' @param ns Session namespace.
#' @details
#' This function initializes a G6 network visualization with several pre-configured features:
#'
#' \itemize{
#'   \item Node styling with background labels
#'   \item Combo (group) support with circular styling
#'   \item Curved edge paths with directional arrows and dashed styling
#'   \item Dagre layout algorithm optimized for directed graphs
#'   \item Interactive behaviors including zoom, drag, selection, and edge creation
#'   \item Plugins for minimap, tooltips, fullscreen mode, context menu, and toolbar
#' }
#'
#' @return A G6 network visualization object that can be further customized or directly
#'   rendered in R Markdown, Shiny, or other R environments.
#' @keywords internal
initialize_g6 <- function(nodes = NULL, edges = NULL, ns) {
  g6(
    nodes = nodes,
    edges = edges
  ) |>
    g6_options(
      animation = FALSE,
      node = list(
        style = list(
          labelBackground = TRUE,
          labelBackgroundRadius = 4,
          labelFontFamily = "Arial",
          labelPadding = c(0, 4),
          labelText = JS(
            "(d) => {
            return d.label
          }"
          )
        )
      ),
      combo = list(
        animation = FALSE,
        type = "circle-combo-with-extra-button",
        style = list(
          labelText = JS(
            "(d) => {
            return d.id
          }"
          )
        )
      ),
      edge = list(
        endArrow = TRUE,
        zIndex = 100,
        style = list(
          lineDash = c(5, 5),
          labelText = JS(
            "(d) => {
            return d.label
          }"
          )
        )
      )
    ) |>
    g6_layout(
      layout = list(
        type = "force"
      )
    ) |>
    g6_behaviors(
      "zoom-canvas",
      "drag-canvas",
      drag_element(),
      click_select(multiple = TRUE),
      brush_select(),
      # avoid conflict with internal function
      g6R::create_edge(
        onFinish = JS(
          sprintf(
            "(edge) => {
              const graph = HTMLWidgets.find('#%s').getWidget();
              const targetType = graph.getElementType(edge.target);
              // Avoid to create edges in combos. If so, we remove it
              if (targetType !== 'node') {
                graph.removeEdgeData([edge.id]);
              } else {
                Shiny.setInputValue('%s', edge);
                // Then we reset the behaviors so there is no conflict
                graph.updateBehavior({
                  key: 'create-edge', // Specify the behavior to update
                  enable: false,
                });
                // Re-enable drag element bahaviors
                graph.updateBehavior({ key: 'drag-element', enable: true });
                graph.updateBehavior({ key: 'drag-element-force', enable: true });
              }
            }",
            ns("network"),
            ns("added_edge")
          )
        )
      )
    ) |>
    g6_plugins(
      #"minimap",
      "tooltip",
      grid_line(),
      fullscreen(),
      # Conditional menu for edge and nodes
      context_menu(
        enable = JS(
          "(e) => { return e.targetType === 'edge' || e.targetType === 'node' }"
        ),
        onClick = JS(
          sprintf(
            "(value, target, current) => {
              const graphId = `${target.closest('.g6').id}`;
              const graph = HTMLWidgets.find(`#${graphId}`).getWidget();
              if (current.id === undefined) return;
              if (value === 'create_edge') {
                graph.updateBehavior({
                  key: 'create-edge', // Specify the behavior to update
                  enable: true,
                });
                // Select node
                graph.setElementState(current.id, 'selected');
                // Disable drag node as it is incompatible with edge creation
                graph.updateBehavior({ key: 'drag-element', enable: false });
                graph.updateBehavior({ key: 'drag-element-force', enable: false });
              } else if (value === 'remove_node') {
                // Send message to R so we can modify the
                // graph from R and not from JS
                Shiny.setInputValue('%s', current.id)
              } else if (value === 'remove_edge') {
                // Needed to destroy the link since edge id can't be edited
                // so the link ID is stored in the data attributes.
                Shiny.setInputValue('%s', graph.getEdgeData(current.id).data.linkId);
                graph.removeEdgeData([current.id]);
                graph.draw();
              } else if (value === 'append_node') {
                Shiny.setInputValue('%s', true, {priority: 'event'})
              }
            }",
            ns("removed_node"),
            ns("removed_edge"),
            ns("append_node")
          )
        ),
        getItems = JS(
          "(e) => {
            if (e.targetType === 'node') {
              return [
                { name: 'Create edge', value: 'create_edge' },
                { name: 'Append node', value: 'append_node' },
                { name: 'Remove node', value: 'remove_node' }
              ];
            } else if (e.targetType === 'edge') {
              return [
                { name: 'Remove edge', value: 'remove_edge' }
              ];
            }
          }"
        )
      ),
      g6R::toolbar(
        position = "left",
        onClick = JS(
          sprintf(
            "( value, target, current ) => {   
                // Handle button click events
              const graph = HTMLWidgets.find(`#${target.closest('.g6').id}`).getWidget();
              const fullScreen = graph.getPluginInstance('fullscreen');
              const zoomLevel = graph.getZoom();
                if ( value === 'zoom-in' ) {   
                  graph.zoomTo (graph.getZoom() + 0.1);
                } else if ( value === 'zoom-out' ) {     
                  graph.zoomTo (graph.getZoom() - 0.1);
                } else if ( value === 'auto-fit' ) {     
                  graph.fitView ( ) ;
                } else if (value === 'delete') {
                  const selectedNodes = graph.getElementDataByState('node', 'selected').map(
                    (node) => {
                    return node.id
                    }
                  );
                  // Send message R so we can modify the network from R
                  // and not JS.
                  Shiny.setInputValue('%s', selectedNodes);
                } else if (value === 'request-fullscreen') {
                  if (fullScreen !== undefined) {
                    fullScreen.request();
                  }
                } else if (value === 'exit-fullscreen') {
                  if (fullScreen !== undefined) {
                    fullScreen.exit();
                  }
                }
              }
            ",
            ns("removed_node")
          )
        )
      )
    )
}

#' Create a new node it to the network
#'
#' The rv are updated and the graph
#' proxy is also updated. We handle either adding new node
#' or append new node to an existing one. The later case,
#' involves some edge creation (connection). No validation is
#' required in practice as a node can theoretically feed
#' as many children nodes as required.
#'
#' @param new New block to add. A block object.
#' @param vals Global reactive values. To communicate between modules.
#' @param rv Board reactive values. Read-only.
#' @param validate Whether to register validation observers for the node.
#' Default to TRUE.
#' @param obs Observer list.
#' @param session Shiny session object.
#' @keywords internal
create_g6_node <- function(new, vals, rv, validate = TRUE, obs, session) {
  stopifnot(
    is_block(new)
  )
  input <- session$input
  ns <- session$ns

  new_node <- list(
    id = block_uid(new),
    label = paste(
      attr(new, "class")[1],
      "\n id:",
      block_uid(new)
    ),
    style = list(
      labelBackgroundFill = "#a0cafa"
    )
  )

  # Select new, unselect old
  if (is.null(input[["network-selected_node"]])) {
    to_select <- setNames(list("selected"), block_uid(new))
  } else {
    to_select <- setNames(
      list("selected", ""),
      c(block_uid(new), input[["network-selected_node"]])
    )
  }

  g6_proxy(ns("network")) |>
    g6_add_nodes(list(new_node)) |>
    g6_set_nodes(to_select)

  if (isTRUE(vals$append_block)) {
    create_g6_edge(
      new = list(
        source = input[["network-selected_node"]],
        target = block_uid(new)
      ),
      vals,
      rv,
      session
    )
  }

  # Handle node update. Change of color due to block validity change ...
  # This needs input parameter from the parent module which contains
  # the list of block server functions.
  if (validate) {
    register_g6_node_validation(
      block_uid(new),
      rv,
      vals,
      session
    )
  }

  vals
}

#' Remove a node
#'
#' Remove node from g6 instance
#'
#' @param selected UID (character string) of node to remove.
#' @param vals Global reactive values.
#' @param session Shiny session object.
#' @keywords internal
remove_g6_node <- function(selected, vals, session) {
  stopifnot(
    is.character(selected),
    nchar(selected) > 0
  )

  ns <- session$ns

  # clear node selection + remove
  g6_proxy(ns("network")) |>
    g6_set_nodes(setNames(list(""), selected)) |>
    g6_remove_nodes(ids = selected)

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
remove_g6_edge <- function(selected, vals, session) {
  stopifnot(
    is.character(selected),
    nchar(selected) > 0
  )

  ns <- session$ns

  vals$removed_edge <- selected

  g6_proxy(ns("network")) |>
    g6_remove_edges(ids = vals$removed_edge)

  vals
}

#' Remove a node and associated edges
#'
#' Combine \link{remove_node} with \link{remove_edge}.
#'
#' @param selected UID (character string) of node to remove.
#' @param vals Reactive values with dataframe representing nodes data.
#' @param rv Board reactive values. Read-only.
#' @param session Shiny session object.
#' @keywords internal
cleanup_g6_node <- function(selected, vals, rv, session) {
  remove_g6_node(selected, vals, session)
  # Need to cleanup any edge associated with this node
  edges <- as.data.frame(board_links(rv$board))
  if (nrow(edges) > 0) {
    # loop over all edges where the target node is part
    edges_to_remove <- c(
      edges[
        edges$from == selected,
        "id"
      ],
      edges[
        edges$to == selected,
        "id"
      ]
    )
    for (edge in edges_to_remove) {
      remove_g6_edge(edge, vals, session)
    }
  }
  vals
}


#' Create an edge and add it to the network
#'
#' There is a validation layer prior to knowing whether
#' we can add the edge. Then rv are updated and the graph
#' proxy is also updated.
#'
#' @param new Edge data. A list like
#' \code{list(source = "from_node_ID", target = "to_node_ID")}.
#' @param vals Global reactive values. Read-write.
#' @param rv Board reactive values. Read-only.
#' @param session Shiny session object.
#' @keywords internal
create_g6_edge <- function(new, vals, rv, session) {
  ns <- session$ns
  stopifnot(is.list(new))

  if (!validate_edge_creation(new$target, rv)) {
    # remove edge when it was created from DND
    g6_proxy(ns("network")) |>
      g6_remove_edges(ids = new$id)

    # Cleanup node when it was created from Append block
    if (vals$append_block) {
      remove_g6_node(new$target, vals, session)
      # send callback to add/rm block plugin
      vals$cancelled_edge <- new$target
      # Re-select source node
      g6_proxy(ns("network")) |>
        g6_set_nodes(setNames(list("selected"), new$source))
    }
    stop()
  }

  to_blk <- rv$blocks[[new$target]]$block

  new_edge <- list(
    source = new$source,
    target = new$target,
    label = define_conlabel(to_blk, new$target, rv)
  )

  # For links
  vals$added_edge <- as_links(
    new_link(
      from = new_edge$source,
      to = new_edge$target,
      input = new_edge$label
    )
  )

  # Ensure we get the link id to be able to
  # remove links laters ...
  new_edge$data$linkId <- vals$added_edge$id

  # If edge is created via create_node. If create via
  # DND, we don't need to draw it as it is already done via JS
  if (!length(new$id)) {
    # Create the connection
    g6_proxy(ns("network")) |>
      g6_add_edges(list(new_edge))
  } else {
    # Update edge id by the link ID since edge was created from JS
    # This will be useful when we want to delete the edge from the JS
    # side and then destroy the link from R
    g6_proxy(ns("network")) |>
      g6_update_edges(
        list(
          list(
            id = new$id,
            data = list(linkId = vals$added_edge$id)
          )
        )
      )
  }

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
register_g6_node_validation <- function(id, rv, vals, session) {
  # We don't need to store the observers
  # as we need them again while restoring
  # a previous state where a removed block was
  observeEvent(
    {
      req(
        length(board_block_ids(rv$board)) > 0,
        id %in% board_block_ids(rv$board)
      )
      # For Nicolas: why does board$msgs() triggers infinitely?
      rv$msgs()[[id]]
    },
    {
      apply_g6_validation(id, vals, rv, session)
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
#' @rdname node-validation
#' @keywords internal
apply_g6_validation <- function(id, vals, rv, session) {
  message <- rv$msgs()[[id]]
  ns <- session$ns
  # Restore blue color if valid
  edges <- as.data.frame(board_links(rv$board))
  connected_edges <- edges[edges$from == id, ]

  if (is.null(message)) {
    # Reset node defaults
    node_config <- list(
      list(
        id = id,
        style = list(
          fill = "#1783FF",
          labelBackgroundFill = "#a0cafa",
          badges = list()
        )
      )
    )

    # Reset connected edges
    if (nrow(connected_edges) > 0) {
      ids <- paste(connected_edges$from, connected_edges$to, sep = "-")
      new_edges <- lapply(ids, \(id) {
        list(
          id = id,
          type = "fly-marker-cubic",
          style = list(
            stroke = "#000",
            badgeText = NULL
          ),
          states = list()
        )
      })
      g6_proxy(ns("network")) |>
        g6_update_edges(new_edges)
    }
  }

  # Color invalid nodes in red
  if (
    length(message$state$error) ||
      length(message$data$error) ||
      length(message$eval$error)
  ) {
    state_badge <- list()
    if (length(message$state$error)) {
      state_badge <- list(
        text = sprintf("State errors: '%s'", length(message$state$error)),
        placement = "right-top",
        backgroundFill = "#ee705c"
      )
    }

    data_badge <- list()
    if (length(message$data$error)) {
      data_badge <- list(
        text = sprintf("Data errors: '%s'", length(message$date$error)),
        placement = "right",
        backgroundFill = "#edb528"
      )
    }

    eval_badge <- list()
    if (length(message$eval$error)) {
      eval_badge <- list(
        text = sprintf("Eval errors: '%s'", length(message$eval$error)),
        placement = "right-bottom",
        backgroundFill = "#85847e"
      )
    }

    node_config <- list(
      list(
        id = id,
        style = list(
          fill = "#ee705c",
          labelBackgroundFill = "#FFB6C1",
          badges = list(
            state_badge,
            data_badge,
            eval_badge
          )
        )
      )
    )

    # Style connected edges
    if (nrow(connected_edges) > 0) {
      ids <- paste(connected_edges$from, connected_edges$to, sep = "-")
      new_edges <- lapply(ids, \(id) {
        list(
          id = id,
          type = "cubic",
          style = list(
            stroke = "#ee705c",
            badgeText = "🛑",
            badgeBackground = FALSE
          ),
          animation = list(),
          states = list("inactive")
        )
      })
      g6_proxy(ns("network")) |>
        g6_update_edges(new_edges)
    }
  }

  g6_proxy(ns("network")) |>
    g6_update_nodes(node_config)
}
