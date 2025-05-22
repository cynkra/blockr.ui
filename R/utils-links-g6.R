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
          labelBackgroundFill = '#FFB6C1',
          labelBackgroundRadius = 4,
          labelFontFamily = 'Arial',
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
        type = "fly-marker-cubic",
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
      drag_element(),
      click_select(multiple = TRUE),
      brush_select(),
      # avoid conflict with internal function
      g6R::create_edge()
    ) |>
    g6_plugins(
      "minimap",
      "tooltip",
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
                graph.removeNodeData([current.id]);
                graph.draw();
              } else if (value === 'remove_edge') {
                Shiny.setInputValue('%s', current.id)
                graph.removeEdgeData([current.id]);
                graph.draw();
              }
            }",
            ns("removed_edge")
          )
        ),
        getItems = JS(
          "(e) => {
            if (e.targetType === 'node') {
              return [
                { name: 'Create edge', value: 'create_edge' },
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
      g6R::toolbar()
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
    #register_g6_node_validation(
    #  block_uid(new),
    #  rv,
    #  vals,
    #  session
    #)
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

  g6_proxy(ns("network")) |>
    g6_remove_nodes(ids = selected)

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
    if (vals$append_block) {
      remove_g6_node(new$target, vals, session)
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
  new_edge$id <- vals$added_edge$id

  # Create the connection
  g6_proxy(ns("network")) |>
    g6_add_edges(list(new_edge))

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
        nrow(vals$nodes) == length(board_block_ids(rv$board)),
        id %in% board_block_ids(rv$board)
      )
      # For Nicolas: why does board$msgs() triggers infinitely?
      rv$msgs()[[id]]
    },
    {
      apply_g6_validation(rv$msgs()[[id]], id, vals, session)
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
apply_g6_validation <- function(message, id, vals, session) {
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
