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
  lgl_ply(rv$inputs[[target]], \(inp) is.null(inp()))
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
check_connections.llm_block <- function(x, target, rv) {
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
  if (!check_cons) {
    return(FALSE)
  }

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
define_conlabel.llm_block <- function(x, target, rv) {
  if (is.null(rv$inputs[[target]]$data())) {
    return("data")
  }
  links <- names(rv$inputs[[target]]$...args)
  res <- if (!length(links)) 1 else length(links) + 1
  as.character(res)
}

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
initialize_g6 <- function(nodes = NULL, edges = NULL, ns, path, context_menu) {
  g6(
    nodes = nodes,
    edges = edges
  ) |>
    default_g6_options() |>
    g6_layout() |>
    default_g6_behaviors(ns = ns) |>
    default_g6_plugins(ns = ns, path = path, context_menu = context_menu)
}

#' Default g6 network options
#' @param graph g6 network instance.
#' @param ... Extra option parameters.
#' @param ns Module namespace.
#' @keywords internal
#' @rdname default-g6
default_g6_options <- function(graph, ...) {
  if (!inherits(graph, "g6")) {
    stop("default_g6_options must be called on a g6 instance")
  }

  graph |>
    g6_options(
      ...,
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
          return `Stack: ${d.label}`
        }"
          )
        )
      ),
      edge = list(
        style = list(
          endArrow = TRUE,
          lineDash = c(5, 5),
          labelText = JS(
            "(d) => {
          return d.label
        }"
          )
        )
      )
    )
}

#' @rdname default-g6
#' @keywords internal
default_g6_behaviors <- function(graph, ..., ns) {
  if (!inherits(graph, "g6")) {
    stop("default_g6_options must be called on a g6 instance")
  }

  graph |>
    g6_behaviors(
      ...,
      "zoom-canvas",
      "drag-canvas",
      # So we can add node to stack from the UI by drag and drop
      drag_element(dropEffect = "link"),
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
    )
}

#' @rdname default-g6
#' @keywords internal
default_g6_plugins <- function(graph, ..., ns, path, context_menu) {
  if (!inherits(graph, "g6")) {
    stop("default_g6_options must be called on a g6 instance")
  }

  graph |>
    g6_plugins(
      ...,
      #"minimap",
      "tooltip",
      #grid_line(),
      fullscreen(),
      # Conditional menu for edge and nodes
      context_menu(
        enable = JS(
          "(e) => { 
          let cond = e.targetType === 'edge' || 
            e.targetType === 'node' || 
            e.targetType === 'canvas' || 
            e.targetType === 'combo';
          return cond;
          }"
        ),
        # nolint start
        onClick = JS(
          context_menu_entry_js(context_menu, ns)
        ),
        # nolint end
        getItems = JS(
          paste0(
            "async (e) => {\n",
            "  const response = await fetch(\n",
            "    '",
            path,
            "',\n",
            "    {\n",
            "      method: 'POST',\n",
            "      headers: {\n",
            "        'Accept': 'application/json',\n",
            "        'Content-Type': 'application/json'\n",
            "      },\n",
            "      body: JSON.stringify(\n",
            "        {\n",
            "          id: e.target.id,\n",
            "          type: e.targetType\n",
            "        }\n",
            "      )\n",
            "    }\n",
            "  );\n",
            "  const items = await response.json();\n",
            "  return items;\n",
            "}"
          )
        )
      ),
      g6R::toolbar(
        style = list(
          backgroundColor = "#f5f5f5",
          padding = "8px",
          boxShadow = "0 2px 8px rgba(0, 0, 0, 0.15)",
          borderRadius = "8px",
          border = "1px solid #e8e8e8",
          opacity = "0.9",
          marginTop = "12px",
          marginLeft = "12px"
        ),
        position = "left",
        getItems = JS(
          "( ) => [   
            { id : 'zoom-in' , value : 'zoom-in' },  
            { id : 'zoom-out' , value : 'zoom-out' },   
            { id : 'auto-fit' , value : 'auto-fit' },
            { id: 'delete', value: 'delete' },
            { id: 'icon-roundaddfill', value : 'add-block'},
            { id: 'icon-down_light', value : 'save-board'},
            { id: 'icon-upload', value : 'browse-snapshots'},
            { id: 'icon-text', value : 'show-code'}
          ]"
        ),
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
              } else if (value === 'add-block') {
                Shiny.setInputValue('%s', true, {priority: 'event'});
              } else if (value === 'save-board') {
                Shiny.setInputValue('%s', true, {priority: 'event'})
              } else if (value === 'browse-snapshots') {
                Shiny.setInputValue('%s', true, {priority: 'event'})
              } else if (value === 'show-code') {
                Shiny.setInputValue('%s', true, {priority: 'event'})
              }
            }
          ",
            ns("remove_node"),
            ns("add_block"),
            ns("save_board"),
            ns("browse_snapshots"),
            ns("show_code")
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
#' @param session Shiny session object.
#' @keywords internal
create_node <- function(new, vals, rv, validate = TRUE, session) {
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
    create_edge(
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
    register_node_validation(
      block_uid(new),
      rv,
      vals,
      session
    )
  }

  # Register add to stack/remove from stack behavior
  register_node_stack_link(block_uid(new), rv, vals, session)

  g6_proxy(ns("network")) |>
    g6_fit_center()

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
remove_node <- function(selected, vals, session) {
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
remove_edge <- function(selected, vals, session) {
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
cleanup_node <- function(selected, vals, rv, session) {
  remove_node(selected, vals, session)
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
      remove_edge(edge, vals, session)
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
create_edge <- function(new, vals, rv, session) {
  ns <- session$ns
  stopifnot(is.list(new))

  if (!validate_edge_creation(new$target, rv)) {
    # remove edge when it was created from DND
    g6_proxy(ns("network")) |>
      g6_remove_edges(ids = new$id)

    # Cleanup node when it was created from Append block
    if (vals$append_block) {
      remove_node(new$target, vals, session)
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
    type = "fly-marker-cubic",
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

  # Replace edge id by link id
  new_edge$id <- vals$added_edge$id
  # If edge is created via create_node. If create via
  # DND, we don't need to draw it as it is already done via JS
  if (!length(new$id)) {
    # Create the connection
    g6_proxy(ns("network")) |>
      g6_add_edges(list(new_edge))
  } else {
    # Remove old edge since edge was created from JS with wrong id
    # This will be useful when we want to delete the edge from the JS
    # side and then destroy the link from R. Also add it the correct
    # type to support data marker animation
    g6_proxy(ns("network")) |>
      g6_remove_edges(new$id) |>
      g6_add_edges(list(new_edge))
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
register_node_validation <- function(id, rv, vals, session) {
  # We don't need to store the observers
  # as we need them again while restoring
  # a previous state where a removed block was
  observeEvent(
    {
      req(
        length(board_block_ids(rv$board)) > 0,
        # Don't trigger if node is removed
        id %in% board_block_ids(rv$board)
      )
      # For Nicolas: why does board$msgs() triggers infinitely?
      rv$msgs()[[id]]
    },
    {
      apply_validation(id, vals, rv, session)
    },
    ignoreNULL = FALSE
  )
}

#' Register block stack bind/unbind
#'
#' For each block we register an observer that
#' captures only the messages related to this block validation
#' status.
#'
#' @param id Block id for which to register the validation.
#' @param rv Board reactive values. Read-only.
#' @param vals Global reactive values. Read-write.
#' @param session Shiny session object.
#' @keywords internal
register_node_stack_link <- function(id, rv, vals, session) {
  ns <- session$ns
  input <- session$input

  # Order state retrieval for target node
  observeEvent(
    {
      req(
        input[["network-initialized"]],
        # Don't trigger if node is removed
        id %in% board_block_ids(rv$board)
      )
      input[["network-state"]]
    },
    {
      g6_proxy(ns("network")) |> g6_get_nodes(id)
    }
  )

  # Perform actions on state change
  observeEvent(
    {
      input[[sprintf("network-%s-state", id)]]$combo
    },
    {
      req(length(board_stacks(rv$board)) > 0)
      # Ensure the state is up to date
      # before triggering something
      req(
        length(board_stacks(rv$board)) ==
          length(input[["network-state"]]$combos)
      )
      stacks_blocks <- unlist(
        lapply(board_stacks(rv$board), stack_blocks),
        use.names = FALSE
      )
      node_state <- input[[sprintf("network-%s-state", id)]]
      has_stack <- node_state$combo

      # Send feedback to stack module to add the node to existing stack
      if (length(has_stack)) {
        if (id %in% stacks_blocks) {
          return(NULL)
        }
        vals$stack_added_node <- list(
          node_id = id,
          stack_id = strsplit(node_state$combo, "combo-")[[1]][2]
        )
      } else {
        if (!(id %in% stacks_blocks)) {
          return(NULL)
        }
        stack_id <- unlist(lapply(
          names(board_stacks(rv$board)),
          \(nme) {
            stack <- board_stacks(rv$board)[[nme]]
            if (id %in% stack_blocks(stack)) nme
          }
        ))
        if (length(stack_id)) {
          vals$stack_removed_node <- list(
            node_id = id,
            # Fin stack name: can we do better?
            stack_id = stack_id
          )
        }
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
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
apply_validation <- function(id, vals, rv, session) {
  message <- rv$msgs()[[id]]
  ns <- session$ns
  # Restore blue color if valid
  edges <- as.data.frame(board_links(rv$board))
  connected_edges <- edges[edges$from == id, "id"]

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

  if (is.null(message)) {
    # Reset connected edges
    if (length(connected_edges) > 0) {
      new_edges <- lapply(connected_edges, \(id) {
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
        text = sprintf("Data errors: '%s'", length(message$data$error)),
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
    if (length(connected_edges) > 0) {
      new_edges <- lapply(connected_edges, \(id) {
        list(
          id = id,
          type = "cubic",
          style = list(
            stroke = "#ee705c",
            badgeText = "ERROR",
            badgeBackgroundFill = "#ee705c",
            badgeBackground = TRUE
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

#' Show stack actions
#'
#' A modal window triggered when create stack is
#' pressed on canvas right click
#'
#' @param rv Reactive values containing board elements. Read-only.
#' @param session Shiny session object.
#'
#' @keywords internal
show_stack_actions <- function(rv, session) {
  ns <- session$ns

  blk_ids <- board_block_ids(rv$board)
  stacks_nodes <- unlist(
    lapply(board_stacks(rv$board), stack_blocks),
    use.names = FALSE
  )
  blk_ids <- blk_ids[!(blk_ids %in% stacks_nodes)]

  showModal(
    modalDialog(
      title = "New stack",
      size = "m",
      div(
        class = "d-grid gap-2 mx-auto",
        role = "group",
        div(
          class = "d-flex gap-4 align-items-center justify-content-around",
          selectInput(
            ns("new_stack_nodes"),
            "Select nodes (leaving NULL creates an empty stack)",
            choices = setNames(
              blk_ids,
              chr_ply(blk_ids, \(id) {
                paste(attr(board_blocks(rv$board)[[id]], "name"), id)
              })
            ),
            selected = blk_ids[1],
            multiple = TRUE
          ),
          shinyWidgets::colorPickr(
            inputId = ns("stack_color"),
            label = "Pick a color for the stack:",
            hue = FALSE,
            preview = FALSE,
            swatches = board_option("stacks_colors", rv$board),
            theme = "nano",
            position = "right-end",
            useAsButton = TRUE
          )
        )
      ),
      footer = tagList(
        actionButton(
          ns("new_stack"),
          "Confirm",
          icon = icon("object-group")
        ),
        modalButton("Dismiss")
      )
    )
  )
}


#' Dynamically group nodes
#'
#' Given a set of selected nodes, add them to a unique group
#' and apply unique color and labels.
#'
#' @param stack_id Stack id to attach nodes to.
#' @param nodes Vector of node ids to stack.
#' @param rv Board reactive values.
#' @param parent Global scope (entire app) reactive values.
#' @param session Shiny session object.
#'
#' @keywords internal
#' @rdname stack-nodes
stack_nodes <- function(
  stack_id = NULL,
  nodes = NULL,
  rv,
  parent,
  session
) {
  ns <- session$ns
  input <- session$input

  if (is.null(stack_id)) {
    stack_id <- tail(board_stack_ids(rv$board), n = 1)
  }
  # avoid duplicated id with edges
  stack_id <- sprintf("combo-%s", stack_id)

  nodes_to_stack <- nodes
  if (is.null(nodes)) {
    nodes_to_stack <- lapply(input$new_stack_nodes, \(node) {
      list(
        id = node,
        combo = stack_id
      )
    })
  }

  stack_color <- input$stack_color
  if (is.null(stack_color)) {
    colors <- board_option("stacks_colors", rv$board)
    if (length(parent$stacks) == 0) {
      stack_color <- colors[1]
    } else {
      stack_color <- colors[length(parent$stacks) * 5]
    }
  }

  parent$stacks <- c(parent$stacks, stack_id)

  # Update graph
  g6_proxy(ns("network")) |>
    g6_add_combos(
      list(
        list(
          id = stack_id,
          label = strsplit(stack_id, "combo-")[[1]][2],
          style = list(
            stroke = stack_color,
            fill = stack_color,
            fillOpacity = 0.2,
            shadowColor = stack_color,
            collapsedFill = stack_color,
            collapsedStroke = stack_color,
            iconFill = stack_color,
            labelPlacement = "top"
          )
        )
      )
    ) |>
    g6_update_nodes(nodes_to_stack) |>
    g6_fit_center()

  parent
}

#' Unstack g6 nodes
#'
#' Useful when removing a stack.
#'
#' @keywords internal
#' @rdname stack-nodes
unstack_nodes <- function(parent, session) {
  ns <- session$ns
  input <- session$input

  # Send callback to stacks plugin
  stack_id <- input$remove_stack
  parent$removed_stack <- strsplit(stack_id, "combo-")[[1]][2]

  # Update local reactiveValues
  parent$stacks <- parent$stacks[-which(parent$stacks == stack_id)]

  # Send message to network
  # (combos are automatically removed from node state so
  # no need to update nodes)
  g6_proxy(ns("network")) |>
    g6_remove_combos(stack_id)
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

  # Replace all graph data and re-render
  # This does not replace plugins and behaviors ...
  g6_proxy(ns("network")) |>
    g6_set_data(unclass(vals$network)) |>
    g6_fit_center()

  # TBD maybe restore the state of vals$stacks?

  # Re apply node validation
  lapply(
    chr_ply(vals$network$nodes, `[[`, "id"),
    register_node_validation,
    rv = rv,
    vals = vals,
    session = session
  )

  # Register add to stack/remove from stack behavior
  lapply(
    chr_ply(vals$network$nodes, `[[`, "id"),
    register_node_stack_link,
    rv = rv,
    vals = vals,
    session = session
  )

  vals$refreshed <- "network"

  vals
}

#' @rdname cold-start
#' @param links Board links.
create_edges_data_from_links <- function(links) {
  unname(lapply(seq_along(links), \(i) {
    link <- links[[i]]
    list(
      id = names(links)[[i]],
      type = "fly-marker-cubic",
      source = link$from,
      target = link$to,
      label = link$input
    )
  }))
}

#' @rdname cold-start
#' @param blocks Board blocks.
#' @param stacks Board stacks.
#' @keywords internal
create_nodes_data_from_blocks <- function(blocks, stacks) {
  blocks_in_stacks <- lapply(stacks, stack_blocks)

  lapply(seq_along(blocks), \(i) {
    current <- blocks[[i]]
    tmp <- list(
      id = names(blocks)[[i]],
      label = paste(
        attr(current, "class")[1],
        "\n id:",
        names(blocks)[[i]]
      ),
      style = list(
        labelBackgroundFill = "#a0cafa"
      )
    )

    # Find in which stack the node is
    tmp$combo <- unlist(lapply(seq_along(blocks_in_stacks), \(i) {
      stack <- blocks_in_stacks[[i]]
      if (tmp$id %in% stack) {
        sprintf("combo-%s", names(blocks_in_stacks)[[i]])
      }
    }))
    tmp
  })
}

#' @rdname cold-start
#' @param stacks Board stacks.
#' @param colors Stacks colors. Internal.
#' @keywords internal
create_combos_data_from_stacks <- function(
  stacks,
  parent,
  colors
) {
  lapply(seq_along(stacks), \(i) {
    stack_id <- sprintf("combo-%s", names(stacks)[[i]])
    if (length(parent$stacks) == 0) {
      stack_color <- colors[1]
    } else {
      stack_color <- colors[length(parent$stacks) * 5]
    }

    parent$stacks <- c(parent$stacks, stack_id)

    list(
      id = stack_id,
      label = strsplit(stack_id, "combo-")[[1]][2],
      style = list(
        stroke = stack_color,
        fill = stack_color,
        fillOpacity = 0.2,
        shadowColor = stack_color,
        collapsedFill = stack_color,
        collapsedStroke = stack_color,
        iconFill = stack_color,
        labelPlacement = "top"
      )
    )
  })
}

#' Create network data from board
#'
#' That's different from \link{restore_network}, as the
#' latter restore a network from a JSON compatible structure.
#' Here we need to re-create all the JSON from the board blocks,
#' links and stacks.
#'
#' @keywords internal
#' @param rv Board reactive values. Read-only
#' @param parent Global app reactive values.
#' @param session Shiny session
#' @rdname cold-start
cold_start <- function(rv, parent, session) {
  ns <- session$ns
  # Cold start
  links <- board_links(rv$board)
  blocks <- board_blocks(rv$board)
  stacks <- board_stacks(rv$board)

  edges_data <- create_edges_data_from_links(links)
  combos_data <- create_combos_data_from_stacks(
    stacks,
    parent,
    board_option("stacks_colors", rv$board)
  )
  nodes_data <- create_nodes_data_from_blocks(blocks, stacks)

  graph_data <- list(
    nodes = nodes_data,
    edges = edges_data,
    combos = combos_data
  )

  #graph_data <- jsonlite::toJSON(graph_data, pretty = TRUE)
  # Render all data all at once for better performances
  g6_proxy(ns("network")) |>
    g6_set_data(graph_data) |>
    g6_fit_center()

  # Re apply node validation
  lapply(
    names(blocks),
    register_node_validation,
    rv = rv,
    vals = parent,
    session = session
  )

  # Register add to stack/remove from stack behavior
  lapply(
    names(blocks),
    register_node_stack_link,
    rv = rv,
    vals = parent,
    session = session
  )

  parent$refreshed <- "network"
  parent
}
