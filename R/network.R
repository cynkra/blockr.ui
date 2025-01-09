#' @rdname board
#' @export
network_ui <- function(id, debug = TRUE) {
  ns <- NS(id)
  # Note: semantic ordering so that
  # elements can be used in the board in different places.
  list(
    action_bar = tagList(
      actionButton(
        ns("add_block"),
        "New block",
        icon = icon("circle-plus"),
        class = "btn-light"
      ),
      scoutbar(
        ns("scoutbar"),
        placeholder = "Search for a block",
        actions = blk_choices()
      )
    ),
    sidebar = div(
      class = "btn-group",
      role = "group",
      actionButton(
        ns("append_block"),
        "Append block",
        icon = icon("circle-plus"),
        class = "btn-light"
      ),
      uiOutput(ns("data_source_ui")),
      actionButton(
        ns("remove"),
        "Remove block",
        icon = icon("trash"),
        class = "btn-light"
      )
    ),
    canvas = tagList(
      visNetworkOutput(ns("network")),
      if (debug) {
        list(
          h4("Edges"),
          verbatimTextOutput(ns("edges")),
          h4("Nodes"),
          verbatimTextOutput(ns("nodes"))
        )
      }
    )
  )
}

#' Add a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param new New block to insert. Must be a valid
#' block registry entry
#' @param nodes dataframe representing network data.
#' @keywords internal
add_node <- function(new, nodes) {
  stopifnot(
    is_block(new),
    is.data.frame(nodes)
  )

  node_data <- data.frame(
    id = block_uid(new),
    label = attr(new, "class")[1],
    # color = "red",
    title = block_uid(new),
    shape = "circle",
    stack = NA,
    icon.code = NA
  )

  if (nrow(nodes) == 0) {
    node_data
  } else {
    rbind(
      nodes,
      node_data
    )
  }
}

#' Add a edge
#'
#' Update dataframe for visNetwork graph
#'
#' @param from Node id.
#' @param to Node it.
#' @param label Edge label. This is useful to map the existing connected
#' nodes to the input slots of the receiving node (for instance a join block).
#' @param edges dataframe representing network data.
#' @keywords internal
add_edge <- function(from, to, label, edges) {
  stopifnot(
    is.character(from),
    is.character(to),
    is.data.frame(edges)
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

  if (nrow(edges) == 0) {
    list(res = edge_data, added = id)
  } else {
    list(
      res = rbind(
        edges,
        edge_data
      ),
      added = id
    )
  }
}

#' Remove a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param selected UID (character string) of node to remove.
#' @param nodes dataframe representing nodes data.
#' @keywords internal
remove_node <- function(selected, nodes) {
  stopifnot(
    is.data.frame(nodes),
    nrow(nodes) > 0,
    is.character(selected),
    nchar(selected) > 0
  )

  to_remove <- which(nodes$id == selected)
  if (length(to_remove) == 0) {
    stop(sprintf("Can't find node with id %s in the data", selected))
  }
  nodes[-to_remove, ]
}

#' Remove an edge
#'
#' Update dataframe for visNetwork graph
#'
#' @param selected UID (character string) of edge to remove.
#' @param edges dataframe representing edges data.
#' @keywords internal
remove_edge <- function(selected, edges) {
  stopifnot(
    is.data.frame(edges),
    nrow(edges) > 0,
    is.character(selected),
    nchar(selected) > 0
  )
  to_remove <- grep(selected, edges$id)
  if (length(to_remove) == 0) {
    stop(sprintf("Can't find edge with id %s in the data", selected))
  }

  list(
    res = edges[-to_remove, ],
    removed = to_remove
  )
}

#' Check node connection
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
check_connections.transform_block <- function(x, vals) {
  n_active_connections <- sum(
    !lgl_ply(vals$connections[[block_uid(x)]], \(slot) is.null(slot()))
  )
  isTRUE(n_active_connections < length(block_inputs(x)))
}

#' @rdname board
#' @param vals Reactive values of parent module.
#' @param debug Print extra information about the network
#' @export
network_server <- function(id, vals, debug = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      edges = data.frame(),
      nodes = data.frame(),
      new_block = NULL,
      append_block = FALSE,
      added_edge = character(),
      removed_edge = character()
    )

    output$network <- renderVisNetwork({
      # Initialized as empty, we'll update with the proxy
      visNetwork(
        data.frame(),
        data.frame(),
        height = "100vh",
        width = "100%"
      ) |>
        visInteraction(hover = FALSE, multiselect = TRUE) |>
        visOptions(
          # To get currently selected node
          nodesIdSelection = TRUE,
          manipulation = TRUE,
          collapse = TRUE
        ) |>
        visEdges(length = 200) |>
        visEvents(
          oncontext = sprintf(
            "function(e) {
              e.event.preventDefault(); // avoid showing web inspector ...
              Shiny.onInputChange('%s', e.nodes, {priority: 'event'});
            }",
            ns("node_right_clicked")
          ),
          hoverNode = sprintf(
            "function(e) {
            Shiny.onInputChange('%s', e.node, {priority: 'event'});
          ;}",
            ns("hovered_node")
          ),
          #blurNode = sprintf(
          #  "function(e) {
          #  Shiny.onInputChange('%s', e.node, {priority: 'event'});
          #;}",
          #  ns("hovered_node")
          #)
        )
    })

    #observeEvent(input$hovered_node, {
    #  showNotification(input$hovered_node)
    #})

    observeEvent(input$node_right_clicked, {
      showNotification(input$node_right_clicked)
    })

    if (debug) {
      output$edges <- renderPrint(rv$edges)
      output$nodes <- renderPrint(rv$nodes)
    }

    # Trigger add block
    observeEvent(input$add_block, {
      rv$new_block <- NULL
      # Reset add_block_to
      rv$append_block <- FALSE
      update_scoutbar(
        session,
        "scoutbar",
        revealScoutbar = TRUE
      )
    })

    # TBD: implement add_block_to -> add a block after the selected one
    # We need a contextual registry and update the scoutbar with relevant
    # choices. I think we can use the same scoutbar as for the classic
    # add block with all choices.
    observeEvent(input$append_block, {
      rv$new_block <- NULL
      rv$append_block <- TRUE
      update_scoutbar(
        session,
        "scoutbar",
        revealScoutbar = TRUE
      )
    })

    # Adding a block, we update the rv$nodes so the graph is updated
    observeEvent(input$scoutbar, {
      # Construct block with empty defaults
      # TODO: maybe we want to provide more choices
      # but that would require more UI elements
      rv$new_block <- create_block(input$scoutbar)

      # Update node vals for the network rendering
      rv$nodes <- add_node(rv$new_block, rv$nodes)

      visNetworkProxy(ns("network")) |>
        visUpdateNodes(rv$nodes) |>
        visSelectNodes(id = tail(rv$nodes$id, n = 1))

      # Handle add_block_to where we also setup the connections
      if (rv$append_block) {
        edges <- add_edge(
          from = input$network_selected,
          to = block_uid(rv$new_block),
          label = block_inputs(rv$new_block)[[1]],
          edges = rv$edges
        )
        rv$added_edge <- edges$added
        rv$edges <- edges$res

        visNetworkProxy(ns("network")) |>
          visUpdateEdges(rv$edges)
      }
    })

    # Selected block object
    selected_block <- reactive({
      req(
        nchar(input$network_selected) > 0,
        input$network_selected %in% rv$nodes$id
      )
      vals$blocks[[input$network_selected]]$block
    })

    # Returns other nodes ids, without the selected one
    # This is useful to prevent to connect the node to itself
    other_nodes <- reactive({
      req(
        nchar(input$network_selected) > 0,
        input$network_selected %in% rv$nodes$id
      )
      rv$nodes[rv$nodes$id != input$network_selected, ]
    })

    # Connect block to existing blocks.
    # We only show it if the node can be connected,
    # that is, check that the number of connections don't exceed
    # the number of input slots of the given block
    can_update_data_sources <- reactive({
      req(
        nchar(input$network_selected) > 0,
        input$network_selected %in% rv$nodes$id
      )
      # Check that node number is enough (excluding the current node from the total)
      n_nodes <- length(rv$nodes[rv$nodes$id != input$network_selected, "id"])
      req(n_nodes >= length(block_inputs(selected_block())))
      check_connections(selected_block(), vals)
    })

    output$data_source_ui <- renderUI({
      req(can_update_data_sources())
      actionButton(
        ns("manage_sources"),
        "Data sources",
        icon = icon("share-nodes"),
        class = "btn-light"
      )
    })

    # TODO: create S3 method that does display contextual menu
    # based on the current block selection inputs slot.
    # The join block would allow to select 2 nodes, while the select block only 1.
    # This only works if the block does not have existing connections.
    observeEvent(input$manage_sources, {
      connect_ui <- lapply(block_inputs(selected_block()), \(slot) {
        selectInput(
          ns(sprintf("%s-%s_input_slot", input$network_selected, slot)),
          sprintf("%s input slot", slot),
          # TO DO: subset choices to avoid the existing connections and the current block
          choices = setNames(
            other_nodes()$id,
            paste(other_nodes()$label, other_nodes()$id)
          )
        )
      })

      # TODO: we don't necessarily need a modal, this UI could be visible at all time
      # in the sidebar...
      showModal(
        modalDialog(
          size = "s",
          title = sprintf(
            "Manage node %s connections",
            block_uid(selected_block())
          ),
          connect_ui,
          actionButton(
            ns("update_sources"),
            "Update"
          )
        )
      )
    })

    # Draw new edges
    observeEvent(input$update_sources, {
      lapply(block_inputs(selected_block()), \(slot) {
        edges <- add_edge(
          from = input[[
            sprintf("%s-%s_input_slot", input$network_selected, slot)
          ]],
          to = input$network_selected,
          label = slot,
          rv$edges
        )
        if (!(edges$added %in% rv$edges$id)) {
          rv$edges <- edges$res
          rv$added_edge <- c(rv$added_edge, edges$added)
        }
      })

      visNetworkProxy(ns("network")) |>
        visUpdateEdges(rv$edges)

      removeModal()
    })

    # TODO: handle node update. Change of color due to block validity change ...
    # This needs input parameter from the parent module which contains
    # the list of block server functions.

    # Remove a block
    # TODO: how do we handle multi block removal?
    observeEvent(input$remove, {
      rv$nodes <- remove_node(input$network_selected, rv$nodes)
      if (nrow(rv$edges) > 0) {
        tryCatch(
          {
            edges <- remove_edge(input$network_selected, rv$edges)
            rv$removed_edge <- rv$edges[edges$removed, "id"]
            rv$edges <- edges$res
          },
          error = function(e) {
            message(
              sprintf("Node %s had no edge to remove", input$network_selected)
            )
          }
        )
      }
      visNetworkProxy(ns("network")) |>
        visRemoveNodes(input$network_selected)
    })

    # TODO: implement edge creation
    # This is different from add_to. Ideally we can drag from one node
    # to another to connect them.

    # TODO: implement remove edge (user selects the edge).

    # The network module must return the edge representation
    # since they are created from the network and needed in other parts,
    # as well as the selected items such as current node and/or edge.
    return(
      list(
        edges = reactive(rv$edges),
        nodes = reactive(rv$nodes),
        selected_node = reactive(input$network_selected),
        added_node = reactive(rv$new_block),
        removed_node = eventReactive(input$remove, {
          # Contains the UID of the block module to remove from
          # the board
          input$network_selected
        }),
        # TODO: send added edges to board module to update the connections
        added_edge = reactive(rv$added_edge),
        removed_edge = reactive(rv$removed_edge)
      )
    )
  })
}
