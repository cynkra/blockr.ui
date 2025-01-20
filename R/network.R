#' Network UI
#'
#' @param id Module id.
#' @param debug Whether to display debug information.
#' @rdname network
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
      #dropdownButton(
      #  inputId = ns("network_options"),
      #  label = "Network Options",
      #  icon = icon("cogs"),
      #  status = "primary",
      #  circle = FALSE
      #)
      if (debug) {
        accordion(
          id = ns("canvas_debug_accordion"),
          open = FALSE,
          class = "accordion-flush",
          accordion_panel(
            title = "Debug info",
            h4("Edges"),
            verbatimTextOutput(ns("edges")),
            h4("Nodes"),
            verbatimTextOutput(ns("nodes")),
            h4("Events"),
            verbatimTextOutput(ns("graph_events"))
          )
        )
      }
    )
  )
}

#' Network server module
#'
#' @rdname network
#' @param vals Reactive values of parent module.
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
        visInteraction(
          hover = FALSE,
          multiselect = TRUE,
          # avoid to select edge when selecting node ...
          # since we have a select edge callback
          selectConnectedEdges = FALSE
        ) |>
        visOptions(
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
        ) |>
        visEdges(length = 300, smooth = FALSE) |>
        visEvents(
          select = sprintf(
            "function(e) {
              if (e.nodes.length > 1) {
                Shiny.setInputValue('%s', e.nodes, {priority: 'event'});
              }
            }",
            ns("selected_nodes")
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
          #),
          #blurNode = sprintf(
          #  "function(e) {
          #  Shiny.setInputValue('%s', e.node, {priority: 'event'});
          #;}",
          #  ns("hovered_node")
          #)
        ) |>
        #visConfigure(
        #  enabled = TRUE,
        #  filter = list("nodes"),
        #  container = sprintf("dropdown-menu-%s", ns("network_options"))
        #) |>
        visPhysics(
          stabilization = list(
            enabled = TRUE,
            iterations = 1000,
            updateInterval = 100,
            onlyDynamicEdges = FALSE,
            fit = TRUE
          ),
          minVelocity = 0.1, # Minimum velocity before node stops moving
          maxVelocity = 50, # Maximum velocity of nodes
          solver = "forceAtlas2Based",
          timestep = 0.5, # Lower values make movement more precise but slower
          adaptiveTimestep = TRUE
        ) |>
        visEvents(
          type = "once",
          # Code to show connection points with edges
          stabilized = sprintf(
            "function() {
              var network = this;
              
              network.on('afterDrawing', function(ctx) {
                var edges = network.body.edges;
                var nodes = network.body.nodes;
                
                Object.keys(edges).forEach(function(edgeId) {
                  var edge = edges[edgeId];
                  var fromNode = nodes[edge.fromId];
                  var toNode = nodes[edge.toId];
                  
                  // Calculate intersection points
                  if (fromNode && toNode) {
                    // Get positions
                    var fromX = fromNode.x;
                    var fromY = fromNode.y;
                    var toX = toNode.x;
                    var toY = toNode.y;
                    
                    // Calculate angles and distances
                    var angle = Math.atan2(toY - fromY, toX - fromX);
                    var reverseAngle = Math.atan2(fromY - toY, fromX - toX);
                    
                    // Get node radii (using shape.width since shape.radius might not be available)
                    var fromRadius = fromNode.shape.width / 2;
                    var toRadius = toNode.shape.width / 2;
                    
                    // Calculate intersection points
                    var fromIntersectX = fromX + (Math.cos(angle) * fromRadius);
                    var fromIntersectY = fromY + (Math.sin(angle) * fromRadius);
                    var toIntersectX = toX + (Math.cos(reverseAngle) * toRadius);
                    var toIntersectY = toY + (Math.sin(reverseAngle) * toRadius);
                    
                    // Draw connection points at intersection
                    ctx.beginPath();
                    ctx.arc(fromIntersectX, fromIntersectY, 4, 0, 2 * Math.PI);
                    ctx.fillStyle = '#2B7CE9';
                    ctx.fill();
                    ctx.strokeStyle = 'white';
                    ctx.lineWidth = 1;
                    ctx.stroke();
                    
                    ctx.beginPath();
                    ctx.arc(toIntersectX, toIntersectY, 4, 0, 2 * Math.PI);
                    ctx.fillStyle = '#2B7CE9';
                    ctx.fill();
                    ctx.strokeStyle = 'white';
                    ctx.lineWidth = 1;
                    ctx.stroke();
                  }
                });
              });
            }"
          )
        )
    })

    #observeEvent(input$network_selected, {
    #  browser()
    #})

    observeEvent(input$selected_nodes, {
      browser()
    })

    #observeEvent(input$hovered_node, {
    #  showNotification(input$hovered_node)
    #})

    #observeEvent(input$node_right_clicked, {
    #  showNotification(input$node_right_clicked)
    #})

    # Bind shift+e and esc to toggle the add edge mode
    # on keyboard events
    observeEvent(req(input$network_initialized), {
      session$sendCustomMessage(
        "bind-network-keyboard-shortcuts",
        list(id = sprintf("#%s", ns("network")))
      )
    })

    # To capture what happens on the client (modify network data)
    # TO DO: maybe we want to expose callbacks to the R API
    #observeEvent(input$network_graphChange, {
    #  browser()
    #})

    # Tweaks UI so that edge mode appears when it should.
    # Also workaround a bug that always activate editNode by default.
    observeEvent(
      {
        req(input$network_initialized)
        c(rv$nodes, input$network_selected, input$network_graphChange)
      },
      {
        session$sendCustomMessage(
          "toggle-manipulation-ui",
          list(
            value = nrow(rv$nodes) > 1
          )
        )
      }
    )

    # Implement edge creation, we can drag from one node
    # to another to connect them.
    # Validation mechanism to allow connections or not...
    # Rules:
    # - The dragged target must exist.
    # - We can't drag the edge on the current selected node (avoid loops).
    # - A node that has already all input slots connected can't received any incoming connection.
    # data block can't receive input data. Transform block receive
    # as many input data as slot they have (1 for select, 2 for join, ...).
    observeEvent(
      {
        req(input$network_graphChange$cmd == "addEdge")
      },
      {
        # vis.js adds the edge on the client on drag. However,
        # there is no callback to the backend. Since add it via the proxy
        # we need to remove the client one.
        visNetworkProxy(ns("network")) |>
          visRemoveEdges(input$network_graphChange$id)
      }
    )

    observeEvent(
      {
        input$new_edge
        req(input$new_edge$from)
      },
      {
        rv <- create_edge(rv, vals, session)
      }
    )

    # Debug info
    if (debug) {
      output$edges <- renderPrint(rv$edges)
      output$nodes <- renderPrint(rv$nodes)
      output$graph_events <- renderPrint(input$network_graphChange)
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
      rv <- create_node(rv, session)
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
          choices = stats::setNames(
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

    # Capture nodes position for serialization
    observeEvent(req(nrow(rv$nodes) > 0), {
      visNetworkProxy(ns("network")) |>
        visGetPositions()
    })
    observeEvent(input$network_positions, {
      lapply(names(input$network_positions), \(id) {
        rv$nodes[rv$nodes$id == id, "x"] <- input$network_positions[[id]]$x
        rv$nodes[rv$nodes$id == id, "y"] <- input$network_positions[[id]]$y
      })
    })

    # TODO: handle node update. Change of color due to block validity change ...
    # This needs input parameter from the parent module which contains
    # the list of block server functions.
    observeEvent(
      {
        req(length(vals$blocks) > 0)
        lapply(names(vals$blocks), \(nme) {
          check_connections(vals$blocks[[nme]]$block, vals)
        })
      },
      {
        for (nme in names(vals$blocks)) {
          res <- check_connections(vals$blocks[[nme]]$block, vals)
          if (res) {
            rv$nodes[rv$nodes$id == nme, "color"] <- "#ffd6d2"
          } else {
            rv$nodes[rv$nodes$id == nme, "color"] <- "#D2E5FF"
          }
        }
        visNetworkProxy(ns("network")) |>
          visUpdateNodes(rv$nodes)
      }
    )

    # Remove a block
    # TODO: how do we handle multi block removal?
    observeEvent(input$remove, {
      rv <- remove_node(input$network_selected, rv)
      if (nrow(rv$edges) > 0) {
        tryCatch(
          {
            rv <- remove_edge(input$network_selected, rv)
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

    # TODO: implement remove edge (user selects the edge).
    observeEvent(input$selected_edge, {
      showModal(
        modalDialog(
          title = sprintf("Edge %s", input$selected_edge),
          actionButton(ns("remove_edge"), "Remove"),
        )
      )
    })

    observeEvent(input$remove_edge, {
      rv$removed_edge <- NULL
      rv <- remove_edge(input$selected_edge, rv)

      # TO DO: check why the disconnected node still get input data
      # from the parent (probably because links are not refreshed ...)
      visNetworkProxy(ns("network")) |>
        visRemoveEdges(input$selected_edge)

      removeModal()
    })

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
