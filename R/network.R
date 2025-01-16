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
          open = TRUE,
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
        visInteraction(hover = FALSE, multiselect = TRUE) |>
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
        visEdges(length = 200, smooth = FALSE) |>
        visEvents(
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
            console.log(e.event.target.offsetParent.className);
            Shiny.setInputValue('%s', e.controlEdge);
            //window.HTMLWidgets.find(`#$('.${e.event.target.offsetParent.className}').parent().parent().parent().attr('id')`)
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
          solver = "repulsion",
          stabilization = FALSE,
          # Make sure nodes are not too far away when created ...
          repulsion = list(centralGravity = 0.8, nodeDistance = 150)
        )
    })

    #observeEvent(input$network_selected, {
    #  browser()
    #})

    #observeEvent(input$hovered_node, {
    #  showNotification(input$hovered_node)
    #})

    #observeEvent(input$node_right_clicked, {
    #  showNotification(input$node_right_clicked)
    #})

    # Bind shift+e and esc to toggle the add edge mode
    # on keyboard events
    session$sendCustomMessage(
      "bind-network-keyboard-shortcuts",
      list(id = sprintf("#%s", ns("network")))
    )

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
    # TODO: implement edge creation
    # This is different from add_to. Ideally we can drag from one node
    # to another to connect them.
    # TODO: we need a validation mechanism to allow connections or not...
    # Rules:
    # - The dragged target must exist.
    # - We can't drag the edge on the current selected node (avoid loops).
    # - data block can't receive input data. Transform block receive
    # as many input data as slot they have (1 for select, 2 for join, ...).
    # - A node that has already all input slots connected can't received any incoming connection.
    observeEvent(
      {
        input$new_edge
        req(input$new_edge$from)
      },
      {
        # Rule 1
        if (is.null(input$new_edge$to)) {
          showNotification(
            "Unable to connect node. Please select a valid target.",
            type = "error"
          )
          return(NULL)
        }

        # Rule 2
        if (input$new_edge$from == input$new_edge$to) {
          showNotification(
            "Can't create a connection on the same block.",
            type = "error"
          )
          return(NULL)
        }

        # Rule 3.
        # TODO: ultimately we create S3 method
        if (inherits(vals$blocks[[input$new_edge$to]]$block, "data_block")) {
          showNotification(
            "Data blocks don't accept any incoming connection (data input).",
            type = "error"
          )
          # Recover the addEdge with input$network_graphChange and remove it right away.
          visNetworkProxy(ns("network")) |>
            visRemoveEdges(input$network_graphChange$id)
          return(NULL)
        }

        # Rule 4: need to track the available connections for a given node
        to_blk <- vals$blocks[[input$new_edge$to]]$block
        if (!check_connections(to_blk, vals)) {
          showNotification(
            "The target block can't receive anymore data input.",
            type = "error"
          )
          visNetworkProxy(ns("network")) |>
            visRemoveEdges(input$network_graphChange$id)
          return(NULL)
        }

        # Create the connection
        con_idx <- which(list_empty_connections(to_blk, vals) == TRUE)[[1]]
        rv <- add_edge(
          from = input$new_edge$from,
          to = input$new_edge$to,
          # TO DO: the connection must be made to the latest available input slot
          label = block_inputs(to_blk)[[con_idx]],
          rv = rv
        )

        visNetworkProxy(ns("network")) |>
          visUpdateEdges(rv$edges)
      }
    )

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

    # TODO: capture nodes position for serialization

    # TODO: handle node update. Change of color due to block validity change ...
    # This needs input parameter from the parent module which contains
    # the list of block server functions.

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
