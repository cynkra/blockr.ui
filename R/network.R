#' @rdname board
#' @export
network_ui <- function(id) {
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
        ns("add_block_to"),
        "New block",
        icon = icon("circle-plus"),
        class = "btn-light"
      ),
      actionButton(ns("remove"), "Remove block", icon = icon("trash"), class = "btn-light"),
    ),
    canvas = visNetworkOutput(ns("network"))
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
    title = NA,
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

#' Remove a node
#'
#' Update dataframe for visNetwork graph
#'
#' @param selected UID (character string) of node to remove.
#' @param nodes dataframe representing network data.
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

#' @rdname board
#' @export
network_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      edges = data.frame(),
      nodes = data.frame(),
      new_block = NULL
    )

    output$network <- renderVisNetwork({
      # Initialized as empty, we'll update with the proxy
      visNetwork(
        data.frame(),
        data.frame(),
        height = "100vh",
        width = "100%"
      ) |>
        visOptions(
          # To get currently selected node
          nodesIdSelection = TRUE
        )
    })

    # Trigger add block
    observeEvent(input$add_block, {
      rv$new_block <- NULL
      update_scoutbar(
        session,
        "scoutbar",
        revealScoutbar = TRUE
      )
    })

    # Adding a block, we update the rv$nodes so the graph is updated
    observeEvent(input$scoutbar, {
      # Construct block with empty defaults
      # TBD: maybe we want to provide more choices
      # but that would require more UI elements
      rv$new_block <- available_blocks()[[input$scoutbar]]()

      # Update node vals for the network rendering
      rv$nodes <- add_node(rv$new_block, rv$nodes)

      visNetworkProxy(ns("network")) |>
        visUpdateNodes(rv$nodes)
    })

    # TBD handle node update: change of color due to block validity change ...

    # TBD: implement add_block_to -> add a block after the selected one
    # We need a contextual registry and update the scoutbar with relevant
    # choices. I think we can use the same scoutbar as for the classic
    # add block with all choices.

    # Remove a block
    # TBD: how do we handle multi block removal?
    observeEvent(input$remove, {
      rv$nodes <- remove_node(input$network_selected, rv$nodes)
      visNetworkProxy(ns("network")) |>
        visRemoveNodes(input$network_selected)
    })

    # TBD implement edge creation

    # The network module must return the edge representation
    # since they are created from the network and needed in other parts,
    # as well as the selected items such as current node and/or edge.
    return(
      list(
        edges = reactive(rv$edges),
        nodes = reactive(rv$nodes),
        selected_node = reactive(input$network_selected),
        added_block = reactive(rv$new_block),
        removed_block = eventReactive(input$remove, {
          # Contains the UID of the block module to remove from
          # the board
          input$network_selected
        })
      )
    )
  })
}