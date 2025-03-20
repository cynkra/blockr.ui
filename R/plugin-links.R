#' Add/remove block links module
#'
#' Customizable logic for adding/removing links between blocks on the
#' board.
#'
#' @param id Namespace ID.
#' @param board Reactive values object.
#' @param update Reactive value object to initiate board updates.
#' @param ... Extra arguments passed from parent scope.
#'
#' @return NULL.
#'
#' @rdname add_rm_link
#' @export
add_rm_link_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      dot_args <- list(...)

      obs <- list()

      # Restore network from serialisation
      observeEvent(req(dot_args$parent$refreshed == "board"), {
        restore_network(board, dot_args$parent, session)
      })

      # Get nodes and coordinates: useful to cache the current
      # nodes data so that we can restore snapshots correctly.
      #observeEvent(
      #  {
      #    if (is.null(dot_args$parent$refreshed)) {
      #      dot_args$parent$nodes
      #    } else {
      #      req(dot_args$parent$refreshed == "network")
      #    }
      #  },
      #  {
      #    visNetworkProxy(ns("network")) |> visGetNodes()
      #  }
      #)

      output$network <- renderVisNetwork({
        # Initialized as empty, we'll update with the proxy
        create_network_widget(ns = ns)
      })

      # Bind shift+e and esc to toggle the add edge mode
      # on keyboard events
      observeEvent(req(input$network_initialized), {
        session$sendCustomMessage(
          "bind-network-keyboard-shortcuts",
          list(id = sprintf("#%s", ns("network")))
        )
        session$sendCustomMessage(
          "hide-vis-dropdown",
          sprintf("#nodeSelect%s", ns("network"))
        )
      })

      # Tweaks UI so that edge mode appears when it should.
      # Also workaround a bug that always activate editNode by default.
      observeEvent(
        {
          req(input$network_initialized)
          list(
            board_links(board$board),
            input$network_selected,
            input$network_graphChange
          )
        },
        {
          session$sendCustomMessage(
            "toggle-manipulation-ui",
            list(
              value = length(board$blocks) > 1
            )
          )
        }
      )

      # Capture nodes position for serialization
      observeEvent(
        {
          req(nrow(dot_args$parent$nodes) > 0)
          input$stabilized
        },
        {
          visNetworkProxy(ns("network")) |>
            visGetPositions()
        }
      )

      observeEvent(input$network_positions, {
        lapply(names(input$network_positions), \(id) {
          dot_args$parent$nodes[
            dot_args$parent$nodes$id == id,
            "x"
          ] <- input$network_positions[[
            id
          ]]$x
          dot_args$parent$nodes[
            dot_args$parent$nodes$id == id,
            "y"
          ] <- input$network_positions[[
            id
          ]]$y
        })
      })

      # Handle node update. Change of color due to block validity change ...
      # This needs input parameter from the parent module which contains
      # the list of block server functions.
      # For Nicolas: why does board$msgs() triggers infinitely?
      observeEvent(dot_args$parent$added_block, {
        register_node_validation(
          block_uid(dot_args$parent$added_block),
          board,
          dot_args$parent,
          session
        )
      })

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
          dot_args$parent$append_block <- FALSE
          tryCatch(
            {
              create_edge(
                new = list(from = input$new_edge$from, to = input$new_edge$to),
                dot_args$parent,
                board,
                session
              )
              # Send callback to create corresponding link
              update(
                list(
                  links = list(add = dot_args$parent$added_edge)
                )
              )
            },
            error = function(e) {
              e$message
            }
          )
        }
      )

      # Add node to network board$nodes so the graph is updated
      observeEvent(dot_args$parent$added_block, {
        tryCatch(
          {
            create_node(
              dot_args$parent$added_block,
              dot_args$parent,
              board,
              session
            )
            if (dot_args$parent$append_block) {
              # Send any created link back to the board
              update(
                list(
                  links = list(add = dot_args$parent$added_edge)
                )
              )
            }
          },
          error = function(e) {
            e$message
          }
        )
        dot_args$parent$append_block <- FALSE
      })

      # Multi nodes removal
      observeEvent(input$remove_blocks, {
        dot_args$parent$removed_block <- input$selected_nodes
      })

      # Remove node + associated edges (we can remove multiple nodes at once)
      observeEvent(dot_args$parent$removed_block, {
        # Note: links are cleaned in the add_rm_blocks plugin
        lapply(dot_args$parent$removed_block, \(removed) {
          cleanup_node(removed, dot_args$parent, session)
        })
      })

      # Communicate selected to upstream modules
      observeEvent(
        {
          req(input$network_initialized)
          input$network_selected
        },
        {
          if (nchar(input$network_selected) == 0) {
            dot_args$parent$selected_block <- NULL
          } else {
            dot_args$parent$selected_block <- input$network_selected
          }
        }
      )

      # Remove edge (user selects the edge).
      observeEvent(input$selected_edge, {
        showModal(
          modalDialog(
            title = sprintf("Edge %s", input$selected_edge),
            actionButton(ns("remove_edge"), "Remove"),
          )
        )
      })

      observeEvent(req(input$remove_edge > 0), {
        remove_edge(input$selected_edge, dot_args$parent, session)
        # Update link callback
        update(
          list(
            links = list(rm = input$selected_edge)
          )
        )
        removeModal()
      })

      # Register event so that we know the mouse position oncontext
      observeEvent(
        req(input$network_initialized),
        {
          session$sendCustomMessage(
            "capture-mouse-position",
            ns("mouse_location")
          )
        },
        once = TRUE
      )

      # Node right click (needs mouse location to correctly insert
      # the card in the DOM)
      observeEvent(
        {
          req(
            input$mouse_location,
            nchar(input$node_right_clicked) > 0,
            length(input$node_right_clicked) == 1
          )
        },
        {
          show_node_menu(
            dot_args$parent$in_grid[[input$node_right_clicked]] %OR%
              FALSE,
            board_stack_ids(board$board),
            !is.na(dot_args$parent$nodes[
              dot_args$parent$nodes$id == input$node_right_clicked,
              "group"
            ]),
            session
          )
        }
      )

      # Register add_to_grid observers
      observeEvent(req(length(board_block_ids(board$board)) > 0), {
        register_node_menu_obs(
          board_block_ids(board$board),
          vals,
          dot_args$parent,
          obs,
          session
        )
      })

      # Multi actions
      # Stack creation from network
      # control + select to multiselect nodes
      # Receive new stack to update nodes
      vals <- reactiveValues(
        stacks = list(),
        palette = hcl.colors(20, palette = "spectral")
      )
      observeEvent(input$selected_nodes, {
        show_stack_actions(input$selected_nodes, dot_args$parent, session)
      })

      # Order stack creation to stack plugin
      observeEvent(input$new_stack, {
        trigger_create_stack(input$selected_nodes, dot_args$parent)
        removeModal()
      })

      # Style nodes within a stack
      observeEvent(
        {
          req(
            length(board_stacks(board$board)) > 0,
            # Only stack nodes that are not already in a stack
            !(tail(board_stack_ids(board$board)) %in% vals$stacks)
          )
        },
        {
          stack_nodes(vals, board, dot_args$parent, session)
        }
      )

      # Order removal of stack
      observeEvent(input$remove_stack, {
        trigger_remove_stack(input$selected_nodes[1], dot_args$parent)
        removeModal()
      })

      # Reset node styling to factory
      observeEvent(
        {
          req(length(board_stacks(board$board)) > 0)
          dot_args$parent$removed_stack
        },
        {
          unstack_nodes(vals, board, dot_args$parent, session)
        }
      )

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_link
#' @export
add_rm_link_ui <- function(id, board) {
  tagList(
    visNetworkOutput(NS(id, "network"))
  )
}
