#' Add/remove block links module
#'
#' Customizable logic for adding/removing links between blocks on the
#' board.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A reactive value that evaluates to `NULL` or a list with components
#' `add` and `rm`, where `add` is either `NULL` or a `data.frame` with columns
#' `id`, `from`, `to` and `input` and `rm` is either `NULL` or a character
#' vector of link IDs.
#'
#' @rdname add_rm_link
#' @export
add_rm_link_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Local reactive values
      vals <- reactiveValues(
        edges = data.frame(),
        nodes = data.frame(),
        added_edge = character(),
        removed_edge = character()
      )

      dot_args <- list(...)

      obs <- list()

      # Restore network from serialisation
      observeEvent(req(rv$refreshed == "board"), {
        restore_network(links(), vals, rv, session)
      })

      # For serialisation
      observeEvent(vals$nodes, {
        rv$board[["nodes"]] <- vals$nodes
      })

      res <- reactiveVal(
        list(add = links(), rm = character())
      )

      links <- reactive(board_links(rv$board))

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

      # To capture what happens on the client (modify network data)
      #observeEvent(input$network_graphChange, {
      #  browser()
      #})

      # Tweaks UI so that edge mode appears when it should.
      # Also workaround a bug that always activate editNode by default.
      observeEvent(
        {
          req(input$network_initialized)
          list(
            board_links(rv$board),
            input$network_selected,
            input$network_graphChange
          )
        },
        {
          session$sendCustomMessage(
            "toggle-manipulation-ui",
            list(
              value = length(rv$blocks) > 1
            )
          )
        }
      )

      # Capture nodes position for serialization
      observeEvent(
        {
          req(nrow(vals$nodes) > 0)
          input$stabilized
        },
        {
          visNetworkProxy(ns("network")) |>
            visGetPositions()
        }
      )

      observeEvent(input$network_positions, {
        lapply(names(input$network_positions), \(id) {
          vals$nodes[vals$nodes$id == id, "x"] <- input$network_positions[[
            id
          ]]$x
          vals$nodes[vals$nodes$id == id, "y"] <- input$network_positions[[
            id
          ]]$y
        })
      })

      # Handle node update. Change of color due to block validity change ...
      # This needs input parameter from the parent module which contains
      # the list of block server functions.
      # For Nicolas: why does rv$msgs() triggers infinitely?
      observeEvent(rv$added_block, {
        register_node_validation(vals, rv, session)
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
          rv$append_block <- FALSE
          tryCatch(
            {
              create_edge(
                new = list(from = input$new_edge$from, to = input$new_edge$to),
                vals,
                rv,
                session
              )
              # Send callback to create corresponding link
              res(
                list(
                  add = vals$added_edge
                )
              )
            },
            error = function(e) {
              e$message
            }
          )
        }
      )

      # TODO: handle multi block action?

      # Add node to network rv$nodes so the graph is updated
      observeEvent(rv$added_block, {
        tryCatch(
          {
            create_node(rv$added_block, vals, rv, session)
            if (rv$append_block) {
              # Send any created link back to the board
              res(
                list(
                  add = vals$added_edge
                )
              )
            }
          },
          error = function(e) {
            e$message
          }
        )
        rv$append_block <- FALSE
      })

      # Remove node + associated edges
      observeEvent(rv$removed_block, {
        # Note: links are cleaned in the add_rm_blocks plugin
        cleanup_node(input$network_selected, vals, session)
      })

      # Communicate selected to upstream modules
      observeEvent(
        {
          req(input$network_initialized)
          input$network_selected
        },
        {
          if (nchar(input$network_selected) == 0) {
            rv$selected_block <- NULL
          } else {
            rv$selected_block <- input$network_selected
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

      observeEvent(input$remove_edge, {
        remove_edge(input$selected_edge, vals, session)
        # Update link callback
        res(
          list(
            rm = input$selected_edge
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
          req(input$mouse_location, nchar(input$node_right_clicked) > 0)
        },
        {
          show_node_menu(
            dot_args$parent$in_grid[[input$node_right_clicked]] %OR%
              FALSE,
            session
          )
        }
      )

      # Register add_to_grid observers
      observeEvent(req(length(board_block_ids(rv$board)) > 0), {
        register_node_menu_obs(
          board_block_ids(rv$board),
          dot_args$parent,
          rv,
          obs,
          session
        )
      })

      res
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
