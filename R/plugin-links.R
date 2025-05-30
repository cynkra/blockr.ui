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

      # TBD When starting from non empty board (happens once)
      observeEvent(
        req(
          isFALSE(dot_args$parent$cold_start),
          input[["network-initialized"]]
        ),
        {
          cold_start(vals, board, dot_args$parent, session)
        },
        once = TRUE
      )

      # TBD Restore network from serialisation
      observeEvent(req(dot_args$parent$refreshed == "board"), {
        restore_network(board, dot_args$parent, session)
      })

      # Serialize state
      observeEvent(input[["network-state"]], {
        dot_args$parent$network <- structure(
          input[["network-state"]],
          class = "network"
        )
      })

      output$network <- render_g6({
        isolate({
          # TBD allow for non empty initialisation
          #dot_args$parent$nodes
          #dot_args$parent$edges
          initialize_g6(ns = session$ns)
        })
      })

      # Trigger scoutbar from network menu
      observeEvent(input$add_block, {
        dot_args$parent$add_block <- input$add_block
      })

      # Add node to network board$nodes so the graph is updated
      observeEvent(dot_args$parent$added_block, {
        tryCatch(
          {
            create_node(
              new = dot_args$parent$added_block,
              vals = dot_args$parent,
              rv = board,
              validate = TRUE,
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

      # Append block
      observeEvent(input$append_node, {
        if (isFALSE(dot_args$parent$append_block))
          dot_args$parent$append_block <- TRUE
      })

      # Implement Edge creation by DND
      # we can drag from one node
      # to another to connect them.
      # Validation mechanism to allow connections or not...
      # Rules:
      # - The dragged target must exist.
      # - We can't drag the edge on the current selected node (avoid loops).
      # - A node that has already all input slots connected can't received any incoming connection.
      # data block can't receive input data. Transform block receive
      # as many input data as slot they have (1 for select, 2 for join, ...).
      observeEvent(input$added_edge, {
        dot_args$parent$append_block <- FALSE
        tryCatch(
          {
            create_edge(
              new = list(
                source = input$added_edge$source,
                target = input$added_edge$target,
                # also pass the edge id ...
                id = input$added_edge$id
              ),
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
      })

      # Communicate selected to upstream modules
      observeEvent(
        {
          req(input[["network-initialized"]])
          input[["network-selected_node"]]
        },
        {
          dot_args$parent$selected_block <- input[["network-selected_node"]]
        },
        ignoreNULL = FALSE
      )

      # Remove edge (user selects the edge).
      observeEvent(input$removed_edge, {
        update(
          list(
            links = list(rm = input$removed_edge)
          )
        )
      })

      # Node removal: from context menu or from toolbar (multi nodes possible)
      observeEvent(input$removed_node, {
        dot_args$parent$removed_block <- input$removed_node
      })

      # Remove node + associated edges (we can remove multiple nodes at once)
      observeEvent(dot_args$parent$removed_block, {
        # Note: links are cleaned in the add_rm_blocks plugin
        lapply(dot_args$parent$removed_block, \(removed) {
          cleanup_node(removed, dot_args$parent, board, session)
        })
      })

      # Create stack from canvas context menu
      observeEvent(input$create_stack, {
        show_stack_actions(
          board,
          session
        )
      })

      observeEvent(input$new_stack, {
        # Allow creation of empty stacks
        nodes_to_stack <- if (is.null(input$new_stack_nodes)) {
          ""
        } else {
          input$new_stack_nodes
        }
        dot_args$parent$added_stack <- nodes_to_stack
        removeModal()
      })

      vals <- reactiveValues(stacks = NULL)

      # Stack nodes
      observeEvent(
        {
          req(
            length(board_stack_ids(board$board)) > 0,
            input[["network-initialized"]]
          )
        },
        {
          last_stack_id <- paste(
            "combo",
            tail(board_stack_ids(board$board), n = 1),
            sep = "-"
          )
          # As soon as one board stack isn't in vals$stacks
          if (!(last_stack_id %in% vals$stacks)) {
            stack_nodes(
              stack_id = NULL,
              nodes = NULL,
              vals,
              board,
              dot_args$parent,
              session
            )
          }
        }
      )

      # Remove stack: context menu for combo
      observeEvent(input$remove_stack, {
        unstack_nodes(vals, dot_args$parent, session)
      })

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_link
#' @export
add_rm_link_ui <- function(id, board) {
  tagList(
    g6_output(NS(id, "network"), height = "400px")
  )
}
