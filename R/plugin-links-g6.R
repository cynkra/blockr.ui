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
#' @rdname add_rm_g6_link
#' @export
add_rm_g6_link_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      dot_args <- list(...)
      obs <- list()

      # TBD When starting from non empty board (happens once)

      # TBD Restore network from serialisation

      output$network <- render_g6({
        isolate({
          # TBD allow for non empty initialisation
          #dot_args$parent$nodes
          #dot_args$parent$edges
          initialize_g6(ns = session$ns)
        })
      })

      # Add node to network board$nodes so the graph is updated
      observeEvent(dot_args$parent$added_block, {
        tryCatch(
          {
            create_g6_node(
              new = dot_args$parent$added_block,
              vals = dot_args$parent,
              rv = board,
              validate = TRUE,
              obs,
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
            create_g6_edge(
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
          cleanup_g6_node(removed, dot_args$parent, board, session)
        })
      })

      # Create stack from canvas context menu
      observeEvent(input$create_stack, {
        show_g6_stack_actions(
          board,
          session
        )
      })

      observeEvent(input$new_stack, {
        dot_args$parent$added_stack <- input$new_stack_nodes
        removeModal()
      })

      vals <- reactiveValues(stacks = NULL)

      # Stack nodes
      observeEvent(
        {
          req(length(board_stack_ids(board$board)) > 0)
          # As soon as one board stack isn't in vals$stacks
          req(any(!(board_stack_ids(board$board) %in% vals$stacks)))
        },
        {
          stack_g6_nodes(vals, board, dot_args$parent, session)
        }
      )

      # Remove stack: context menu for combo
      observeEvent(input$remove_stack, {
        unstack_g6_nodes(dot_args$parent, session)
      })

      output$state <- renderPrint(board_stacks(board$board))

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_g6_link
#' @export
add_rm_g6_link_ui <- function(id, board) {
  tagList(
    g6_output(NS(id, "network"), height = "400px"),
    verbatimTextOutput(NS(id, "state"))
  )
}
