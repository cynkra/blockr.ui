#' Add/remove block links module
#'
#' Customizable logic for adding/removing links between blocks on the
#' board.
#'
#' @param context_menu Context menu.
#'
#' @return NULL.
#'
#' @rdname add_rm_link
#' @export
gen_add_rm_link_server <- function(context_menu) {
  function(id, board, update, parent) {
    moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns

        # Signal to close the loading screen once the network is initialized
        observeEvent(input[["network-initialized"]], {
          session$sendCustomMessage("app-ready", list())
        })

        # When starting from non empty board (happens once)
        observeEvent(
          req(
            isFALSE(parent$cold_start),
            input[["network-initialized"]]
          ),
          {
            cold_start(vals, board, parent, session)
          },
          once = TRUE
        )

        # Restore network from serialisation
        observeEvent(req(parent$refreshed == "board"), {
          restore_network(board, parent, session)
        })

        # Serialize state
        observeEvent(input[["network-state"]], {
          parent$network <- structure(
            input[["network-state"]],
            class = "network"
          )
        })

        ctx_path <- session$registerDataObj(
          name = "context-menu-items",
          data = list(),
          filterFunc = function(data, req) {
            body_bytes <- req$rook.input$read(-1)
            res <- jsonlite::toJSON(
              build_context_menu(
                context_menu,
                board = isolate(reactiveValuesToList(board)),
                parent = isolate(reactiveValuesToList(parent)),
                target = jsonlite::fromJSON(rawToChar(body_bytes))
              )
            )
            httpResponse(
              content_type = "application/json",
              content = res
            )
          }
        )

        output$network <- render_g6({
          isolate({
            # TBD allow for non empty initialisation
            #parent$nodes
            #parent$edges
            initialize_g6(
              ns = session$ns,
              path = ctx_path,
              context_menu = context_menu
            )
          })
        })

        context_menu_entry_action(context_menu, input, output, session, board,
                                  update, parent)

        # Trigger show code
        observeEvent(input$show_code, {
          parent$display_code <- TRUE
        })

        # Trigger scoutbar from network menu
        observeEvent(input$add_block, {
          parent$scoutbar$trigger <- "links"
          parent$open_scoutbar <- TRUE
        })

        # Trigger save board
        observeEvent(input$save_board, {
          parent$save_board <- input$save_board
        })

        # Auto-click on the right scoutbar page
        # to display the right scoutbar sub-page depending on the trigger
        observeEvent(req(parent$scoutbar$is_open), {
          session$sendCustomMessage(
            "select-scoutbar-page",
            list(
              value = parent$scoutbar$trigger
            )
          )
        })

        # Trigger browse snapshots/open scoutbar
        observeEvent(input$browse_snapshots, {
          parent$scoutbar$trigger <- "serialize"
          parent$open_scoutbar <- TRUE
        })

        # Add node to network board$nodes so the graph is updated
        observeEvent(parent$added_block, {
          tryCatch(
            {
              create_node(
                new = parent$added_block,
                vals = parent,
                rv = board,
                validate = TRUE,
                session
              )

              if (parent$append_block) {
                # Send any created link back to the board
                update(
                  list(
                    links = list(add = parent$added_edge)
                  )
                )
              }
            },
            error = function(e) {
              e$message
            }
          )
          parent$append_block <- FALSE
        })

        # Append block
        observeEvent(
          {
            input$append_node
          },
          {
            if (is.null(parent$selected_block)) {
              return(NULL)
            }
            parent$scoutbar$trigger <- "links"
            if (isFALSE(parent$append_block)) {
              parent$append_block <- TRUE
            }
          }
        )

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
          parent$append_block <- FALSE
          tryCatch(
            {
              create_edge(
                new = list(
                  source = input$added_edge$source,
                  target = input$added_edge$target,
                  # also pass the edge id ...
                  id = input$added_edge$id
                ),
                parent,
                board,
                session
              )

              # Send callback to create corresponding link
              update(
                list(
                  links = list(add = parent$added_edge)
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
            parent$selected_block <- input[["network-selected_node"]]
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

        # Remove node + associated edges (we can remove multiple nodes at once)
        observeEvent(parent$removed_block, {
          # Note: links are cleaned in the add_rm_blocks plugin
          lapply(parent$removed_block, \(removed) {
            cleanup_node(removed, parent, board, session)
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
          parent$added_stack <- nodes_to_stack
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
                parent,
                session
              )
            }
          }
        )

        # Remove stack: context menu for combo
        observeEvent(input$remove_stack, {
          unstack_nodes(vals, parent, session)
        })

        # Add/remove to/from dashboard
        observeEvent(input$add_to_dashboard, {
          req(parent$selected_block)
          parent$in_grid[[parent$selected_block]] <- TRUE
        })

        observeEvent(input$remove_from_dashboard, {
          req(parent$selected_block)
          parent$in_grid[[parent$selected_block]] <- FALSE
        })

        NULL
      }
    )
  }
}

#' @param board The initial `board` object
#' @rdname add_rm_link
#' @export
add_rm_link_ui <- function(id, board) {
  tagList(
    g6_output(NS(id, "network"), height = "800px")
  )
}
