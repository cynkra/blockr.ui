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
#' @rdname add_rm_link_g6
#' @export
add_rm_link_g6_server <- function(id, board, update, ...) {
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

      output$state <- renderPrint(input[["network-state"]])

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_link_g6
#' @export
add_rm_link_g6_ui <- function(id, board) {
  tagList(
    g6_output(NS(id, "network"), height = "400px") #,
    #verbatimTextOutput(NS(id, "state"))
  )
}
