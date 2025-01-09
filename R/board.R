#' Create block choices for scoutbaR widget
#'
#' Utility to populate the scoutbar with block
#' registry information. Create one page per block category
#' @keywords internal
blk_choices <- function() {
  blk_cats <- sort(
    unique(chr_ply(available_blocks(), \(b) attr(b, "category")))
  )

  lapply(blk_cats, \(cat) {
    scout_section(
      label = cat,
      .list = dropNulls(
        unname(
          lapply(available_blocks(), \(choice) {
            if (attr(choice, "category") == cat) {
              scout_action(
                id = attr(choice, "classes")[1],
                label = attr(choice, "name"),
                description = attr(choice, "description")
              )
            }
          })
        )
      )
    )
  })
}

#' Restore block ui based on its state
#' @keywords internal
restore_block_ui <- function(block, state, id) {
  state <- lapply(state, \(el) {
    if (is.reactive(el)) el() else el
  })
  do.call(block_ui, c(list(x = block, id = id), state))
}

#' The board provides the main ui for blockr2
#'
#' The board is composed of 2 views pointing to the network module
#' or the dashboard module.
#' @param id Unique id.
#' @rdname board
#' @export
main_ui <- function(id) {
  ns <- NS(id)

  network_ui <- network_ui(ns("dag"))

  tagList(
    div(
      class = "d-flex justify-content-center align-items-center",
      div(
        class = "btn-group",
        role = "group",
        network_ui$action_bar
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        open = TRUE,
        position = "right",
        width = "60%",
        tabsetPanel(
          id = ns("board_tabs"),
          tabPanel(
            "Properties",
            value = "network_tab",
            # Node module (ui filters + output)
            div(id = ns("block_container_ui")),
            network_ui$sidebar,
            bslib::input_switch(
              ns("block_mode"),
              "Use in dashboard?"
            )
          ),
          tabPanel(
            "Dashboard",
            value = "dashboard_tab",
            gridstackOutput(ns("grid")),
            verbatimTextOutput(ns("grid_content"))
          )
        )
      ),
      network_ui$canvas
    )
  )
}

#' Init connections for a block
#'
#' @param blk Block object.
#' @keywords internal
init_connection <- function(blk) {
  stats::setNames(
    lapply(block_inputs(blk), \(x) reactiveVal()),
    block_inputs(blk)
  )
}

#' Add connection between 2 blocks
#'
#' @param con Edge id. Character.
#' @param edges Edges dataframe.
#' @param rv Reactivevalues containing connections information.
#' @keywords internal
#' @return A list with new observers and connections reactive values.
add_connection <- function(con, edges, rv) {
  # edge id is made as follows: <FROM_NODE_ID>_<TO_NODE_ID>
  ids <- strsplit(con, "_")[[1]]
  from_id <- ids[1]
  to_id <- ids[2]

  from_blk <- rv$blocks[[from_id]]
  to_blk <- rv$blocks[[to_id]]

  # Check receiver block input slots
  blk_inputs <- block_inputs(to_blk$block)
  if (!length(block_inputs(to_blk$block))) return(NULL)

  # Find connections
  con_label <- edges[edges$id == con, "label"]

  if (!length(con_label)) return(NULL)

  # Add connections
  # Inject result of connected downstream block if the connection
  # is not yet made. This needs an observer to listen to any change
  # in the upstream block result.
  obs_id <- sprintf("%s_%s_%s", from_id, to_id, con_label)

  rv$obs[[obs_id]] <- observeEvent(rv$blocks[[from_id]]$server$result(), {
    rv$connections[[to_id]][[con_label]](rv$blocks[[from_id]]$server$result())
  })

  list(obs = rv$obs, connections = rv$connections)
}

#' Remove connection between 2 blocks
#'
#' @param con Edge id to remove. Character.
#' @param rv Reactivevalues containing connections information.
#' @keywords internal
remove_connection <- function(con, rv) {
  ids <- strsplit(con, "_")[[1]]
  id_from <- ids[1]
  id_to <- ids[2]

  # Reset connections
  for (slot in names(rv$connections[[id_to]])) {
    rv$connections[[id_to]][[slot]](NULL)
  }

  # Destroy all update observers
  obs_to_destroy <- grep(id_from, names(rv$obs), value = TRUE)
  for (el in obs_to_destroy) {
    rv$obs[[el]]$destroy()
    rv$obs[[el]] <- NULL
  }

  list(obs = rv$obs, connections = rv$connections)
}

#' Init block ui and server module
#'
#' @param blk Block object.
#' @param rv Reactivevalues containing connections information.
#' @param ns Namespace.
#' @keywords internal
init_block <- function(blk, rv, ns) {
  rv$connections[[block_uid(blk)]] <- init_connection(blk)

  # Block ui needs to come before server is initialized
  # so that the UI is updated
  insertUI(
    sprintf("#%s", ns("block_container_ui")),
    ui = div(
      class = "m-2",
      id = ns(block_uid(blk)),
      card(
        full_screen = TRUE,
        card_title(sprintf("Node %s properties", block_uid(blk))),
        block_ui(blk, ns(NULL))
      )
    ),
    immediate = TRUE
  )

  rv$blocks[[block_uid(blk)]] <- list(
    # We need the block object to render the UI
    block = blk,
    # The server is the module from which we can
    # extract data, ...
    server = block_server(
      blk,
      data = rv$connections[[block_uid(blk)]]
    ),
    mode = "editor"
  )

  list(connections = rv$connections, blocks = rv$blocks)
}

#' @rdname board
#' @export
main_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # The board must know about the blocks and connections between them
      rv <- reactiveValues(
        blocks = list(),
        connections = list(),
        obs = list(),
        # Cache where nodes should be in the dashboard mode.
        grid = data.frame(),
      )

      # For shinytest2 (don't remove)
      exportTestValues(
        blocks = rv$blocks,
        network_out = network_out
      )

      # TO DO: make a function ...
      # Move items between properties tab and grid
      observeEvent(
        input$board_tabs,
        {
          dashboard_blocks <- rv$blocks[
            which(chr_ply(rv$blocks, `[[`, "mode") == "dashboard")
          ]
          if (!length(dashboard_blocks)) return(NULL)

          lapply(dashboard_blocks, \(blk) {
            if (input$board_tabs == "dashboard_tab") {
              # Similar gs_proxy_add so that we can
              # move an element to the grid and call the JS method
              # with parameters we like.

              # TO DO: currently the grid is reset by gs_proxy_remove_all
              # Find a way to keep the grid state.
              pars <- if (!nrow(rv$grid)) {
                list(
                  id = ns(block_uid(blk$block)),
                  w = 4,
                  h = 4
                )
              } else {
                as.list(rv$grid[rv$grid$id == ns(block_uid(blk$block)), ])
              }

              session$sendCustomMessage(
                "add-grid-widget",
                message = list(
                  id = ns("grid"),
                  data = pars
                )
              )
            } else {
              # Move items back to properties panel
              session$sendCustomMessage(
                "move-widget-to-sidebar",
                list(
                  id = sprintf("#%s", ns("block_container_ui")),
                  block_id = sprintf("#%s", ns(block_uid(blk$block)))
                )
              )
            }
          })

          # Cleanup grid in editor mode
          if (input$board_tabs == "network_tab") {
            gs_proxy_remove_all("grid")
          }
        }
      )

      # DAG representation
      network_out <- network_server("dag", rv)

      # Dashboard mode
      #dashboard_out <- dashboard_server("dash")

      # Manage new connections
      observeEvent(req(network_out$added_edge()), {
        # In some cases like the join block, multiple edges can
        # be added
        for (edge in network_out$added_edge()) {
          res <- add_connection(edge, network_out$edges(), rv)
          rv$obs <- res$obs
          rv$connections <- res$connections
        }
      })

      # When an edge is removed, we reset the correponding connection
      # so that blocks don't show outdated data ...
      observeEvent(req(network_out$removed_edge()), {
        # As removing a node may remove multiple edges ...
        # we need to loop over ...
        for (con in network_out$removed_edge()) {
          res <- remove_connection(con, rv)
          rv$obs <- res$obs
          rv$connections <- res$connections
        }
      })

      # Call block server module when node is added
      observeEvent(network_out$added_node(), {
        res <- init_block(network_out$added_node(), rv, ns)
        rv$connections <- res$connections
        rv$blocks <- res$blocks
      })

      # To be able to know if the block needs to
      # be rendered in the dashboard grid.
      # TO DO: fix a bug when adding multiple nodes
      # the button should be reset to FALSE when a node
      # isn't in the dashboard mode...
      observeEvent(
        {
          req(nchar(network_out$selected_node()) > 0)
          input$block_mode
        },
        {
          selected <- network_out$selected_node()
          if (input$block_mode) {
            rv$blocks[[selected]]$mode <- "dashboard"
          } else {
            rv$blocks[[selected]]$mode <- "editor"
          }
        }
      )

      # When a node is selected, we need to only show the
      # UI of the selected block
      observeEvent(req(nchar(network_out$selected_node()) > 0), {
        selected <- network_out$selected_node()
        to_hide <- which(names(rv$blocks) != selected)

        shinyjs::show(selected)
        if (length(to_hide)) {
          lapply(names(rv$blocks)[to_hide], \(el) {
            shinyjs::hide(el)
          })
        }
      })

      # Handle node removal
      observeEvent(network_out$removed_node(), {
        # cleanup
        rv$blocks[[network_out$removed_node()]] <- NULL
        # Remove node from grid: need to find in which grid it is ...
        is_in_dashboard <- length(
          grep(network_out$removed_node(), rv$grid$id)
        ) >
          0
        if (is_in_dashboard) {
          gs_proxy_remove_item("grid", id = network_out$removed_node())
        } else {
          # Remove block UI
          removeUI(sprintf("#%s", ns(network_out$removed_node())))
        }

        bslib::toggle_sidebar("sidebar", open = FALSE)
      })

      # Render bucket of node outputs in dashboard mode
      # To avoid an ID duplication issue,
      # We rely on an rv cache to keep track of where a node was before switching from
      # a mode to another. This cache is a list contains bucket and body dataframes with block
      # id, (x, y) position and dimensions (h and w).
      # NOTE: it seems that Shiny some inputs won't work in a gridstack like selectize.js elements

      # The GridStack layout can be retrieved via the special shiny input ⁠input$<outputId>_layout⁠.
      # This allows us to know which block is where and restore the correct layout via a proxy (see
      # observer above).
      output$grid <- renderGridstack({
        gridstack(
          margin = "10px",
          cellHeight = "140px",
          resize_handles = "all",
          removable = TRUE,
          float = TRUE,
          options = list(
            acceptWidgets = TRUE
          )
        )
      })
      outputOptions(output, "grid", suspendWhenHidden = FALSE)

      # Same as for the bucket, except that we assign dummy dimensions
      # when they don't exist.
      grid_content <- reactive({
        if (is.null(input$grid_layout)) return(data.frame())
        res <- do.call(rbind.data.frame, input$grid_layout$children)
        res[, !names(res) %in% c("content")]
      })

      # Update rv cache to real time change in the grid
      observeEvent(grid_content(), {
        rv$grid <- grid_content()
      })

      # Debug only
      output$grid_content <- renderPrint(grid_content())

      # Toggle sidebar on node selection/deselection
      observeEvent(
        network_out$selected_node(),
        {
          bslib::toggle_sidebar(
            "sidebar",
            open = !is.null(network_out$selected_node()) &&
              nchar(network_out$selected_node()) > 0
          )
        },
        ignoreNULL = FALSE
      )
    }
  )
}
