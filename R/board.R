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
                label = paste0(
                  attr(choice, "name"),
                  " (",
                  attr(choice, "package"),
                  ")"
                ),
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
board_ui <- function(id) {
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
        open = FALSE,
        width = 300,
        class = "rounded",
        bg = "white",
        network_ui$sidebar
      ),
      layout_sidebar(
        sidebar = sidebar(
          open = FALSE,
          position = "right",
          width = "100%",
          # bucket
          gridstackOutput(ns("bucket")),
          verbatimTextOutput(ns("bucket_content")),
          # body
          gridstackOutput(ns("body")),
          verbatimTextOutput(ns("body_content"))
        ),
        network_ui$canvas,
        border = FALSE
      ),
      border_radius = FALSE,
      fillable = TRUE,
      class = "p-0"
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

#' Init block server module
#'
#' @param blk Block object.
#' @param rv Reactivevalues containing connections information.
#' @keywords internal
init_block_server <- function(blk, rv) {
  rv$connections[[block_uid(blk)]] <- init_connection(blk)
  rv$blocks[[block_uid(blk)]] <- list(
    # We need the block object to render the UI
    block = blk,
    # The server is the module from which we can
    # extract data, ...
    server = block_server(
      blk,
      data = rv$connections[[block_uid(blk)]]
    )
  )

  list(connections = rv$connections, blocks = rv$blocks)
}

#' @rdname board
#' @export
board_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # The board must know about the blocks and connections between them
      rv <- reactiveValues(
        blocks = list(),
        connections = list(),
        obs = list(),
        # Cache where nodes should be in the dashboard mode: either bucket or body.
        grid = list(
          bucket = data.frame(),
          body = data.frame()
        )
      )

      # For shinytest2 (don't remove)
      exportTestValues(
        blocks = rv$blocks,
        network_out = network_out
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
        res <- init_block_server(network_out$added_node(), rv)
        rv$connections <- res$connections
        rv$blocks <- res$blocks

        new_blk <- rv$blocks[[block_uid(network_out$added_node())]]

        gs_proxy_add(
          "bucket",
          gs_item(
            card(
              full_screen = TRUE,
              card_title(sprintf("Block %s", class(new_blk$block)[1])),
              #htmltools::tagQuery(
              #restore_block_ui(blk$block, blk$server$state, id)
              #)$selectedTags()[[2]],
              restore_block_ui(new_blk$block, new_blk$server$state, id)
            )
          ),
          list(id = block_uid(network_out$added_node()))
        )
      })

      # Handle node removal
      observeEvent(network_out$removed_node(), {
        # cleanup
        rv$blocks[[network_out$removed_node()]] <- NULL
        # Remove node from grid: need to find in which grid it is ...
        is_in_bucket <- length(
          grep(network_out$removed_node(), rv$grid$bucket$id)
        ) >
          0
        if (is_in_bucket) proxy <- "bucket" else "body"
        gs_proxy_remove_item(proxy, id = network_out$removed_node())

        bslib::toggle_sidebar("sidebar", open = FALSE)
      })

      # Render bucket of node outputs in dashboard mode
      # To avoid an ID duplication issue,
      # We rely on an rv cache to keep track of where a node was before switching from
      # a mode to another. This cache is a list contains bucket and body dataframes with block
      # id, (x, y) position and dimensions (h and w).
      # NOTE: it seems that Shiny some inputs won't work in a gridstack like selectize.js elements

      # Store in output to activate the widget callback (see below)
      output$bucket <- renderGridstack({
        gridstack(
          disableResize = TRUE,
          column = 1,
          options = list(
            acceptWidgets = TRUE,
            dragOut = TRUE
          )
        )
      })
      # We still need to update the bucket content when the output is hidden
      # This is because some dependent nodes need the result of a parent node
      # to render their result.
      outputOptions(output, "bucket", suspendWhenHidden = FALSE)

      # The GridStack layout can be retrieved via the special shiny input ⁠input$<outputId>_layout⁠.
      # This allows us to know which block is where and restore the correct layout via a proxy (see
      # observer above).
      bucket_content <- reactive({
        if (is.null(input$bucket_layout)) return(data.frame())
        res <- do.call(rbind.data.frame, input$bucket_layout$children)
        res[, !names(res) %in% c("content")]
      })

      # Update rv cache to real time change in the grid
      observeEvent(bucket_content(), {
        rv$grid$bucket <- bucket_content()
      })

      # Debug only
      output$bucket_content <- renderPrint(bucket_content())

      output$body <- renderGridstack({
        gridstack(
          margin = "10px",
          cellHeight = "140px",
          resize_handles = "all",
          float = TRUE,
          options = list(
            acceptWidgets = TRUE
          )
        )
      })
      outputOptions(output, "body", suspendWhenHidden = FALSE)

      # Same as for the bucket, except that we assign dummy dimensions
      # when they don't exist.
      body_content <- reactive({
        if (is.null(input$body_layout)) return(data.frame())
        items <- lapply(input$body_layout$children, \(child) {
          if (is.null(child$h)) child$h <- NA
          if (is.null(child$w)) child$w <- NA
          child
        })
        res <- do.call(rbind.data.frame, items)
        res[, !names(res) %in% c("content")]
      })

      # Update rv cache to real time change in the grid
      observeEvent(body_content(), {
        rv$grid$body <- body_content()
      })

      # Debug only
      output$body_content <- renderPrint(body_content())

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
