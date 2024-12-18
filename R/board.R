#' Create block choices for scoutbaR widget
#'
#' Utility to populate the scoutbar with block
#' registry information. Create one page per block category
#' @keywords internal
blk_choices <- function() {
  blk_cats <- sort(unique(chr_ply(available_blocks(), \(b) attr(b, "category"))))

  lapply(blk_cats, \(cat) {
    scout_section(
      label = cat,
      .list = dropNulls(
        unname(lapply(available_blocks(), \(choice) {
          if (attr(choice, "category") == cat) {
            scout_action(
              id = attr(choice, "classes")[1],
              label = paste0(attr(choice, "name"), " (", attr(choice, "package"), ")"),
              description = attr(choice, "description")
            )
          }
        }))
      )
    )
  })
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
      shinyWidgets::switchInput(
        ns("mode"),
        onStatus = "default",
        onLabel = icon("network-wired"),
        offLabel = icon("table-columns"),
        value = TRUE,
        size = "mini"
      ),
      div(
        class = "btn-group",
        role = "group",
        network_ui$action_bar
      )
    ),
    tabsetPanel(
      id = ns("board_tabs"),
      type = "hidden",
      tabPanelBody(
        "network_tab",
        layout_sidebar(
          sidebar = sidebar(
            id = ns("sidebar"),
            open = FALSE,
            width = 600,
            class = "rounded",
            bg = "white",
            position = "right",
            # Node module (ui filters + output)
            uiOutput(ns("node_ui")),
            network_ui$sidebar
          ),
          network_ui$canvas
        )
      ),
      tabPanelBody(
        "dashboard_tab",
        dashboard_ui(ns("dash"))
      )
    )
  )
}

#' @rdname board
#' @export
board_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # The board must know about the blocks
      rv <- reactiveValues(blocks = list(), connections = list())
      exportTestValues(
        blocks = rv$blocks,
        network_out = network_out
      )

      # DAG representation
      # network_out$connections: dataframe of connected node ids.
      network_out <- network_server("dag")

      # Dashboard mode
      dashboard_server("dash")

      # Switch between dashboard and network view
      # TBD: ideally we create a toggle input with 2 values
      observeEvent(input$mode, {
        tab <- if (input$mode) "network" else "dashboard"
        updateTabsetPanel(
          session,
          "board_tabs",
          selected = sprintf("%s_tab", tab)
        )
      })

      # TBD: update the connections when edges change in the DAG
      # TBD: optimize -> only export added or removed connections
      # to avoid looping over all blocks
      observeEvent(network_out$edges(), {
        # TBD create function
        lapply(rv$blocks, \(blk) {
          if (!length(block_inputs(blk$block))) return(NULL)
          id <- block_uid(blk$block)
          blk_inputs <- block_inputs(blk$block)
          connections <- network_out$edges()[
            network_out$edges()$to == id, "from"
          ]

          if (!length(connections)) return(NULL)

          # TO DO Check that the number of connections don't exceed
          # the number of input slots of the given block

          # Inject result of connected downstream block if the connection
          # is not yet made
          if (is.null(rv$connections[[id]][[blk_inputs]]())) {
            rv$connections[[id]][[blk_inputs]](rv$blocks[[connections]]$server$res())
          }
        })
      })

      # TO DO: after the connection is created, we need to update it
      # whenever the result changes

      # Call block server module when node is added or removed
      observeEvent(network_out$added_block(), {
        blk <- network_out$added_block()
        rv$connections[[block_uid(blk)]] <- setNames(
          lapply(block_inputs(blk), \(x) reactiveVal()),
          block_inputs(blk)
        )
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
      })

      observeEvent(network_out$removed_block(), {
        # cleanup
        rv$blocks[[network_out$removed_block()]] <- NULL
        rv$connections[[network_out$removed_block()]] <- NULL
        bslib::toggle_sidebar("sidebar", open = FALSE)
      })

      # When a node is selected, we need to display
      # sidebar with node UI module.
      output$node_ui <- renderUI({
        selected <- network_out$selected()
        req(
          nchar(selected) > 0,
          rv$blocks[[selected]]
        )
        tmp <- rv$blocks[[selected]]
        isolate({
          # TBD create function
          state <- lapply(tmp$server$state, \(el) {
            if (is.reactive(el)) el() else el
          })
          do.call(block_ui, c(list(x = tmp$block, id = id), state))
        })
      })

      observeEvent(network_out$selected(),
        {
          bslib::toggle_sidebar(
            "sidebar",
            open = !is.null(network_out$selected()) && nchar(network_out$selected()) > 0
          )
        },
        ignoreNULL = FALSE
      )
    }
  )
}
