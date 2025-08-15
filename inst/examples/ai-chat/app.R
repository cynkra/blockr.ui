library(blockr.dplyr)
library(blockr.io)
library(blockr.sdtm)
library(blockr.ai)
library(blockr.gt)
library(shinychat)
library(ellmer)

available_block_names <- tool(
  function() {
    names(blockr.core::available_blocks())
  },
  name = "available_block_names",
  description = "Returns a vector of strings containing registered block names.",
  arguments = list()
)

get_block_parameters <- function(ctor) {
  parms <- formals(ctor)
  parms[setdiff(names(parms), "...")]
}

chat_mod_ui <- function(id, board, ...) {
  ns <- NS(NS(id, "chat"))
  card(
    card_header(
      class = "d-flex justify-content-between",
      "blockr.ui assistant",
      actionLink(
        ns("prompt_clean"),
        icon("trash")
      )
    ),
    chat_ui(
      id = ns("prompt"),
      messages = list(
        "Hi! I'll help you to build blockr.ui pipeline with OpenAI's `gpt-4o`."
      )
    )
  )
}

chat_mod_srv <- function(board, update, session, parent, ...) {
  moduleServer(
    "chat",
    function(input, output, session) {
      # Maybe we want to replace this by ellmer::chat to allow for any provider. The provider name
      # could be passed as a board option or so ...
      openai <- ellmer::chat_openai(
        system_prompt = readLines(system.file(
          "examples/ai-chat/rules.md",
          package = "blockr.ui"
        ))
      )

      openai$register_tool(available_block_names)

      # Request to store tool results
      app_request <- reactiveVal(NULL)
      create_block_tool_factory <- tool(
        function(ctor) {
          # Extract constructor parameters
          if (grepl("llm", ctor)) {
            # LLM only have question field
            parms <- pairlist(question = character())
          } else {
            parms <- get_block_parameters(ctor)
          }

          # Create tool arguments based on constructor parameters
          tool_args <- lapply(names(parms), function(name) {
            if (is.language(parms[[name]])) {
              parms[[name]] <- eval(parms[[name]])
            }
            switch(
              typeof(parms[[name]]),
              character = type_array(type_string()),
              integer = type_array(type_integer()),
              numeric = type_array(type_number()),
              logical = type_array(type_boolean()),
              list = type_object(),
              NULL
            )
          })
          names(tool_args) <- names(parms)

          # Create the specific block tool
          block_tool <- tool(
            function(name, append = FALSE, parms = list()) {
              dat <- list(
                name = name,
                append = append,
                parms = parms
              )

              # Needs a reactive context... will happen once
              observeEvent(TRUE, {
                parent$scoutbar$action <- "add_block"
                parent$scoutbar$value <- dat
                if (dat$append) {
                  parent$append_block <- TRUE
                }
              })
              return(app_request(list(action = "add_block", data = dat)))
            },
            name = paste0("add_", ctor),
            description = paste(
              "Add a",
              ctor,
              "with specific parameters"
            ),
            arguments = list(
              name = type_string(
                "Name of the block to be created. Typically like *_block where '*' is
                the block type (dataset, select, ...) and without the new_ prefix.
                Valid names are given by the `available_block_names` tool."
              ),
              append = type_boolean(
                "Whether to append to previous block. Default to FALSE."
              ),
              parms = do.call(
                "type_object",
                c(
                  .description = "Parameters for the block constructor. 
                  Each parameter type is inferred from the default values of 
                  the block constructor.",
                  tool_args,
                  .required = FALSE
                )
              )
            )
          )
          openai$register_tool(block_tool)
          return(NULL)
        },
        name = "create_block_tool_factory",
        description = "Create a tool for a given block type.",
        arguments = list(
          ctor = type_string(
            "Block constructor to create tool for (e.g., 'new_dataset_block', 'new_select_block').
            Valid names are given by the `available_block_names` tool."
          )
        )
      )
      openai$register_tool(create_block_tool_factory)

      remove_block <- tool(
        function(id) {
          # Needs a reactive context... will happen once
          observeEvent(TRUE, {
            if (!(id %in% board_block_ids(board$board))) {
              showNotification(
                paste("Block with id", id, "does not exist."),
                type = "error"
              )
              return(NULL)
            }
            parent$removed_block <- id
          })
          return(app_request(list(action = "remove_block", data = id)))
        },
        name = "remove_block",
        description = "Remove a block by its id.",
        arguments = list(
          id = type_string(
            "Id of the block to be removed."
          )
        )
      )

      openai$register_tool(remove_block)

      create_stack <- tool(
        function(blocks) {
          # Needs a reactive context... will happen once
          observeEvent(TRUE, {
            if (any(!(blocks %in% board_block_ids(board$board)))) {
              showNotification(
                "Some blocks do not exist in the board.",
                type = "error"
              )
              return(NULL)
            }
            parent$added_stack <- blocks
          })
          return(app_request(list(
            action = "add_stack",
            data = list(blocks = blocks)
          )))
        },
        name = "create_stack",
        description = "Create a stack with a given name and blocks.",
        arguments = list(
          blocks = type_array(type_string(
            "Ids of the blocks to include in the stack."
          ))
        )
      )

      openai$register_tool(create_stack)

      append_stream_task <- shiny::ExtendedTask$new(
        function(client, ui_id, user_input) {
          promises::then(
            promises::promise_resolve(client$stream_async(
              user_input,
              tool_mode = "sequential",
              stream = "content"
            )),
            function(stream) {
              chat_append(ui_id, stream)
            }
          )
        }
      )

      chat_restore("prompt", openai)

      observeEvent(input$prompt_user_input, {
        append_stream_task$invoke(openai, "prompt", input$prompt_user_input)
      })

      res <- reactive({
        if (append_stream_task$status() == "success") {
          openai$last_turn()
        }
      })

      observeEvent(input$prompt_clean, {
        chat_clear("prompt")
        # This also erase the chat memory and not just the UI
        #openai$set_turns(list())
      })

      observeEvent(res(), {
        # Order append block
        #if (app_request()$action == "add_block") {
        #  # If the action is to set the board, we can update the parent with the new board
        #  parent$scoutbar$action <- "add_block"
        #  parent$scoutbar$value <- app_request()$data
        #  if (app_request()$data$append) {
        #    parent$append_block <- TRUE
        #  }
        #}
        #else if (app_request()$action == "set_board") {
        #  browser()
        #  if (!length(board_blocks(board$board))) {
        #    board_blocks(board$board) <- app_request()$data$blocks
        #    board_links(board$board) <- app_request()$data$links
        #    board_stacks(board$board) <- app_request()$data$stacks
        #    cold_start(board, parent, session)
        #  } else {}
        #  # Are there existing blocks in the board?
        #}
      })
    }
  )
}

new_chat_module <- function(id = "blockr_assistant", title = "AI chat") {
  new_board_module(
    chat_mod_ui,
    chat_mod_srv,
    id = id,
    title = title,
    context_menu = list(
      new_context_menu_entry(
        name = "Open AI chat",
        js = "() => {
          console.log('Hello world')
        }",
        action = function(input, output, session, board, update, parent) {
          # TBD
        }
      )
    ),
    class = "chat_module"
  )
}

serve(
  new_dag_board(
    modules = list(
      #dash = new_dashboard_module(),
      chat = new_chat_module()
    )
  ),
  "main"
)
