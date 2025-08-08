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

return_board <- tool(
  function(blocks, links, stacks) {
    to_json(new_dag_board(
      blocks = blocks,
      links = links,
      stacks = stacks
    ))
  },
  name = "return_board",
  description = "Returns the current board.",
  arguments = list(
    blocks = type_object("a block produced by blockr.core::new_block"),
    links = type_object("a link produced by blockr.core::new_link"),
    stacks = type_object("a stack produced by blockr.core::new_stack")
  )
)

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
        system_prompt = "You are a helpful assistant for developers who want to work with blockr but do not
          not very well data analysis. Packages are available at: https://github.com/BristolMyersSquibb/blockr.core,
          https://github.com/BristolMyersSquibb/blockr.dplyr, https://github.com/BristolMyersSquibb/blockr.io, These packages provide
          blocks such that people can import data with new_dataset_block, transform them with new_select_block and do other things.
          We also have blockr.ai that basically exposes llm blocks such as new_llm_transform_block and new_llm_plot_block. They
          are convenient to accomplish tasks for which no block exists yet ... A blockr.ui application allows people to build blockr pipeline
          step by step by adding block one after each other.
          When you are asked to add a block, you will return brief explanations followed by 'Adding block: *_block', '*' being the name of the block.
          The name cannot contain 'new'.
          Possible block names are found running the tool `available_block_names()`. Do not try to invent new blocks.
          If you are asked to add a block with custom parameters, your explanation end by `Adding custom block: list(ctor = '<CONSTRUCTOR>', parms = list(param1 = 'value1', ...))`,
          <CONSTRUCTOR> being the name of the block constructor, such as `dataset_block`, `select_block`, etc.
          If you are asked to opperate on the previous block, your explanations end by 'Append block: <BLOCK>', where '<BLOCK>' can be
          a default block or a custom block configuration, as previously stated.
          For instance 'Add me a block to select columns from a dataset' would return explanations as well as
          'Adding a: select_block' at the end. If you are asked a question like 'How to load data?'
          You will answer to the question with more ellaborated answer from the blockr documentation.
          When you are not explicitly asked to add a block, you can suggest prompts the user might want to write, wrap the text of each prompt in `<span class=\"suggestion\">` tags.
Also use 'Suggested next steps:' to introduce the suggestions. For example:

```
Suggested next steps:

1. <span class=\"suggestion\">Suggestion 1.</span>
2. <span class=\"suggestion\">Suggestion 2.</span>
3. <span class=\"suggestion\">Suggestion 3.</span>
```. If there is a previous block, suggested text can be 'Append block: *_block to the previous block', '*' being
      the new block. When you return suggestions, you will not return any block configuration or so.
          If you are asked 'How to get started?' or similar, you will explain how a blockr pipeline works, from a data block to a plot block, followed by
          some suggestions as stated above.
          If you are asked to create a pipeline involving multiple blocks, you will return a board object with
          the corresponding blocks and links and stacks. For instance, if you are asked to 'Plot bill length as a function of flipper length in palmerpenguins female data.',
          you return `new_dag_board(
 blocks = c(
  data = new_dataset_block(dataset = \"penguins\", package = \"palmerpenguins\"),
  filter = new_filter_block(string = \"sex == 'female'\"),
  plot = new_scatter_block(x = \"flipper_length_mm\", y = \"bill_length_mm\")
 ),
 links = c(
  ab = new_link(\"data\", \"filter\"),
  bc = new_link(\"filter\", \"plot\")
 )
)`. Finally, your answer will be consise as we have limited space in the UI."
      )

      openai$register_tool(available_block_names)
      openai$register_tool(return_board)

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
        openai$set_turns(list())
      })

      observeEvent(res(), {
        no_block <- !grepl("block:", res()@text)
        if (no_block) {
          return(NULL)
        }
        append <- grepl("Append block:", res()@text)
        is_config <- grepl("Adding custom block:", res()@text)

        if (is_config) {
          # YOLO
          blk <- eval(str2lang(sub(".*: ", "", res()@text)))
        } else {
          blk <- sub(".*: ", "", res()@text)
        }

        # Order append block
        if (append) {
          parent$append_block <- TRUE
        }

        parent$scoutbar$action <- "add_block"
        parent$scoutbar$value <- blk
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

# Add a dataset block with custom parameters: dataset is penguins and package is palmerpenguins
