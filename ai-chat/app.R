library(blockr.dplyr)
library(blockr.io)
library(blockr.sdtm)
library(blockr.ai)
library(blockr.gt)

chat_ui <- function(id, board, ...) {
  id <- NS(id, "chat")
  card(
    card_header("blockr.ui assistant"),
    shinychat::chat_mod_ui(
      id = NS(id, "openai"),
      messages = list(
        "Hi! I'll help you to build blockr.ui pipeline with OpenAI's `gpt-4o`."
      )
    )
  )
}

chat_srv <- function(board, update, session, parent, ...) {
  moduleServer(
    "chat",
    function(input, output, session) {
      openai <- ellmer::chat_openai(
        system_prompt = "You are a helpful assistant for developers who want to work with blockr but do not
          not very well data analysis. Packages are available at: https://github.com/BristolMyersSquibb/blockr.core,
          https://github.com/BristolMyersSquibb/blockr.dplyr, https://github.com/BristolMyersSquibb/blockr.io, These packages provide
          blocks such that people can import data with new_dataset_block, transform them with new_select_block and do other things.
          We also have blockr.ai that basically exposes llm blocks such as new_llm_transform_block and new_llm_plot_block. They
          are convenient to accomplish tasks for which no block exists yet ... A blockr.ui application allows people to build blockr pipeline
          step by step by adding block one after each other. 
          When you are given an order, you will only return the corresponding block 
          constructor to the user without the 'new'. You will return 'Adding a: *_block', '*' being the
          type of the block. Possible blocks are found in `available_blocks()` and the block name
          is found in the 'ctor_name' attribute removing the 'new' keyword.
          If you are asked to pass custom parameters to the block, 
          you will return an answer like `Adding custom block configuration: list(ctor = '<CONSTRUCTOR>', parms = list(param1 = 'value1', ...))`.
          If you are asked to opperate on the previous block, you will modify the message by replacing the beginning of the message by 'Append block: *', '*' can be
          a default block or a custom block configuration, as previously stated.
          For instance 'Add me a block to select columns from a dataset' would return 
          'Adding a: select_block'. If you are asked a question like 'How to load data?' 
          You will answer to the question with more ellaborated answer from the blockr documentation 
          and then you finish (your turn) the anwser by 'Adding a: dataset_block'.
          If you are asked 'How to get started?' or similar, you will explain how a blockr pipeline works, from a data block to a plot block and then you finish (your turn) by the anwser by 
          'Adding a: dataset_block' as a starting point.
          It you are asked anything else not 
          related to blockr, answer 'Your question is not related to blockr, please ask a question about blockr blocks.'.
          After returning the answer, the user has to be able to ask you a new question in a new turn.",
        model = "gpt-4o",
        echo = "all"
      )
      res <- shinychat::chat_mod_server("openai", openai)
      observeEvent(res(), {
        out_of_context <- grepl(
          "Your question is not related to blockr",
          res()@text
        )
        if (out_of_context) {
          return(NULL)
        }
        append <- grepl("Append block:", res()@text)
        is_config <- grepl("Adding custom block configuration:", res()@text)

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
    chat_ui,
    chat_srv,
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
      dash = new_dashboard_module(),
      chat = new_chat_module()
    )
  ),
  "main"
)

# Add a dataset block with custom parameters: dataset is penguins and package is palmerpenguins
