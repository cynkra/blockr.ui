#' Register custom board plugins
#'
#' Ovewrite the default core plugins
#'
#' @param which (Optional) character vectors of plugins to include
#' @rdname plugins
#' @export
custom_board_plugins <- function(which = NULL) {
  plugins <- plugins(
    preserve_board(server = ser_deser_server, ui = ser_deser_ui),
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
    manage_links(server = add_rm_link_server, ui = add_rm_link_ui),
    manage_stacks(
      server = add_rm_stack_server,
      ui = add_rm_stack_ui
    ),
    notify_user(server = blockr.core::block_notification_server),
    generate_code(server = blockr.core::gen_code_server, ui = gen_code_ui),
    edit_block(
      server = blockr.core::edit_block_server,
      ui = blockr.core::edit_block_ui
    )
  )

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}
