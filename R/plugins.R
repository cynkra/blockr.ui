#' Register custom board plugins
#'
#' Ovewrite the default core plugins
#'
#' @param which (Optional) character vectors of plugins to include
#' @rdname plugins
#' @export
dash_board_plugins <- function(which = NULL) {
  plugins <- plugins(
    preserve_board(server = ser_deser_server, ui = ser_deser_ui),
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
    manage_links(server = add_rm_link_g6_server, ui = add_rm_link_g6_ui),
    manage_stacks(server = add_rm_stack_server, ui = add_rm_stack_ui),
    notify_user(),
    generate_code(ui = gen_code_ui),
    edit_block(),
    edit_stack()
  )

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}
