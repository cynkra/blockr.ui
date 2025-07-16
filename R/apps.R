#' Demo app
#'
#' Run demo app
#'
#' @param ... Forwarded to \link[blockr.core]{new_board}.
#'
#' @export
run_demo_app <- function(...) {
  serve(
    new_dag_board(...),
    "main"
  )
}

#' @rdname run_demo_app
#' @export
new_dag_board <- function(..., modules = new_dashboard_module()) {

  if (is_board_module(modules)) {
    modules <- list(modules)
  }

  stopifnot(is.list(modules), all(lgl_ply(modules, is_board_module)))

  n_stacks <- 40
  stacks_color_palette <- "spectral"
  if (nchar(Sys.getenv("N_STACKS_COLORS")) > 0) {
    n_stacks <- Sys.getenv("N_STACKS_COLORS")
  }
  if (nchar(Sys.getenv("STACKS_COLOR_PALETTE")) > 0) {
    stacks_color_palett <- Sys.getenv("STACKS_COLOR_PALETTE")
  }

  snapshot_location <- tempdir()
  if (nchar(Sys.getenv("SNAPSHOT_LOCATION")) > 0) {
    snapshot_location <- Sys.getenv("SNAPSHOT_LOCATION")
  }

  auto_snapshot <- FALSE
  if (nchar(Sys.getenv("AUTO_SNAPSHOT")) > 0) {
    auto_snapshot <- as.logical(Sys.getenv("AUTO_SNAPSHOT"))
  }

  new_board(
    ...,
    options = new_board_options(
      dark_mode = "light",
      stacks_colors = hcl.colors(n_stacks, palette = stacks_color_palette),
      dashboard_type = dashboard_type,
      dashboard_zoom = 1,
      snapshot = list(
        location = snapshot_location,
        auto = auto_snapshot
      )
    ),
    modules = modules,
    class = "dag_board"
  )
}

board_modules <- function(board) {
  stopifnot(is_board(board))
  modules <- board[["modules"]]
  stopifnot(is.list(modules), all(lgl_ply(modules, is_board_module)))
  modules
}

#' @export
serve.dag_board <- function(x, id = "main", ...) {

  Sys.setenv("blockr_dark_mode" = "light")

  modules <- board_modules(x)

  ctx_menu_items <- unlst(
    c(
      list(
        list(
          create_edge_ctxm,
          remove_node_ctxm,
          remove_edge_ctxm,
          append_node_ctxm,
          create_stack_ctxm,
          remove_stack_ctxm,
          add_block_ctxm
        )
      ),
      lapply(modules, board_module_context_menu)
    )
  )

  plugins <- plugins(
    preserve_board(server = ser_deser_server, ui = ser_deser_ui),
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
    manage_links(
      server = gen_add_rm_link_server(ctx_menu_items),
      ui = add_rm_link_ui
    ),
    manage_stacks(server = add_rm_stack_server, ui = add_rm_stack_ui),
    generate_code(server = generate_code_server, ui = generate_code_ui),
    notify_user()
  )

  ui <- page_fillable(
    padding = 0,
    gap = 0,
    shinyjs::useShinyjs(),
    add_busy_load_deps(main_ui(id, x, plugins))
  )

  server <- function(input, output, session) {
    main_server(id, x, plugins, modules)
  }

  shinyApp(add_blockr.ui_deps(ui), server)
}
