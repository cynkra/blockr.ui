new_board_module <- function(ui, server, id, title, class = character()) {

	stopifnot(
	  is.function(ui),
    is.function(server),
    is.character(id), length(id) == 1L,
    is.character(title), length(title) == 1L
  )

  structure(
    list(server = server, ui = ui),
    id = id,
    title = title,
    class = c(class, "board_module")
  )
}

is_board_module <- function(x) {
  inherits(x, "board_module")
}

board_module_server <- function(x) {
  stopifnot(is_board_module(x))
  x[["server"]]
}

board_module_ui <- function(x) {
  stopifnot(is_board_module(x))
  x[["ui"]]
}

board_module_id <- function(x) {
  stopifnot(is_board_module(x))
  attr(x, "id")
}

board_module_title <- function(x) {
  stopifnot(is_board_module(x))
  attr(x, "title")
}

new_dashboard_module <- function(id = "dashboard", title = "Dashboard") {
  new_board_module(dashboard_ui, dashboard_server, id = id, title = title,
                   class = "dashboard_module")
}

call_board_module_ui <- function(x, ...) {
  board_module_ui(x)(...)
}
