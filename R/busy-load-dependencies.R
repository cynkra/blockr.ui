#' busy-load dependencies utils
#'
#' @description This function attaches busy-load. dependencies to the given tag
#'
#' @param tag Element to attach the dependencies.
#'
#' @importFrom htmltools tagList htmlDependency
#' @export
add_busy_load_deps <- function(tag) {
  busy_load_deps <- htmlDependency(
    name = "busy-load",
    version = "0.1.2",
    src = c(file = "busy-load-0.1.2"),
    script = "js/app.min.js",
    stylesheet = "css/app.min.css",
    package = "blockr.ui",
  )
  tagList(tag, busy_load_deps)
}
