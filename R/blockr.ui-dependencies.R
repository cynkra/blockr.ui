#' blockr.ui dependencies utils
#'
#' @description This function attaches blockr.ui dependencies to the given tag
#'
#' @param tag Element to attach the dependencies.
#'
#' @importFrom utils packageVersion
#' @importFrom htmltools tagList htmlDependency
#' @export
add_blockr.ui_deps <- function(tag) {
 blockr.ui_deps <- htmlDependency(
  name = "blockr.ui",
  version = "1.0.0",
  src = c(file = "blockr.ui-1.0.0"),
  script = "dist/blockr.ui.min.js",
  stylesheet = "dist/blockr.ui.min.css",
  package = "blockr.ui",
 )
 tagList(tag, blockr.ui_deps)
}
    
