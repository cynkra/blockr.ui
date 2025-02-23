#' Insert blockr.ui dependencies
#'
#' Contains JavaScript necessary for the app to run.
#'
#' @param tag Tag to attach the dependency to.
#'
#' @keywords internal
use_blockr_ui_deps <- function(tag) {
  tagList(
    tag,
    htmltools::htmlDependency(
      name = "blockr-ui",
      version = utils::packageVersion(utils::packageName()),
      src = "assets",
      script = "js/custom.js",
      package = utils::packageName()
    )
  )
}
