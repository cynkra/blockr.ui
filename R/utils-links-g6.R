#' Initialize a G6 ANTV Network Visualization
#'
#' Creates a G6 ANTV network visualization with pre-configured options for nodes, edges,
#' layout, behaviors, and plugins.
#'
#' @param nodes A data frame or list containing node information. Each node should
#'   typically have at least an `id` field, and optionally other attributes like `label`.
#'   Default is `NULL`.
#' @param edges A data frame or list containing edge information. Each edge typically
#'   needs `source` and `target` fields to define connections. Default is `NULL`.
#'
#' @details
#' This function initializes a G6 network visualization with several pre-configured features:
#'
#' \itemize{
#'   \item Node styling with background labels
#'   \item Combo (group) support with circular styling
#'   \item Curved edge paths with directional arrows and dashed styling
#'   \item Dagre layout algorithm optimized for directed graphs
#'   \item Interactive behaviors including zoom, drag, selection, and edge creation
#'   \item Plugins for minimap, tooltips, fullscreen mode, context menu, and toolbar
#' }
#'
#' @return A G6 network visualization object that can be further customized or directly
#'   rendered in R Markdown, Shiny, or other R environments.
#' @keywords internal
initialize_g6 <- function(nodes = NULL, edges = NULL) {
  g6(
    nodes = nodes,
    edges = edges
  ) |>
    g6_options(
      animation = FALSE,
      node = list(
        style = list(
          labelBackground = TRUE,
          labelBackgroundFill = '#FFB6C1',
          labelBackgroundRadius = 4,
          labelFontFamily = 'Arial',
          labelPadding = c(0, 4),
          labelText = JS(
            "(d) => {
            return d.label
          }"
          )
        )
      ),
      combo = list(
        animation = FALSE,
        type = "circle-combo-with-extra-button",
        style = list(
          labelText = JS(
            "(d) => {
            return d.id
          }"
          )
        )
      ),
      edge = list(
        type = "fly-marker-cubic",
        endArrow = TRUE,
        zIndex = 100,
        style = list(targetPort = "port-1", lineDash = c(5, 5))
      )
    ) |>
    g6_layout(
      layout = antv_dagre_layout(
        ranksep = 500,
        nodesep = 100,
        sortByCombo = TRUE,
        controlPoints = TRUE
      )
    ) |>
    g6_behaviors(
      "zoom-canvas",
      drag_element(),
      click_select(multiple = TRUE),
      brush_select(),
      # avoid conflict with internal function
      g6R::create_edge()
    ) |>
    g6_plugins(
      "minimap",
      "tooltip",
      fullscreen(),
      context_menu(),
      g6R::toolbar()
    )
}
