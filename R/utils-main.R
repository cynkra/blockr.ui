toggle_preview <- function(vals, session) {
  is_odd <- session$input$preview %% 2 == 0
  vals$preview <- if (is_odd) FALSE else TRUE
  update_switch("mode", value = is_odd)
  updateActionButton(
    session,
    "preview",
    icon = if (!is_odd) icon("eye-slash") else icon("eye")
  )
  session$sendCustomMessage(
    "toggle-view",
    list(
      id = sprintf("#%s", session$ns("dashboard")),
      val = !is_odd
    )
  )
}
