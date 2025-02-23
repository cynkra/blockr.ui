const getWidget = (id) => {
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);
  var widgetObj;
  if (typeof htmlWidgetsObj !== "undefined") {
    widgetObj = htmlWidgetsObj.getWidget();
  }
  return widgetObj;
}

export const setupBlocksGrid = () => {
  Shiny.addCustomMessageHandler("add-grid-widget", (obj) => {
    var grid = getWidget(obj.id);
    if (typeof grid != "undefined") {
      obj.data.content = `<div
              id="grid-stack-item-content-${obj.data.id}"
              class="bg-white p-2 border rounded-4">
            </div>`
      // Initialize the widget
      grid.addWidget(obj.data);
      // Move element from the sidebar to the grid slot
      $(`#grid-stack-item-content-${obj.data.id}`).append($(`#${obj.data.id}`));
      // If the element is hidden by shinyjs, show it
      if ($(`#${obj.data.id}`).is(":hidden")) {
        $(`#${obj.data.id}`).show();
      }
    }
  });

  Shiny.addCustomMessageHandler('move-widget-to-sidebar', (m) => {
    $(m.id).append($(m.block_id));
  });

  // Handle grid zoom
  Shiny.addCustomMessageHandler('update-grid-zoom', (m) => {
    $(m.id).css('zoom', m.zoom);
  });
} 
