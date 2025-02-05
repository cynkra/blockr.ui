$(function () {

  function getWidget(id) {
    var htmlWidgetsObj = HTMLWidgets.find("#" + id);
    var widgetObj;
    if (typeof htmlWidgetsObj !== "undefined") {
      widgetObj = htmlWidgetsObj.getWidget();
    }
    return widgetObj;
  }

  Shiny.addCustomMessageHandler("bind-network-keyboard-shortcuts", (m) => {

    let widget = HTMLWidgets.find(m.id).network;

    $(document).on("keydown", (e) => {
      // Shift + E
      if (e.shiftKey && e.keyCode == 69) {
        widget.addEdgeMode();
        $("button.vis-back").hide();
        $(".vis-separator-line").hide();
      }
    })

    $(document).on("keydown", (e) => {
      // esc
      if (e.key === "Escape") {
        widget.disableEditMode();
        widget.enableEditMode();
        $(".vis-close").hide();
        $(".vis-edit").hide();
        $(".vis-separator-line").hide();
      }
    })
  })

  // Hide visNetwork selection dropdown
  Shiny.addCustomMessageHandler("hide-vis-dropdown", (m) => {
    $(m).css("visibility", "hidden")
  })

  // Handle manipulation ui visibility
  Shiny.addCustomMessageHandler("toggle-manipulation-ui", (m) => {
    $(".vis-close").hide();
    $(".vis-edit").hide();
    $(".vis-separator-line").hide();
    if (m.value) {
      $(".vis-manipulation").show();
    } else {
      $(".vis-manipulation").hide();
    }
  })

  // Hide sidebar toggles
  Shiny.addCustomMessageHandler("hide-sidebars-toggles", (m) => {
    $(`button[aria-controls=\"${m.ns}-dashboard\"]`).css('visibility', 'hidden');
    $(`button[aria-controls=\"${m.ns}-properties\"]`).css('visibility', 'hidden');
  });

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
});