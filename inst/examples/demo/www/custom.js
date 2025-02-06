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

  // Viewer mode
  Shiny.addCustomMessageHandler('toggle-view', (m) => {
    if (!m.val) {
      $(m.id)
        .parent('.bslib-sidebar-layout')
        .css('--_sidebar-width', '75%');
      $(m.id).css('border-left', 'var(--_vert-border)');
    } else {
      $(m.id)
        .parent('.bslib-sidebar-layout')
        .css('--_sidebar-width', '100%');
      $(m.id).css('border-left', 'none');
    }
  });

  // Handle grid zoom
  Shiny.addCustomMessageHandler('update-grid-zoom', (m) => {
    $(m.id).css('zoom', m.zoom);
  });

  // Node dropdown menu
  Shiny.addCustomMessageHandler("show-node-menu", (m) => {

    let widget = HTMLWidgets.find(`#${m.parent}`).network;
    let coords = widget.canvasToDOM(m.coords);

    let dropTag = `<div class="dropdown" id="${m.id}">
      <button class="btn btn-secondary dropdown-toggle" type="button" data-bs-toggle="dropdown" aria-expanded="false">
        Dropdown trigger
      </button>
      <ul class="dropdown-menu px-4 py-3">
        <div class="bslib-input-switch form-switch form-check">
          <input 
            id="${m.id}-add_to_grid"
            class="form-check-input shinyjs-resettable shiny-bound-input"
            type="checkbox"
            role="switch"
            data-shinyjs-resettable-id="${m.id}-add_to_grid"
            data-shinyjs-resettable-type="Checkbox"
            data-shinyjs-resettable-value="false">
          <label class="form-check-label" for="${m.id}-add_to_grid">
            <span>Use in dashboard?</span>
          </label>
        </div>
      </ul>
    </div>`
    $(`#${m.parent}`).append(dropTag);
    $(`#${m.id}`).css({ 'top': coords.y, 'left': coords.x });

    new bootstrap.Dropdown($(`#${m.id}`)[0]).show();
  });
});