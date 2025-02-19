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

  // TBD How to prevent from unselecting node when clickin outside of it.

  const captureMouseLocation = (id, event) => {
    let location = { x: event.clientX, y: event.clientY };
    Shiny.setInputValue(
      id,
      location,
      { priority: 'event' }
    );
  }

  Shiny.addCustomMessageHandler("capture-mouse-position", (m) => {
    //document.addEventListener('click', (e) => {
    //  captureMouseLocation(m, e)
    //});
    document.addEventListener('contextmenu', (e) => {
      captureMouseLocation(m, e)
    });
  })

  // Hide node menu whenever we click outside
  $('body').click((e) => {
    if ($(e.target).closest('.node-menu-card').length === 0) {
      $('.node-menu-card').hide();
    }
  })

  Shiny.addCustomMessageHandler("show-node-menu", (m) => {
    // Create element if needed
    if ($(`#${m.id}`).length === 0) {
      let shortId = m.id.split(m.ns)[1];
      let dropTag = `<div class="card node-menu-card" style="width: 18rem; position: absolute;" id="${m.id}">
        <div class="card-header"><h5 class="card-title">Block ${shortId} options</h5></div>
        <div class="card-body">
          <div class="bslib-input-switch form-switch form-check">
            <input 
              id="${m.id}-add_to_grid"
              class="form-check-input"
              type="checkbox"
              role="switch">
            <label class="form-check-label" for="${m.id}-add_to_grid">
              <span>Use in dashboard?</span>
            </label>
            <div class="d-flex justify-content-center align-items-center btn-group" role="group">
              <button class="btn btn-default action-button btn-light" id="${m.id}-append_block" type="button">
                <i class="fas fa-circle-plus" role="presentation" aria-label="circle-plus icon"></i>
              </button>
              <button class="btn btn-default action-button btn-danger" id="${m.id}-remove_block" type="button">
                <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i>
              </button>
            </div>
          </div>
        </div>
      </div>`
      $('body').append(dropTag);

      // Ensure we account for the switch grid state if it exists
      // so that the 2 switches start in sync.
      if (m.value) {
        $(`#${m.id}-add_to_grid`).attr("checked", "")
      }
      Shiny.bindAll($(`#${m.id}`));
    }

    // Show dropdown
    $(`#${m.id}`).show().css({ 'top': m.coords.y, 'left': m.coords.x });
  });
});