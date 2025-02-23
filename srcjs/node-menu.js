const captureMouseLocation = (id, event) => {
  let location = { x: event.clientX, y: event.clientY };
  Shiny.setInputValue(
    id,
    location,
    { priority: 'event' }
  );
}

export const manageNodeMenu = () => {
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
  });

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
                </div>
                <div class="btn-group btn-group-sm d-flex justify-content-center align-items-center" role="group">
                  <button class="btn action-button btn-light" id="${m.id}-append_block" type="button">
                    <i class="fas fa-circle-plus" role="presentation" aria-label="circle-plus icon"></i>
                  </button>
                  <button class="btn action-button btn-danger" id="${m.id}-remove_block" type="button">
                    <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i>
                  </button>
                </div>
              </div>
            </div>`
      $('body').append(dropTag);

      // Ensure we account for the switch grid state if it exists
      // so that the 2 switches start in sync.
      if (m.value) {
        $(`#${m.id}-add_to_grid`).attr("checked", "")
      }

      $(`#${m.id} .btn-group`).click(() => {
        $('.node-menu-card').hide();
      });

      Shiny.bindAll($(`#${m.id}`));
    }

    // Show dropdown
    $(`#${m.id}`).show().css({ 'top': m.coords.y, 'left': m.coords.x });
  });
}
