export const setupDashboard = () => {
  // Handle dashboard zoom
  Shiny.addCustomMessageHandler('update-dashboard-zoom', (m) => {
    $(m.id).css('zoom', m.zoom);
  });

  // Manage dock mode
  //Shiny.addCustomMessageHandler("move-block-in-dock", (m) => {
  //  let dock = HTMLWidgets.find(m.dock_id);
  //  // Set active panel so that it is mounted in the DOM
  //  // before moving it to sidebar (if there are tabs
  //  // only the active tabs is rendered in the DOM)
  //  dock.getWidget().getPanel(m.panel_id).api.setActive();
  //  $(`${m.dock_id}-${m.panel_id}`).append($(m.block_id));
  //  if ($(m.block_id).is(":hidden")) {
  //    $(m.block_id).show();
  //  }
  //})

  //Shiny.addCustomMessageHandler("remove-block-from-dock", (m) => {
  //  let dock = HTMLWidgets.find(m.dock_id);
  //  // Set active panel so that it is mounted in the DOM
  //  // before moving it to sidebar (if there are tabs
  //  // only the active tabs is rendered in the DOM)
  //  dock.getWidget().getPanel(m.panel_id).api.setActive();
  //  $(m.id).append($(`${m.dock_id}-${m.panel_id} > * `));
  //})
} 
