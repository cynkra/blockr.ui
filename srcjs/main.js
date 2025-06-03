import "../styles/main.scss";
import { setupDashboard } from "./dashboard.js";
import { handleViewerMode } from "./viewer-mode.js";

$(function () {
  setupDashboard()
  handleViewerMode();
  // Hide sidebar toggles
  Shiny.addCustomMessageHandler("hide-sidebars-toggles", (m) => {
    $(`button[aria-controls=\"${m.ns}-dashboard\"]`).css('visibility', 'hidden');
    $(`button[aria-controls=\"${m.ns}-properties\"]`).css('visibility', 'hidden');
  });
});
