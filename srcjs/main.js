import "../styles/main.scss";
import { setKeyBindings } from "./keybindings.js";
import { initNetwork } from "./init-network.js";
import { setupBlocksGrid } from "./move-blocks.js";
import { manageNodeMenu } from "./node-menu.js";
import { handleViewerMode } from "./viewer-mode.js";

$(function () {
  setKeyBindings();
  initNetwork();
  setupBlocksGrid();
  manageNodeMenu();
  handleViewerMode();
  // Unselect all nodes when removing a block.
  Shiny.addCustomMessageHandler("reset-node-selection", (m) => {
    let widget = HTMLWidgets.find(m.id).network;
    widget.unselectAll();
  })
});
