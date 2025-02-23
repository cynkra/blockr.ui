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
  // TBD How to prevent from unselecting node when clickin outside of it.
});
