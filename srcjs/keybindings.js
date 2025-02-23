export const setKeyBindings = () => {
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
}
