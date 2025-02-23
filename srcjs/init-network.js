export const initNetwork = () => {
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
} 
