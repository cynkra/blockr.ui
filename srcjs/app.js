export const setupApp = () => {
  $.busyLoadFull('show', {
    background: '#5e626b',
    spinner: 'circles',
    animation: 'slide'
  });

  Shiny.addCustomMessageHandler('app-ready', (m) => {
    setTimeout(() => {
      $.busyLoadFull('hide');
    }, 1000);
  });

  // Handle dashboard zoom
  Shiny.addCustomMessageHandler('update-dashboard-zoom', (m) => {
    $(m.id).css('zoom', m.zoom);
  });

  Shiny.addCustomMessageHandler('select-scoutbar-page', (m) => {
    let scoutbarChoice;
    switch (m.value) {
      case 'links':
        scoutbarChoice = 0;
        break;
      case 'serialize':
        scoutbarChoice = 1;
        break;
    }
    $('.scoutbar-cell-item')[scoutbarChoice].click();
  })

  // Offcanvas initial state
  Shiny.addCustomMessageHandler('init-offcanvas-state', (m) => {
    Shiny.setInputValue(m.offcanvas_input, []);
  });

  // Move block UI to offcanvas
  Shiny.addCustomMessageHandler('hide-block', (m) => {
    $(m.offcanvas).find('.offcanvas-body').append($(m.block_id).find('.card'));
    Shiny.shinyapp.$inputValues[m.offcanvas_input].push(m.block_raw_id)
    Shiny.setInputValue(m.offcanvas_input, Shiny.shinyapp.$inputValues[m.offcanvas_input]);
  })

  // Move block UI back to panel
  Shiny.addCustomMessageHandler('show-block', (m) => {
    $(m.panel_id).append($(m.block_id));
    if (!Shiny.shinyapp.$inputValues[m.offcanvas_input].length) return;
    const index = Shiny.shinyapp.$inputValues[m.offcanvas_input].indexOf(m.block_raw_id);
    Shiny.shinyapp.$inputValues[m.offcanvas_input].splice(index, 1);
    Shiny.setInputValue(m.offcanvas_input, Shiny.shinyapp.$inputValues[m.offcanvas_input]);
  })
} 
