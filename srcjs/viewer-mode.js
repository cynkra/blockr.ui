export const handleViewerMode = () => {
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
} 
