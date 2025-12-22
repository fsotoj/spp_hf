(function () {
  let touchStartX = null;
  let touchEndX = null;
  let touchStartedInSidebar = false;

  function initSidebarSwipe() {

    document.addEventListener(
      "touchstart",
      function (e) {
        const sidebar = document.querySelector(".main-sidebar");
        if (!sidebar) {
          touchStartedInSidebar = false;
          return;
        }

        const rect = sidebar.getBoundingClientRect();
        const t = e.changedTouches[0];
        const x = t.clientX;
        const y = t.clientY;

        // Check if the touch begins inside the sidebar panel
        const insideSidebar =
          x >= rect.left && x <= rect.right && y >= rect.top && y <= rect.bottom;

        touchStartedInSidebar = insideSidebar;

        if (!insideSidebar) return;

        touchStartX = t.screenX;
      },
      { passive: true }
    );

    document.addEventListener(
      "touchend",
      function (e) {
        if (!touchStartedInSidebar) return;

        const t = e.changedTouches[0];
        touchEndX = t.screenX;
        const delta = touchStartX - touchEndX;
        const threshold = 50;

        touchStartedInSidebar = false;

        // Left swipe only
        if (delta > threshold) {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue("sidebar_swipe", "left", { priority: "event" });
          }
        }
      },
      { passive: true }
    );
  }

  // Wait for DOM if needed
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initSidebarSwipe);
  } else {
    initSidebarSwipe();
  }
})();
