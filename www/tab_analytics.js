/******************************************************
 * SAFE GTAG WRAPPER
 ******************************************************/
function safeGtag() {
  if (typeof gtag === "function") {
    gtag.apply(null, arguments);
  } else {
    console.warn("gtag not ready, event skipped:", arguments);
  }
}

/******************************************************
 * TAB STATE
 ******************************************************/
let lastTab = "map_tab";
let lastTabStart = Date.now();
let isInitialMap = true;

/******************************************************
 * ACTIVE TIME TRACKING (IDLE FREEZE)
 ******************************************************/
let idleThreshold = 60000; // 60s inactivity → idle
let idleTimer = null;
let isIdle = false;
let accumulatedActiveSeconds = 0;

// Start idle timer and detect user activity
function resetIdleTimer() {
  if (isIdle) {
    // User returns from idle → resume timing
    isIdle = false;
    lastTabStart = Date.now();
  }

  if (idleTimer) clearTimeout(idleTimer);

  idleTimer = setTimeout(() => {
    // User becomes idle → freeze timer
    isIdle = true;

    const now = Date.now();
    accumulatedActiveSeconds += Math.round((now - lastTabStart) / 1000);
  }, idleThreshold);
}

// Register activity events
["mousemove", "keydown", "click", "scroll", "touchstart"].forEach(ev => {
  window.addEventListener(ev, resetIdleTimer, { passive: true });
});

// Initialize idle tracking immediately
resetIdleTimer();

// Compute active seconds for the current tab
function getActiveSeconds() {
  if (isIdle) {
    return accumulatedActiveSeconds;
  } else {
    const now = Date.now();
    return (
      accumulatedActiveSeconds +
      Math.round((now - lastTabStart) / 1000)
    );
  }
}

/******************************************************
 * INITIAL OPEN EVENT
 ******************************************************/
safeGtag("event", "tab_open", { tab_name: "map_tab_initial" });

/******************************************************
 * TAB CHANGE TRACKING
 ******************************************************/
$(document).on("shiny:inputchanged", function (e) {
  if (e.name === "tabs") {
    const seconds = getActiveSeconds();

    // Determine correct label for LEAVING tab
    let tabLabel =
      lastTab === "map_tab" && isInitialMap
        ? "map_tab_initial"
        : lastTab;

    // Send tab duration
    safeGtag("event", "tab_duration", {
      tab_name: tabLabel,
      seconds: seconds
    });

    /*************** Prepare next tab ***************/
    lastTab = e.value;
    accumulatedActiveSeconds = 0;
    lastTabStart = Date.now();
    resetIdleTimer();

    // Label for ENTERING tab
    let nextLabel =
      lastTab === "map_tab" && isInitialMap
        ? "map_tab_initial"
        : lastTab;

    // First time leaving map_tab_initial
    isInitialMap = false;

    // Send tab_open event
    safeGtag("event", "tab_open", {
      tab_name: nextLabel
    });
  }
});

/******************************************************
 * WINDOW CLOSE / REFRESH
 ******************************************************/
window.addEventListener("beforeunload", function () {
  const seconds = getActiveSeconds();

  let tabLabel =
    lastTab === "map_tab" && isInitialMap
      ? "map_tab_initial"
      : lastTab;

  const payload = {
    client_id: window.gtagClientId || "",
    events: [
      {
        name: "tab_duration",
        params: {
          tab_name: tabLabel,
          seconds: seconds
        }
      }
    ]
  };

  navigator.sendBeacon(
    window.location.pathname + "ga",
    JSON.stringify(payload)
  );
});
