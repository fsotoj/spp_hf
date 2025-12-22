/******************************************************
 * UNIVERSAL SHINY INPUT TRACKING
 * Sends event: "input_change"
 ******************************************************/

function safeGtag() {
  if (typeof gtag === "function") {
    gtag.apply(null, arguments);
  } else {
    console.warn("gtag not ready, event skipped:", arguments);
  }
}

// This captures ALL input changes:
$(document).on("shiny:inputchanged", function (e) {

  // Ignore internal Shiny stuff
  if (!e.name || e.name.startsWith(".clientdata")) return;
  if (e.name.includes("shinydashboard.sidebar")) return;

  safeGtag("event", "input_change", {
    input_id: e.name,
    value: typeof e.value === "object"
      ? JSON.stringify(e.value)
      : String(e.value)
  });

});
