Shiny.addCustomMessageHandler("addExportButton", function (message) {
  const selector = "#" + message.mapId;
  const widget = HTMLWidgets.find(selector);

  if (!widget || typeof widget.getMap !== "function") {
    console.warn("EasyPrint: Leaflet widget not found for", selector);
    return;
  }

  const map = widget.getMap();
  if (!map || typeof L === "undefined") {
    console.warn("EasyPrint: Leaflet library not ready for", selector);
    return;
  }

  // CLEANUP: Remove any old EasyPrint wrapper or control
  const oldWrapper = document.querySelector("#custom-easyprint-wrapper");
  if (oldWrapper) oldWrapper.remove();
  if (map._easyPrintControl) {
    try {
      map.removeControl(map._easyPrintControl);
    } catch (e) {
      console.warn("EasyPrint cleanup warning:", e);
    }
    map._easyPrintControl = null;
  }

  // Create EasyPrint control
  map._easyPrintControl = L.easyPrint({
    title: "Download map",
    position: "bottomright",
    filename: "spp_map",
    exportOnly: true,
    hideControlContainer: false,
    sizeModes: ["A4Landscape"]
  }).addTo(map);
  
  // ---  Move EasyPrint button right beside the Play/Pause button (same corner)
  const ctrl = map.getContainer().querySelector(".leaflet-control-easyPrint");
  
  function moveButtonNextToPlay() {
    const playBtn = document.querySelector(".slider-animate-button");
    if (playBtn && ctrl) {
      // cleanup any old wrapper
      const oldWrapper = document.querySelector("#custom-easyprint-wrapper");
      if (oldWrapper) oldWrapper.remove();
  
      // small inline wrapper to keep spacing and alignment
      const wrapper = document.createElement("span");
      wrapper.id = "custom-easyprint-wrapper";
      Object.assign(wrapper.style, {
        display: "inline-block",
        //marginTop: "55px",
        verticalAlign: "middle",
      });
  
      // insert right after the Play button
      playBtn.parentNode.insertBefore(wrapper, playBtn.nextSibling);
      wrapper.appendChild(ctrl);
  
      // stop observing once placed
      observer.disconnect();
    }
  }
  
  // Wait until Play button appears
  const observer = new MutationObserver(moveButtonNextToPlay);
  observer.observe(document.body, { childList: true, subtree: true });
  
  // Try immediately in case it already exists
  moveButtonNextToPlay();
  


  // ✅ Add print events
  map.on("easyPrint-start", function () {
    const ctrl = document.querySelector(".leaflet-control-easyPrint");
    if (ctrl) {
      ctrl.style.visibility = "hidden";
      ctrl.style.opacity = "0";
    }
    
    
    
    const legend = map.getContainer().querySelector(".info.legend.leaflet-control");
    if (legend) {
        //legend.style.width = rect.width + "px"; // fijar ancho actual
      legend.style.whiteSpace = "nowrap";     // evita quiebre de líneas
    }

    const textOverlay = document.createElement("div");
    textOverlay.id = "map-print-text";
    textOverlay.textContent =
      message.printText || "Source: Subnational Politics Project (SPP), 2025.";
    Object.assign(textOverlay.style, {
      position: "absolute",
      bottom: "0px",
      right: "10px",
      color: "#222",
      backgroundColor: "rgba(255,255,255,0.8)",
      padding: "3px 8px",
      fontSize: "12px",
      borderRadius: "4px",
      fontFamily: "sans-serif",
      zIndex: "99999"
    });
    map.getContainer().appendChild(textOverlay);
  });

  map.on("easyPrint-finished", function () {
    const legend = map.getContainer().querySelector(".info.legend.leaflet-control");
    legend.style.whiteSpace = "normal";     // evita quiebre de líneas
    const textOverlay = document.getElementById("map-print-text");
    if (textOverlay) textOverlay.remove();

    const ctrl = document.querySelector(".leaflet-control-easyPrint");
    if (ctrl) {
      setTimeout(() => {
        ctrl.style.visibility = "visible";
        ctrl.style.opacity = "1";
      }, 100);
    }
  });

});
