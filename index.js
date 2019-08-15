const { Elm } = require("./src/Main.elm");
import { registerCustomElement } from "elm-mapbox";

import * as mapboxgl from "mapbox-gl";
import "mapbox-gl/dist/mapbox-gl.css";

registerCustomElement({
  token: ""
});

document.addEventListener("DOMContentLoaded", () => {
  const app = Elm.Main.init({ flags: {}, node: document.querySelector("main") });
  const map = document.querySelector("elm-mapbox-map")._map;
  const hoverPopup = new mapboxgl.Popup({
    closeButton: false,
    closeOnClick: false,
  });

  let activeWard = null;
  let hoverId = null;

  function handleFeaturesHover(features) {
    if (hoverId) {
      map.setFeatureState({ source: "wards", id: hoverId }, { hover: false });
      hoverId = null;
    }

    features.forEach((f) => {
      hoverId = f.id;
    });

    if (hoverId) {
      map.setFeatureState({ source: "wards", id: hoverId }, { hover: true });
    }
    return hoverId;
  }

  function removePopup(popup) {
    map.getCanvas().style.cursor = "";
    popup.remove();
  }

  function onMouseMove(e) {
    const features = map.queryRenderedFeatures(e.point, { layers: ["wards"]});
    const ward = handleFeaturesHover(features);
    if (features.length > 0) {
      map.getCanvas().style.cursor = "pointer";
      hoverPopup.setLngLat(e.lngLat)
        .setHTML(`<strong>Ward ${ward}</strong>`)
        .addTo(map);
    } else {
      removePopup(hoverPopup);
      map.getCanvas().style.cursor = "";
    }
  }

  function onMouseOut(e) {
    handleFeaturesHover([]);
    removePopup(hoverPopup);
    map.getCanvas().style.cursor = "";
  }

  function onMapClick(e) {
    const features = map.queryRenderedFeatures(e.point, { layers: ["wards"] });
    const ward = handleFeaturesHover(features);
    updateActiveFeature(activeWard, ward);
    activeWard = ward;
    if (features.length === 0) {
      return;
    }
    app.ports.selectedWard.send(ward);
  }

  function updateActiveFeature(prevWard, currWard) {
    if (prevWard) {
      map.setFeatureState({ source: "wards", id: prevWard }, { active: false });
    }
    if (currWard) {
      map.setFeatureState({ source: "wards", id: currWard }, { active: true });
    }
  }

  map.on("mousemove", "wards", onMouseMove);
  map.on("mouseout", "wards", onMouseOut);
  map.on("click", "wards", onMapClick);

  app.ports.mapLoaded.send(true);

  app.ports.selectWard.subscribe(ward => {
    updateActiveFeature(activeWard, ward);
    activeWard = ward;
  });
});
