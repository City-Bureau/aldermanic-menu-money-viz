const { Elm } = require("./src/Main.elm");
import { registerCustomElement } from "elm-mapbox";

import * as mapboxgl from "mapbox-gl";
import "mapbox-gl/dist/mapbox-gl.css";

import "./src/scss/style.scss";

registerCustomElement({
  token: ""
});

document.addEventListener("DOMContentLoaded", () => {
  const app = Elm.Main.init({ flags: {}, node: document.querySelector("main") });
  const map = document.querySelector("elm-mapbox-map")._map;

  map.on("load", () => {
    map.dragRotate.disable();
    map.touchZoomRotate.disableRotation();

    const IS_MOBILE = window.innerWidth <= 500;
    // Disable drag pan on mobile
    if (IS_MOBILE) {
      map.dragPan.disable();
    }

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
      const features = map.queryRenderedFeatures(e.point, { layers: ["wards"] });
      const ward = handleFeaturesHover(features);
      if (features.length > 0) {
        map.getCanvas().style.cursor = "pointer";
        hoverPopup.setLngLat(e.lngLat)
          .setHTML(`<strong>Ward ${ward + 1}</strong>`)
          .addTo(map);
      } else {
        removePopup(hoverPopup);
        map.getCanvas().style.cursor = "";
      }

      if (IS_MOBILE) {
        onMapClick(e);
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
      app.ports.selectedWard.send(ward + 1);
    }

    function updateActiveFeature(prevWard, currWard) {
      if (typeof prevWard === "number") {
        map.setFeatureState({ source: "wards", id: prevWard }, { active: false });
      }
      if (typeof currWard === "number") {
        map.setFeatureState({ source: "wards", id: currWard }, { active: true });
      }
    }

    map.on("mousemove", "wards", onMouseMove);
    map.on("mouseout", "wards", onMouseOut);
    map.on("click", "wards", onMapClick);

    app.ports.mapLoaded.send(true);

    app.ports.selectWard.subscribe(ward => {
      const newWard = isNaN(+ward) ? -1 : +ward - 1;
      updateActiveFeature(activeWard, newWard);
      activeWard = newWard;
    });
  });
});
