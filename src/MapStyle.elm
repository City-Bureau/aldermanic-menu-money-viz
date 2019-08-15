module MapStyle exposing (mapStyle)

import List
import LngLat exposing (LngLat)
import Mapbox.Expression as E exposing (false, float, str, true)
import Mapbox.Layer as Layer exposing (Layer(..))
import Mapbox.Source as Source
import Mapbox.Style as Style exposing (Style(..))


baseLayers =
    [ Layer.background "background" [ Layer.backgroundColor (E.rgba 242 243 240 1) ]
    , Layer.fill "park"
        "openmaptiles"
        [ Layer.sourceLayer "park"
        , Layer.filter (E.geometryType |> E.isEqual (str "Polygon"))
        , Layer.fillColor (E.rgba 230 233 229 1)
        ]
    , Layer.fill "water"
        "openmaptiles"
        [ Layer.sourceLayer "water"
        , Layer.filter (E.geometryType |> E.isEqual (str "Polygon"))
        , Layer.fillColor (E.rgba 194 200 202 1)
        , Layer.fillAntialias true
        ]
    , Layer.fill "landcover_ice_shelf"
        "openmaptiles"
        [ Layer.sourceLayer "landcover"
        , Layer.maxzoom 8
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Polygon")
                , E.getProperty (str "subclass") |> E.isEqual (str "ice_shelf")
                ]
            )
        , Layer.fillColor (E.rgba 249 249 249 1)
        , Layer.fillOpacity (float 0.7)
        ]
    , Layer.fill "landcover_glacier"
        "openmaptiles"
        [ Layer.sourceLayer "landcover"
        , Layer.maxzoom 8
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Polygon")
                , E.getProperty (str "subclass") |> E.isEqual (str "glacier")
                ]
            )
        , Layer.fillColor (E.rgba 249 249 249 1)
        , Layer.fillOpacity (E.zoom |> E.interpolate (E.Exponential 1) [ ( 0, float 1 ), ( 8, float 0.5 ) ])
        ]
    , Layer.fill "landuse_residential"
        "openmaptiles"
        [ Layer.sourceLayer "landuse"
        , Layer.maxzoom 16
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Polygon")
                , E.getProperty (str "class") |> E.isEqual (str "residential")
                ]
            )
        , Layer.fillColor (E.rgba 234 234 230 1)
        , Layer.fillOpacity (E.zoom |> E.interpolate (E.Exponential 0.6) [ ( 8, float 0.8 ), ( 9, float 0.6 ) ])
        ]
    , Layer.fill "landcover_wood"
        "openmaptiles"
        [ Layer.sourceLayer "landcover"
        , Layer.minzoom 10
        , Layer.filter (E.all [ E.geometryType |> E.isEqual (str "Polygon"), E.getProperty (str "class") |> E.isEqual (str "wood") ])
        , Layer.fillColor (E.rgba 220 224 220 1)
        , Layer.fillOpacity (E.zoom |> E.interpolate (E.Exponential 1) [ ( 8, float 0 ), ( 12, float 1 ) ])
        ]
    , Layer.line "waterway"
        "openmaptiles"
        [ Layer.sourceLayer "waterway"
        , Layer.filter (E.geometryType |> E.isEqual (str "LineString"))
        , Layer.lineColor (E.rgba 189 203 208 1)
        ]
    , Layer.symbol "water_name"
        "openmaptiles"
        [ Layer.sourceLayer "water_name"
        , Layer.filter (E.geometryType |> E.isEqual (str "LineString"))
        , Layer.textColor (E.rgba 157 169 177 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        , Layer.symbolPlacement E.line
        , Layer.textRotationAlignment E.map
        , Layer.symbolSpacing (float 500)
        , Layer.textFont (E.strings [ "Metropolis+Medium+Italic" ])
        , Layer.textSize (float 12)
        ]
    , Layer.fill "building"
        "openmaptiles"
        [ Layer.sourceLayer "building"
        , Layer.minzoom 12
        , Layer.fillColor (E.rgba 234 234 229 1)
        , Layer.fillOutlineColor (E.rgba 219 219 218 1)
        , Layer.fillAntialias true
        ]
    , Layer.line "tunnel_motorway_casing"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 6
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.getProperty (str "brunnel") |> E.isEqual (str "tunnel")
                    , E.getProperty (str "class") |> E.isEqual (str "motorway")
                    ]
                ]
            )
        , Layer.lineColor (E.rgba 213 213 213 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.4) [ ( 5.8, float 0 ), ( 6, float 3 ), ( 20, float 40 ) ])
        , Layer.lineOpacity (float 1)
        , Layer.lineCap E.butt
        , Layer.lineJoin E.miter
        ]
    , Layer.line "tunnel_motorway_inner"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 6
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.getProperty (str "brunnel") |> E.isEqual (str "tunnel")
                    , E.getProperty (str "class") |> E.isEqual (str "motorway")
                    ]
                ]
            )
        , Layer.lineColor (E.rgba 234 234 234 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.4) [ ( 4, float 2 ), ( 6, float 1.3 ), ( 20, float 30 ) ])
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "aeroway-taxiway"
        "openmaptiles"
        [ Layer.sourceLayer "aeroway"
        , Layer.minzoom 12
        , Layer.filter (E.getProperty (str "class") |> E.matchesStr [ ( "taxiway", true ) ] false)
        , Layer.lineColor (E.rgba 224 224 224 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.55) [ ( 13, float 1.8 ), ( 20, float 20 ) ])
        , Layer.lineOpacity (float 1)
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "aeroway-runway-casing"
        "openmaptiles"
        [ Layer.sourceLayer "aeroway"
        , Layer.minzoom 11
        , Layer.filter (E.getProperty (str "class") |> E.matchesStr [ ( "runway", true ) ] false)
        , Layer.lineColor (E.rgba 224 224 224 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.5) [ ( 11, float 6 ), ( 17, float 55 ) ])
        , Layer.lineOpacity (float 1)
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.fill "aeroway-area"
        "openmaptiles"
        [ Layer.sourceLayer "aeroway"
        , Layer.minzoom 4
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Polygon")
                , E.getProperty (str "class") |> E.matchesStr [ ( "runway", true ), ( "taxiway", true ) ] false
                ]
            )
        , Layer.fillOpacity (E.zoom |> E.interpolate (E.Exponential 1) [ ( 13, float 0 ), ( 14, float 1 ) ])
        , Layer.fillColor (E.rgba 255 255 255 1)
        ]
    , Layer.line "aeroway-runway"
        "openmaptiles"
        [ Layer.sourceLayer "aeroway"
        , Layer.minzoom 11
        , Layer.filter
            (E.all
                [ E.getProperty (str "class") |> E.matchesStr [ ( "runway", true ) ] false
                , E.geometryType |> E.isEqual (str "LineString")
                ]
            )
        , Layer.lineColor (E.rgba 255 255 255 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.5) [ ( 11, float 4 ), ( 17, float 50 ) ])
        , Layer.lineOpacity (float 1)
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.fill "road_area_pier"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.filter (E.all [ E.geometryType |> E.isEqual (str "Polygon"), E.getProperty (str "class") |> E.isEqual (str "pier") ])
        , Layer.fillColor (E.rgba 242 243 240 1)
        , Layer.fillAntialias true
        ]
    , Layer.line "road_pier"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.getProperty (str "class") |> E.matchesStr [ ( "pier", true ) ] false
                ]
            )
        , Layer.lineColor (E.rgba 242 243 240 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.2) [ ( 15, float 1 ), ( 17, float 4 ) ])
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "highway_path"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.filter (E.all [ E.geometryType |> E.isEqual (str "LineString"), E.getProperty (str "class") |> E.isEqual (str "path") ])
        , Layer.lineColor (E.rgba 234 234 234 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.2) [ ( 13, float 1 ), ( 20, float 10 ) ])
        , Layer.lineOpacity (float 0.9)
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "highway_minor"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 8
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.getProperty (str "class")
                    |> E.matchesStr [ ( "minor", true ), ( "service", true ), ( "track", true ) ] false
                ]
            )
        , Layer.lineColor (E.rgba 224 224 224 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.55) [ ( 13, float 1.8 ), ( 20, float 20 ) ])
        , Layer.lineOpacity (float 0.9)
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "highway_major_casing"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 11
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.getProperty (str "class")
                    |> E.matchesStr [ ( "primary", true ), ( "secondary", true ), ( "tertiary", true ), ( "trunk", true ) ] false
                ]
            )
        , Layer.lineColor (E.rgba 213 213 213 1)
        , Layer.lineDasharray (E.floats [ 12, 0 ])
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.3) [ ( 10, float 3 ), ( 20, float 23 ) ])
        , Layer.lineCap E.butt
        , Layer.lineJoin E.miter
        ]
    , Layer.line "highway_major_inner"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 11
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.getProperty (str "class")
                    |> E.matchesStr [ ( "primary", true ), ( "secondary", true ), ( "tertiary", true ), ( "trunk", true ) ] false
                ]
            )
        , Layer.lineColor (E.rgba 255 255 255 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.3) [ ( 10, float 2 ), ( 20, float 20 ) ])
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "highway_major_subtle"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.maxzoom 11
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.getProperty (str "class")
                    |> E.matchesStr [ ( "primary", true ), ( "secondary", true ), ( "tertiary", true ), ( "trunk", true ) ] false
                ]
            )
        , Layer.lineColor (E.rgba 216 216 216 0.69)
        , Layer.lineWidth (float 2)
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "highway_motorway_casing"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 6
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.getProperty (str "brunnel") |> E.matchesStr [ ( "bridge", false ), ( "tunnel", false ) ] true
                    , E.getProperty (str "class") |> E.isEqual (str "motorway")
                    ]
                ]
            )
        , Layer.lineColor (E.rgba 213 213 213 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.4) [ ( 5.8, float 0 ), ( 6, float 3 ), ( 20, float 40 ) ])
        , Layer.lineDasharray (E.floats [ 2, 0 ])
        , Layer.lineOpacity (float 1)
        , Layer.lineCap E.butt
        , Layer.lineJoin E.miter
        ]
    , Layer.line "highway_motorway_inner"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 6
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.getProperty (str "brunnel") |> E.matchesStr [ ( "bridge", false ), ( "tunnel", false ) ] true
                    , E.getProperty (str "class") |> E.isEqual (str "motorway")
                    ]
                ]
            )
        , Layer.lineColor
            (E.zoom
                |> E.interpolate (E.Exponential 1) [ ( 5.8, E.rgba 216 216 216 0.53 ), ( 6, E.rgba 255 255 255 1 ) ]
            )
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.4) [ ( 4, float 2 ), ( 6, float 1.3 ), ( 20, float 30 ) ])
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "highway_motorway_subtle"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.maxzoom 6
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.getProperty (str "class") |> E.isEqual (str "motorway")
                ]
            )
        , Layer.lineColor (E.rgba 216 216 216 0.53)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.4) [ ( 4, float 2 ), ( 6, float 1.3 ) ])
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "railway_transit"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 16
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.getProperty (str "class") |> E.isEqual (str "transit")
                    , E.getProperty (str "brunnel") |> E.matchesStr [ ( "tunnel", false ) ] true
                    ]
                ]
            )
        , Layer.lineColor (E.rgba 221 221 221 1)
        , Layer.lineWidth (float 3)
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "railway_transit_dashline"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 16
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.getProperty (str "class") |> E.isEqual (str "transit")
                    , E.getProperty (str "brunnel") |> E.matchesStr [ ( "tunnel", false ) ] true
                    ]
                ]
            )
        , Layer.lineColor (E.rgba 250 250 250 1)
        , Layer.lineWidth (float 2)
        , Layer.lineDasharray (E.floats [ 3, 3 ])
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "railway_service"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 16
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.isEqual (E.getProperty (str "class")) (str "rail")
                    , E.hasProperty (str "service")
                    ]
                ]
            )
        , Layer.lineColor (E.rgba 221 221 221 1)
        , Layer.lineWidth (float 3)
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "railway_service_dashline"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 16
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.getProperty (str "class") |> E.isEqual (str "rail")
                , E.hasProperty (str "service")
                ]
            )
        , Layer.lineColor (E.rgba 250 250 250 1)
        , Layer.lineWidth (float 2)
        , Layer.lineDasharray (E.floats [ 3, 3 ])
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "railway"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 13
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all [ E.not (E.hasProperty (str "service")), E.getProperty (str "class") |> E.isEqual (str "rail") ]
                ]
            )
        , Layer.lineColor (E.rgba 221 221 221 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.3) [ ( 16, float 3 ), ( 20, float 7 ) ])
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "railway_dashline"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 13
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all [ E.not (E.hasProperty (str "service")), E.getProperty (str "class") |> E.isEqual (str "rail") ]
                ]
            )
        , Layer.lineColor (E.rgba 250 250 250 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.3) [ ( 16, float 2 ), ( 20, float 6 ) ])
        , Layer.lineDasharray (E.floats [ 3, 3 ])
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "highway_motorway_bridge_casing"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 6
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.getProperty (str "brunnel") |> E.isEqual (str "bridge")
                    , E.getProperty (str "class") |> E.isEqual (str "motorway")
                    ]
                ]
            )
        , Layer.lineColor (E.rgba 213 213 213 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.4) [ ( 5.8, float 0 ), ( 6, float 5 ), ( 20, float 45 ) ])
        , Layer.lineDasharray (E.floats [ 2, 0 ])
        , Layer.lineOpacity (float 1)
        , Layer.lineCap E.butt
        , Layer.lineJoin E.miter
        ]
    , Layer.line "highway_motorway_bridge_inner"
        "openmaptiles"
        [ Layer.sourceLayer "transportation"
        , Layer.minzoom 6
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.all
                    [ E.getProperty (str "brunnel") |> E.isEqual (str "bridge")
                    , E.getProperty (str "class") |> E.isEqual (str "motorway")
                    ]
                ]
            )
        , Layer.lineColor
            (E.zoom
                |> E.interpolate (E.Exponential 1) [ ( 5.8, E.rgba 216 216 216 0.53 ), ( 6, E.rgba 255 255 255 1 ) ]
            )
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.4) [ ( 4, float 2 ), ( 6, float 1.3 ), ( 20, float 30 ) ])
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    ]


symbolLayers =
    [ Layer.symbol "highway_name_other"
        "openmaptiles"
        [ Layer.sourceLayer "transportation_name"
        , Layer.filter
            (E.all
                [ E.getProperty (str "class") |> E.notEqual (str "motorway")
                , E.geometryType |> E.isEqual (str "LineString")
                ]
            )
        , Layer.textColor (E.rgba 187 187 187 1)
        , Layer.textHaloColor (E.rgba 255 255 255 1)
        , Layer.textTranslate (E.floats [ 0, 0 ])
        , Layer.textHaloWidth (float 2)
        , Layer.textHaloBlur (float 1)
        , Layer.textSize (float 10)
        , Layer.textMaxAngle (float 30)
        , Layer.textTransform E.uppercase
        , Layer.symbolSpacing (float 350)
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.symbolPlacement E.line
        , Layer.textRotationAlignment E.map
        , Layer.textPitchAlignment E.viewport
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str " ")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        ]
    , Layer.symbol "highway_name_motorway"
        "openmaptiles"
        [ Layer.sourceLayer "transportation_name"
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "LineString")
                , E.getProperty (str "class") |> E.isEqual (str "motorway")
                ]
            )
        , Layer.textColor (E.rgba 117 129 145 1)
        , Layer.textHaloColor (E.rgba 255 255 255 1)
        , Layer.textTranslate (E.floats [ 0, 2 ])
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.textSize (float 10)
        , Layer.symbolSpacing (float 350)
        , Layer.textFont (E.strings [ "Metropolis+Light" ])
        , Layer.symbolPlacement E.line
        , Layer.textRotationAlignment E.viewport
        , Layer.textPitchAlignment E.viewport
        , Layer.textField (E.toString (E.getProperty (str "ref")) |> E.toFormattedText)
        ]
    , Layer.line "boundary_state"
        "openmaptiles"
        [ Layer.sourceLayer "boundary"
        , Layer.filter (E.getProperty (str "admin_level") |> E.isEqual (float 4))
        , Layer.lineColor (E.rgba 230 204 207 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.3) [ ( 3, float 1 ), ( 22, float 15 ) ])
        , Layer.lineBlur (float 0.4)
        , Layer.lineDasharray (E.floats [ 2, 2 ])
        , Layer.lineOpacity (float 1)
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.line "boundary_country"
        "openmaptiles"
        [ Layer.sourceLayer "boundary"
        , Layer.filter (E.getProperty (str "admin_level") |> E.isEqual (float 2))
        , Layer.lineColor (E.rgba 230 204 207 1)
        , Layer.lineWidth (E.zoom |> E.interpolate (E.Exponential 1.1) [ ( 3, float 1 ), ( 22, float 20 ) ])
        , Layer.lineBlur (E.zoom |> E.interpolate (E.Exponential 1) [ ( 0, float 0.4 ), ( 22, float 4 ) ])
        , Layer.lineOpacity (float 1)
        , Layer.lineCap E.rounded
        , Layer.lineJoin E.rounded
        ]
    , Layer.symbol "place_other"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 14
        , Layer.filter
            (E.all
                [ E.getProperty (str "class")
                    |> E.matchesStr [ ( "continent", true ), ( "hamlet", true ), ( "neighbourhood", true ), ( "isolated_dwelling", true ) ] false
                , E.geometryType |> E.isEqual (str "Point")
                ]
            )
        , Layer.textColor (E.rgba 117 129 145 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.textSize (float 10)
        , Layer.textTransform E.uppercase
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textJustify E.center
        , Layer.textOffset (E.floats [ 0.5, 0 ])
        , Layer.textAnchor E.center
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        ]
    , Layer.symbol "place_suburb"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 15
        , Layer.filter (E.all [ E.geometryType |> E.isEqual (str "Point"), E.getProperty (str "class") |> E.isEqual (str "suburb") ])
        , Layer.textColor (E.rgba 117 129 145 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.textSize (float 10)
        , Layer.textTransform E.uppercase
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textJustify E.center
        , Layer.textOffset (E.floats [ 0.5, 0 ])
        , Layer.textAnchor E.center
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        ]
    , Layer.symbol "place_village"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 14
        , Layer.filter (E.all [ E.geometryType |> E.isEqual (str "Point"), E.getProperty (str "class") |> E.isEqual (str "village") ])
        , Layer.textColor (E.rgba 117 129 145 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.iconOpacity (float 0.7)
        , Layer.textSize (float 10)
        , Layer.textTransform E.uppercase
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textJustify E.left
        , Layer.textOffset (E.floats [ 0.5, 0.2 ])
        , Layer.iconSize (float 0.4)
        , Layer.textAnchor E.left
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        ]
    , Layer.symbol "place_town"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 15
        , Layer.filter (E.all [ E.geometryType |> E.isEqual (str "Point"), E.getProperty (str "class") |> E.isEqual (str "town") ])
        , Layer.textColor (E.rgba 117 129 145 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.iconOpacity (float 0.7)
        , Layer.textSize (float 10)
        , Layer.iconImage (E.zoom |> E.step (str "circle-11") [ ( 8, str "" ) ])
        , Layer.textTransform E.uppercase
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textJustify E.left
        , Layer.textOffset (E.floats [ 0.5, 0.2 ])
        , Layer.iconSize (float 0.4)
        , Layer.textAnchor (E.zoom |> E.step E.left [ ( 8, E.center ) ])
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        ]
    , Layer.symbol "place_city"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 14
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Point")
                , E.all
                    [ E.getProperty (str "capital") |> E.notEqual (float 2)
                    , E.getProperty (str "class") |> E.isEqual (str "city")
                    , E.getProperty (str "rank") |> E.greaterThan (float 3)
                    ]
                ]
            )
        , Layer.textColor (E.rgba 117 129 145 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.iconOpacity (float 0.7)
        , Layer.textSize (float 10)
        , Layer.iconImage (E.zoom |> E.step (str "circle-11") [ ( 8, str "" ) ])
        , Layer.textTransform E.uppercase
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textJustify E.left
        , Layer.textOffset (E.floats [ 0.5, 0.2 ])
        , Layer.iconSize (float 0.4)
        , Layer.textAnchor (E.zoom |> E.step E.left [ ( 8, E.center ) ])
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        ]
    , Layer.symbol "place_capital"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 12
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Point")
                , E.all
                    [ E.getProperty (str "capital") |> E.isEqual (float 2)
                    , E.getProperty (str "class") |> E.isEqual (str "city")
                    ]
                ]
            )
        , Layer.textColor (E.rgba 117 129 145 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.iconOpacity (float 0.7)
        , Layer.textSize (float 14)
        , Layer.iconImage (E.zoom |> E.step (str "star-11") [ ( 8, str "" ) ])
        , Layer.textTransform E.uppercase
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textJustify E.left
        , Layer.textOffset (E.floats [ 0.5, 0.2 ])
        , Layer.iconSize (float 1)
        , Layer.textAnchor (E.zoom |> E.step E.left [ ( 8, E.center ) ])
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        ]
    , Layer.symbol "place_city_large"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 12
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Point")
                , E.all
                    [ E.getProperty (str "capital") |> E.notEqual (float 2)
                    , E.getProperty (str "rank") |> E.lessThanOrEqual (float 3)
                    , E.getProperty (str "class") |> E.isEqual (str "city")
                    ]
                ]
            )
        , Layer.textColor (E.rgba 117 129 145 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.iconOpacity (float 0.7)
        , Layer.textSize (float 14)
        , Layer.iconImage (E.zoom |> E.step (str "circle-11") [ ( 8, str "" ) ])
        , Layer.textTransform E.uppercase
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textJustify E.left
        , Layer.textOffset (E.floats [ 0.5, 0.2 ])
        , Layer.iconSize (float 0.4)
        , Layer.textAnchor (E.zoom |> E.step E.left [ ( 8, E.center ) ])
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        ]
    , Layer.symbol "place_state"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 12
        , Layer.filter (E.all [ E.geometryType |> E.isEqual (str "Point"), E.getProperty (str "class") |> E.isEqual (str "state") ])
        , Layer.textColor (E.rgba 113 129 144 1)
        , Layer.textHaloColor (E.rgba 242 243 240 1)
        , Layer.textHaloWidth (float 1)
        , Layer.textHaloBlur (float 1)
        , Layer.textField
            (E.getProperty (str "name:latin")
                |> E.append (str "\n")
                |> E.append (E.getProperty (str "name:nonlatin"))
                |> E.toFormattedText
            )
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textTransform E.uppercase
        , Layer.textSize (float 10)
        ]
    , Layer.symbol "place_country_other"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 8
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Point")
                , E.getProperty (str "class") |> E.isEqual (str "country")
                , E.not (E.hasProperty (str "iso_a2"))
                ]
            )
        , Layer.textHaloWidth (float 1.4)
        , Layer.textHaloColor (E.rgba 236 236 234 0.7)
        , Layer.textColor (E.zoom |> E.interpolate (E.Exponential 1) [ ( 3, E.rgba 157 169 177 1 ), ( 4, E.rgba 153 153 153 1 ) ])
        , Layer.textField (E.toString (E.getProperty (str "name:latin")) |> E.toFormattedText)
        , Layer.textFont (E.strings [ "Metropolis+Light+Italic" ])
        , Layer.textTransform E.uppercase
        , Layer.textSize (E.zoom |> E.interpolate (E.Exponential 1) [ ( 0, float 9 ), ( 6, float 11 ) ])
        ]
    , Layer.symbol "place_country_minor"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 8
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Point")
                , E.getProperty (str "class") |> E.isEqual (str "country")
                , E.getProperty (str "rank") |> E.greaterThanOrEqual (float 2)
                , E.hasProperty (str "iso_a2")
                ]
            )
        , Layer.textHaloWidth (float 1.4)
        , Layer.textHaloColor (E.rgba 236 236 234 0.7)
        , Layer.textColor (E.zoom |> E.interpolate (E.Exponential 1) [ ( 3, E.rgba 157 169 177 1 ), ( 4, E.rgba 153 153 153 1 ) ])
        , Layer.textField (E.toString (E.getProperty (str "name:latin")) |> E.toFormattedText)
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textTransform E.uppercase
        , Layer.textSize (E.zoom |> E.interpolate (E.Exponential 1) [ ( 0, float 10 ), ( 6, float 12 ) ])
        ]
    , Layer.symbol "place_country_major"
        "openmaptiles"
        [ Layer.sourceLayer "place"
        , Layer.maxzoom 6
        , Layer.filter
            (E.all
                [ E.geometryType |> E.isEqual (str "Point")
                , E.getProperty (str "rank") |> E.lessThanOrEqual (float 1)
                , E.getProperty (str "class") |> E.isEqual (str "country")
                , E.hasProperty (str "iso_a2")
                ]
            )
        , Layer.textHaloWidth (float 1.4)
        , Layer.textHaloColor (E.rgba 236 236 234 0.7)
        , Layer.textColor (E.zoom |> E.interpolate (E.Exponential 1) [ ( 3, E.rgba 157 169 177 1 ), ( 4, E.rgba 153 153 153 1 ) ])
        , Layer.textField (E.toString (E.getProperty (str "name:latin")) |> E.toFormattedText)
        , Layer.textFont (E.strings [ "Metropolis+Regular" ])
        , Layer.textTransform E.uppercase
        , Layer.textSize (E.zoom |> E.interpolate (E.Exponential 1.4) [ ( 0, float 10 ), ( 3, float 12 ), ( 4, float 14 ) ])
        , Layer.textAnchor E.center
        ]
    ]


dataLayers =
    [ Layer.fill "wards"
        "wards"
        [ Layer.fillOutlineColor (E.rgba 0 0 0 0)
        , Layer.fillColor (E.rgba 228 26 28 1)
        , Layer.fillOpacity (E.ifElse (E.toBool (E.featureState (str "active"))) (float 0.9) (float 0.1))
        ]
    , Layer.line "ward-lines"
        "wards"
        [ Layer.lineColor (E.rgba 228 26 28 1)
        , Layer.lineOpacity (float 0.5)
        , Layer.lineWidth (E.ifElse (E.toBool (E.featureState (str "hover"))) (float 2.5) (float 0.75))
        ]
    ]


mapStyle : Style
mapStyle =
    Style
        { transition = Style.defaultTransition
        , light = Style.defaultLight
        , layers =
            baseLayers ++ dataLayers ++ symbolLayers
        , sources =
            [ Source.vector "openmaptiles"
                [ "https://s3.us-east-2.amazonaws.com/city-bureau-openmaptiles/{z}/{x}/{y}.pbf" ]
                [ Source.maxzoom 14
                , Source.attribution "<a href=\"https://www.maptiler.com/copyright/\" target=\"_blank\">&copy; MapTiler</a> <a href=\"https://www.openstreetmap.org/copyright\" target=\"_blank\">&copy; OpenStreetMap contributors</a>"
                ]
            , Source.geoJSONFromUrl "wards" "wards.geojson" [Source.generateIds]
            ]
        , misc =
            [ Style.sprite "https://s3.us-east-2.amazonaws.com/city-bureau-openmaptiles/positron/sprite"
            , Style.glyphs "https://s3.us-east-2.amazonaws.com/city-bureau-openmaptiles/fonts/{fontstack}/{range}.pbf"
            , Style.defaultCenter <| LngLat -87.6597 41.8369
            , Style.defaultZoomLevel 10
            , Style.name "Positron"
            ]
        }
