module Main exposing (Model, Msg(..), WardYear, view)

import Animation exposing (..)
import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Cmd exposing (Ward, mapLoaded, selectWard, selectedWard)
import Color exposing (Color)
import Csv exposing (Csv)
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, div, node, option, p, select, span, text)
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (..)
import Http
import List.Extra exposing (elemIndex)
import MapStyle exposing (mapStyle)
import Mapbox.Element exposing (..)
import Parser exposing (Parser)
import Path
import Shape exposing (PieConfig, defaultPieConfig)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (onMouseOut, onMouseOver)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), em)


type alias WardYear =
    { ward : String
    , year : Int
    , data : Dict String Float
    }


type alias WardYearGroup =
    { label : String, data : Float }


type Msg
    = UpdateWard String
    | UpdateYears (List Int)
    | LoadData (Result Http.Error String)
    | Tick Float
    | HoverCategory String


type alias Model =
    { loading : Bool
    , clock : Clock
    , ward : String
    , years : List Int
    , rows : List WardYear
    , data : Dict String Float
    , hoverCategory : String
    , animations : List Animation
    }


categories : List ( String, String )
categories =
    [ ( "streets", "CDOT/Streets" )
    , ( "lighting", "Lighting" )
    , ( "parks", "Parks" )
    , ( "schools", "Schools" )
    , ( "cameras", "Cameras" )
    , ( "misc", "Misc." )
    , ( "arts", "Arts" )
    , ( "trees", "Trees/Gardens" )
    , ( "other", "Other" )
    , ( "libraries", "Libraries" )
    ]


wardAldMap : Dict Int String
wardAldMap =
    [ ( 1, "" ), ( 2, "" ) ] |> Dict.fromList


w : Float
w =
    450


h : Float
h =
    450


radius : Float
radius =
    min w h / 2


animDur : Float
animDur =
    1000


colors : Array Color
colors =
    Array.fromList
        [ Color.rgb255 31 118 180
        , Color.rgb255 255 126 14
        , Color.rgb255 44 160 44
        , Color.rgb255 214 39 39
        , Color.rgb255 147 103 189
        , Color.rgb255 140 86 75
        , Color.rgb255 227 119 195
        , Color.rgb255 127 127 127
        , Color.rgb255 189 189 34
        , Color.rgb255 23 189 207
        ]


parseData : Result Http.Error String -> List WardYear
parseData result =
    case result of
        Ok dataText ->
            case Csv.parse dataText of
                Ok { headers, records } ->
                    List.map (\res -> csvRecToWardYear headers res) records

                Err _ ->
                    []

        Err _ ->
            []


csvRecToWardYear : List String -> List String -> WardYear
csvRecToWardYear headers results =
    List.map2 Tuple.pair headers results
        |> List.foldl
            (\( header, result ) rec ->
                case header of
                    "ward" ->
                        { rec | ward = result }

                    "year" ->
                        { rec | year = Maybe.withDefault 0 (String.toInt result) }

                    _ ->
                        { rec
                            | data =
                                Dict.union
                                    (Dict.fromList
                                        [ ( header, Maybe.withDefault 0 (String.toFloat result) )
                                        ]
                                    )
                                    rec.data
                        }
            )
            emptyWardYear


emptyWardYear : WardYear
emptyWardYear =
    { ward = "1"
    , year =
        2018
    , data =
        Dict.empty
    }


groupDataRows : List WardYear -> Dict String Float
groupDataRows rows =
    List.foldl
        (\{ data } groupRec ->
            Dict.merge
                (\k a -> Dict.insert k a)
                (\k a b -> Dict.insert k (a + b))
                (\k b -> Dict.insert k b)
                data
                groupRec
                Dict.empty
        )
        emptyDataRow
        rows


emptyDataRow : Dict String Float
emptyDataRow =
    Dict.fromList
        (List.map2
            Tuple.pair
            (List.map Tuple.first categories)
            (List.repeat (List.length categories) 0)
        )


filterRows : String -> List Int -> List WardYear -> List WardYear
filterRows ward years rows =
    rows
        |> List.filter (.ward >> (==) ward)
        |> List.filter (.year >> (\y -> List.member y years))


getAnimations : Clock -> Dict String Float -> Dict String Float -> List Animation
getAnimations clock prev cur =
    List.map
        (Tuple.first
            >> (\c ->
                    animation clock
                        |> from (Maybe.withDefault 0 (Dict.get c prev))
                        |> to (Maybe.withDefault 0 (Dict.get c cur))
                        |> duration animDur
               )
        )
        categories


compareWithList : List comparable -> comparable -> comparable -> Order
compareWithList baseList a b =
    let
        baseLen =
            List.length baseList + 1
    in
    Basics.compare (Maybe.withDefault baseLen (elemIndex a baseList)) (Maybe.withDefault baseLen (elemIndex b baseList))


pieConfig : PieConfig WardYearGroup
pieConfig =
    { startAngle = 0
    , endAngle = 2 * pi
    , padAngle = 0
    , sortingFn = \a b -> compareWithList (categories |> List.map Tuple.first) a.label b.label
    , valueFn = .data
    , innerRadius = 100
    , outerRadius = 200
    , cornerRadius = 5
    , padRadius = 0
    }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { loading = True
      , clock = 0
      , ward = "1"
      , years = List.range 2012 2018
      , rows = []
      , data = Dict.empty
      , hoverCategory = ""
      , animations = []
      }
    , Http.get
        { url = "data.csv", expect = Http.expectString LoadData }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWard ward ->
            let
                data =
                    filterRows ward model.years model.rows |> groupDataRows

                animations =
                    getAnimations model.clock model.data data
            in
            ( { model | ward = ward, data = data, animations = animations }, selectWard (String.toInt ward |> Maybe.withDefault 0) )

        UpdateYears years ->
            let
                data =
                    filterRows model.ward years model.rows |> groupDataRows

                animations =
                    getAnimations model.clock model.data data
            in
            ( { model | years = years, data = data, animations = animations }, Cmd.none )

        LoadData result ->
            let
                rows =
                    parseData result

                data =
                    filterRows model.ward model.years rows |> groupDataRows

                animations =
                    getAnimations model.clock model.data data
            in
            ( { model | loading = False, rows = rows, data = data, animations = animations }, Cmd.none )

        HoverCategory category ->
            ( { model | hoverCategory = category }, Cmd.none )

        Tick dt ->
            ( { model | clock = model.clock + dt }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ onAnimationFrameDelta Tick, selectedWard (\ward -> UpdateWard (String.fromInt ward)), mapLoaded (\b -> UpdateWard model.ward) ]


view : Model -> Html Msg
view model =
    let
        minYear =
            Maybe.withDefault 2012 (model.years |> List.head)

        maxYear =
            Maybe.withDefault 2018 (model.years |> List.reverse |> List.head)

        wardData =
            model.animations
                |> List.map (animate model.clock)
                |> List.map2 WardYearGroup (List.map Tuple.second categories)

        pieData =
            wardData |> Shape.pie pieConfig

        categoryArr =
            List.map Tuple.second categories |> Array.fromList

        displayData =
            wardData
                |> List.indexedMap (\idx data -> ( data, idx ))

        -- Create an array with a reference to the sorted index
        sortedArray =
            displayData
                |> List.sortBy (Tuple.first >> .data)
                |> List.reverse
                |> List.indexedMap (\sortIdx ( data, idx ) -> ( idx, sortIdx ))
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
                |> Array.fromList

        makeSlice index datum =
            let
                category =
                    Array.get index categoryArr |> Maybe.withDefault ""
            in
            Path.element (Shape.arc datum)
                [ fill <|
                    Fill <|
                        Maybe.withDefault Color.black <|
                            Array.get index colors
                , stroke
                    (if model.hoverCategory == category then
                        Color.black

                     else
                        Color.white
                    )
                , onMouseOver (HoverCategory category)
                , onMouseOut (HoverCategory "")
                ]
    in
    div []
        [ div []
            [ select [ onInput UpdateWard ]
                (List.map
                    (\ward ->
                        option [ value ward, selected (model.ward == ward) ] [ text ward ]
                    )
                    (List.range 1 50 |> List.map String.fromInt)
                )
            , select
                [ onInput (\input -> List.range (String.toInt input |> Maybe.withDefault 2012) maxYear |> UpdateYears) ]
                (List.map
                    (\year ->
                        option [ value year, selected (String.fromInt minYear == year) ] [ text year ]
                    )
                    (List.range 2012 maxYear |> List.map String.fromInt)
                )
            , select
                [ onInput (\input -> List.range minYear (String.toInt input |> Maybe.withDefault 2018) |> UpdateYears) ]
                (List.map
                    (\year ->
                        option [ value year, selected (String.fromInt maxYear == year) ] [ text year ]
                    )
                    (List.range minYear 2018 |> List.map String.fromInt)
                )
            ]
        , div [ style "display" "flex", style "flex" "1" ]
            [ div [ style "width" "100%" ]
                (List.map
                    (\( data, idx ) ->
                        p
                            [ style "position" "absolute"
                            , style "transition" "transform 200ms ease-in-out"
                            , style "transform" ("translateY(" ++ String.fromInt ((Array.get idx sortedArray |> Maybe.withDefault 0) * 25) ++ "px)")
                            ]
                            [ span
                                [ style "display" "inline-block"
                                , style "background-color"
                                    (Array.get idx colors |> Maybe.withDefault Color.black |> Color.toCssString)
                                , style "width" "20px"
                                , style "height" "20px"
                                , style "margin-right" "6px"
                                ]
                                []
                            , span [] [ text (data.label ++ ": $" ++ format usLocale data.data) ]
                            ]
                    )
                    displayData
                )
            , div [ style "width" "100%", style "max-height" "400px" ]
                [ svg
                    [ viewBox 0 0 w h, style "max-width" "100%", style "max-height" "100%" ]
                    [ g [ transform [ Translate (w / 2) (h / 2) ] ]
                        [ g [] <| List.indexedMap makeSlice pieData
                        ]
                    ]
                ]
            , div [ style "position" "relative", style "width" "500px", style "height" "500px" ]
                [ Mapbox.Element.map
                    [ minZoom 9
                    , maxZoom 17
                    , Mapbox.Element.id "map"
                    , eventFeaturesLayers [ "wards" ]
                    ]
                    mapStyle
                ]
            ]
        ]
