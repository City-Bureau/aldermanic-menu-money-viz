module Main exposing (Model, Msg(..), WardYear, view)

import Animation exposing (..)
import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Cmd exposing (Ward, mapLoaded, selectWard, selectedWard)
import Color exposing (Color)
import Csv exposing (Csv)
import Dict exposing (Dict)
import Html exposing (Html, br, div, label, node, option, p, select, span, text)
import Html.Attributes exposing (class, for, id, selected, style, value)
import Html.Events exposing (..)
import Http
import List.Extra exposing (elemIndex)
import MapStyle exposing (mapStyle)
import Mapbox.Element exposing (..)
import NumberSuffix exposing (standardConfig)
import Parser exposing (Parser)


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
    , ( "schools", "Playgrounds" )
    , ( "cameras", "Cameras" )
    , ( "misc", "Misc." )
    , ( "arts", "Arts" )
    , ( "trees", "Trees/Gardens" )
    , ( "other", "Other" )
    , ( "libraries", "Libraries" )
    ]


w : Float
w =
    450


h : Float
h =
    450


padding : Float
padding =
    20


rowHeight : Int
rowHeight =
    50


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
                        { rec | year = result |> String.toInt |> Maybe.withDefault 0 }

                    _ ->
                        { rec
                            | data =
                                Dict.union
                                    (Dict.fromList
                                        [ ( header, result |> String.toFloat |> Maybe.withDefault 0 )
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
        |> List.filter
            (.ward
                >> (if ward == "All" then
                        \wv -> True

                    else
                        (==) ward
                   )
            )
        |> List.filter (.year >> (\y -> List.member y years))


getAnimations : Clock -> Dict String Float -> Dict String Float -> List Animation
getAnimations clock prev cur =
    List.map
        (Tuple.first
            >> (\c ->
                    animation clock
                        |> from (prev |> Dict.get c |> Maybe.withDefault 0)
                        |> to (cur |> Dict.get c |> Maybe.withDefault 0)
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
    Basics.compare (baseList |> elemIndex a |> Maybe.withDefault baseLen) (baseList |> elemIndex b |> Maybe.withDefault baseLen)


selectInput : String -> (String -> Msg) -> (a -> Html Msg) -> List a -> Html Msg
selectInput labelStr msg optionFunc optionList =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label" ] [ label [ class "label", for labelStr ] [ text labelStr ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ div [ class "select" ]
                    [ select [ Html.Attributes.id labelStr, onInput msg ]
                        (List.map optionFunc optionList)
                    ]
                ]
            ]
        ]


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
      , ward = "All"
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
            ( { model | ward = ward, data = data, animations = animations }, selectWard ward )

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
    Sub.batch [ onAnimationFrameDelta Tick, selectedWard UpdateWard, mapLoaded (\b -> UpdateWard model.ward) ]


view : Model -> Html Msg
view model =
    let
        minYear =
            model.years |> List.head |> Maybe.withDefault 2012

        maxYear =
            model.years
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 2018

        wardData =
            model.animations
                |> List.map (animate model.clock)
                |> List.map2 WardYearGroup (List.map Tuple.second categories)

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

        dataSum =
            displayData |> List.map (Tuple.first >> .data) |> List.sum

        containerHeight =
            (categories |> List.length |> (*) rowHeight |> String.fromInt) ++ "px"
    in
    div []
        [ div [ class "field-container" ]
            [ div [ class "ward-input-container" ]
                [ selectInput
                    "Ward"
                    UpdateWard
                    (\ward ->
                        option [ value ward, selected (model.ward == ward) ] [ text ward ]
                    )
                    ([ "All" ] ++ (List.range 1 50 |> List.map String.fromInt))
                ]
            , div [ class "year-inputs-container" ]
                [ selectInput
                    "From"
                    (\input -> List.range (input |> String.toInt |> Maybe.withDefault 2012) maxYear |> UpdateYears)
                    (\year -> option [ value year, selected (minYear |> String.fromInt |> (==) year) ] [ text year ])
                    (List.range 2012 maxYear |> List.map String.fromInt)
                , selectInput
                    "To"
                    (\input -> input |> String.toInt |> Maybe.withDefault 2018 |> List.range minYear |> UpdateYears)
                    (\year ->
                        option [ value year, selected (maxYear |> String.fromInt |> (==) year) ] [ text year ]
                    )
                    (List.range minYear 2018 |> List.map String.fromInt)
                ]
            ]
        , div [ class "display-container" ]
            [ div [ class "legend", style "min-height" containerHeight ]
                ([ span [ class "axis-label" ] [ text "100%" ] ]
                    ++ List.map
                        (\( data, idx ) ->
                            let
                                bgColor =
                                    colors |> Array.get idx |> Maybe.withDefault Color.black |> Color.toCssString
                            in
                            div
                                [ class "legend-item"
                                , style "transform" ("translateY(" ++ String.fromInt ((Array.get idx sortedArray |> Maybe.withDefault 0) * rowHeight) ++ "px)")
                                ]
                                [ div [ class "legend-label" ]
                                    [ span [ style "font-weight" "bold" ] [ text data.label ]
                                    , br [] []
                                    , span [] [ text ("$" ++ NumberSuffix.format { standardConfig | getSuffix = NumberSuffix.suffixStandardShort } data.data) ]
                                    ]
                                , div [ class "legend-bar-container", style "position" "relative" ]
                                    [ div
                                        [ style "width" ((data.data / dataSum |> (*) 100.0 |> String.fromFloat) ++ "%")
                                        , style "background-color" bgColor
                                        , style "height" ((rowHeight |> toFloat |> (*) 0.6 |> String.fromFloat) ++ "px")
                                        ]
                                        []
                                    ]
                                ]
                        )
                        displayData
                )
            , div [ class "map-container", style "min-height" containerHeight ]
                [ Mapbox.Element.map
                    [ minZoom 8
                    , maxZoom 17
                    , Mapbox.Element.id "map"
                    ]
                    mapStyle
                ]
            ]
        ]
