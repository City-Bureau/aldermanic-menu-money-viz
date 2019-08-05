module Main exposing (Model, Msg(..), WardYear, view)

import Animation exposing (..)
import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Color exposing (Color)
import Csv exposing (Csv)
import Dict exposing (Dict)
import Html exposing (Html, div, option, p, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (..)
import Http
import List.Extra exposing (elemIndex)
import Parser exposing (Parser)
import Path
import Shape exposing (PieConfig, defaultPieConfig)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
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


type alias Model =
    { loading : Bool
    , clock : Clock
    , ward : String
    , years : List Int
    , rows : List WardYear
    , data : Dict String Float
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


w : Float
w =
    990


h : Float
h =
    504


radius : Float
radius =
    min w h / 2


animDur : Float
animDur =
    2500


colors : Array Color
colors =
    Array.fromList
        [ Color.rgb255 152 171 198
        , Color.rgb255 138 137 166
        , Color.rgb255 123 104 136
        , Color.rgb255 107 72 107
        , Color.rgb255 159 92 85
        , Color.rgb255 208 116 60
        , Color.rgb255 255 96 0
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
        |> Dict.map (\k v -> v / (List.length rows |> toFloat))


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
    , innerRadius = 0
    , outerRadius = 100
    , cornerRadius = 0
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
            ( { model | ward = ward, data = data, animations = animations }, Cmd.none )

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
                    getAnimations model.clock Dict.empty data
            in
            ( { model | loading = False, rows = rows, data = data, animations = animations }, Cmd.none )

        Tick dt ->
            ( { model | clock = model.clock + dt }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrameDelta Tick


view : Model -> Html Msg
view model =
    let
        minYear =
            Maybe.withDefault 2012 (model.years |> List.head)

        maxYear =
            Maybe.withDefault 2018 (model.years |> List.reverse |> List.head)

        pieData =
            model.animations
                |> List.map (animate model.clock)
                |> List.map2 WardYearGroup (List.map Tuple.second categories)
                |> Shape.pie pieConfig

        categoryMap =
            Dict.fromList categories

        makeSlice index datum =
            Path.element (Shape.arc datum)
                [ fill <|
                    Fill <|
                        Maybe.withDefault Color.black <|
                            Array.get index colors
                , stroke Color.white
                ]

        makeLabel slice ( key, label ) =
            let
                ( x, y ) =
                    Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 150 }
            in
            text_
                [ transform [ Translate x y ]
                , dy (em 0.35)
                , textAnchor AnchorMiddle
                ]
                [ text
                    (if slice.endAngle - slice.startAngle > 0.2 then
                        label

                     else
                        ""
                    )
                ]
    in
    div []
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
        , svg
            [ viewBox 0 0 w h ]
            [ g [ transform [ Translate (w / 2) (h / 2) ] ]
                [ g [] <| List.indexedMap makeSlice pieData
                , g [] <| List.map2 makeLabel pieData categories
                ]
            ]
        ]
