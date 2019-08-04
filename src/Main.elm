module Main exposing (Model, Msg(..), WardYear, view)

import Array exposing (Array)
import Browser
import Color exposing (Color)
import Csv exposing (Csv)
import Dict exposing (Dict)
import Html exposing (Html, div, p, text)
import Http
import Parser exposing (Parser)
import Path
import Shape exposing (defaultPieConfig)
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


type Msg
    = UpdateWard String
    | UpdateYears (List Int)
    | LoadData (Result Http.Error String)


type alias Model =
    { loading : Bool, ward : String, years : List Int, data : List WardYear }


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
                            | data = Dict.union (Dict.fromList [ ( header, Maybe.withDefault 0 (String.toFloat result) ) ]) rec.data
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
    Dict.fromList (List.map2 Tuple.pair (List.map Tuple.first categories) (List.repeat (List.length categories) 0))


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { loading = True, ward = "1", years = [ 2012, 2013, 2014, 2015, 2016, 2017, 2018 ], data = [] }
    , Http.get
        { url = "data.csv", expect = Http.expectString LoadData }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWard ward ->
            ( { model | ward = ward }, Cmd.none )

        UpdateYears years ->
            ( { model | years = years }, Cmd.none )

        LoadData result ->
            ( { model | loading = False, data = parseData result }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        dataRows =
            model.data |> groupDataRows >> Dict.toList

        pieData =
            dataRows |> List.map Tuple.second |> Shape.pie defaultPieConfig

        categoryMap =
            Dict.fromList categories

        makeSlice index datum =
            Path.element (Shape.arc datum) [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors, stroke Color.white ]

        makeLabel slice ( label, value ) =
            let
                ( x, y ) =
                    Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }
            in
            text_
                [ transform [ Translate x y ]
                , dy (em 0.35)
                , textAnchor AnchorMiddle
                ]
                [ text (Maybe.withDefault "" (Dict.get label categoryMap)) ]
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ g [] <| List.indexedMap makeSlice pieData
            , g [] <| List.map2 makeLabel pieData dataRows
            ]
        ]
