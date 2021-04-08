module Main exposing (..)

import Browser exposing (Document)
import Element exposing (Element, column, el, fill, height, px, table, text, width, padding, rgb255)
import Element.Font as Font
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder, field)
import List.Extra as L
import Plot exposing (circle, decentPositions, dots, hintDot, interval, simpleLabel, simpleLine, simpleTick, viewCircle, viewSeries)
import Svg
import Time exposing (Posix)


main =
    Browser.document
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- TYPES


type alias InitFlags =
    { incidentsRaw : List GunIncidentRaw
    , gunLawsRaw : List GunLawRaw
    }


type alias GunIncidentRaw =
    { id : Int
    , date : Posix
    , state : String
    , cityOrCounty : String
    , killed : Int
    , injured : Int
    }


type alias GunLawRaw =
    { state : String
    , rating : Int
    }


type alias StateIncidents =
    { state : String
    , rating : Int
    , incidents : Int
    }



-- INIT


type alias Model =
    { gunIncidentsRaw : List GunIncidentRaw
    , gunLawsRaw : List GunLawRaw
    , stateIncidents : List StateIncidents
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        { incidentsRaw, gunLawsRaw } =
            Result.withDefault { incidentsRaw = [], gunLawsRaw = [] } (Decode.decodeValue initFlagsDecoder flags)
    in
    ( { gunIncidentsRaw = incidentsRaw
      , gunLawsRaw = gunLawsRaw
      , stateIncidents = getStateData gunLawsRaw incidentsRaw
      }
    , Cmd.none
    )


getStateData : List GunLawRaw -> List GunIncidentRaw -> List StateIncidents
getStateData gunLawsRaw incidentsRaw =
    calculateIncidentsPerState incidentsRaw
        |> updateStateGunRatings gunLawsRaw
        |> List.sortBy .state


calculateIncidentsPerState : List GunIncidentRaw -> List StateIncidents
calculateIncidentsPerState incidents =
    let
        reducer : GunIncidentRaw -> List StateIncidents -> List StateIncidents
        reducer incident stateIncidents =
            case L.findIndex (\stateIncident -> incident.state == stateIncident.state) stateIncidents of
                Nothing ->
                    StateIncidents incident.state 0 1 :: stateIncidents

                Just index ->
                    L.updateAt index (\state -> { state | incidents = state.incidents + 1 }) stateIncidents
    in
    List.foldl reducer [] incidents


updateStateGunRatings : List GunLawRaw -> List StateIncidents -> List StateIncidents
updateStateGunRatings gunLaws stateIncidents =
    let
        updateState gunLaw =
            case L.find (\state -> state.state == gunLaw.state) stateIncidents of
                Just state ->
                    { state | rating = gunLaw.rating }

                Nothing ->
                    StateIncidents gunLaw.state gunLaw.rating 0
    in
    List.map updateState gunLaws



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "guns"
    , body = [ layout model ]
    }


layout : Model -> Html Msg
layout model =
    Element.layout [] <|
        column [ width fill, ]
            [ el [ height (px 400), width fill ] <| Element.html (scatterChart model.stateIncidents)
            , dataTable model.stateIncidents
            ]

-- Chart

scatterChart : List StateIncidents -> Html msg
scatterChart data =
    let
        toDataPoints state =
            let
                incidents =
                    toFloat state.incidents

                rating =
                    toFloat state.rating
            in
            circle
                incidents
                rating
    in
    Plot.viewSeriesCustom
        customizedScatterPlot
        [ dots (List.map toDataPoints) ]
        data

customizedScatterPlot : Plot.PlotCustomizations msg
customizedScatterPlot =
    let
        defaultCustomization =
            Plot.defaultSeriesPlotCustomizations

        attributes =
            [ Attr.style "height" "100%", Attr.style "width" "100%" ]

        horizontalAxis =
            Plot.customAxis <|
                \summary ->
                    { position = Plot.closestToZero
                    , axisLine = Just (simpleLine summary)
                    , ticks = List.map simpleTick (decentPositions summary)
                    , labels = List.map simpleLabel (interval 0 20 summary)
                    , flipAnchor = False
                    }
    in
    { defaultCustomization | attributes = attributes, horizontalAxis = horizontalAxis }

-- Data Table

dataTable : List StateIncidents -> Element msg
dataTable stateIncidents =
    table [ width fill ]
                { data = stateIncidents
                , columns =
                    [ { header = tableHeader "State"
                      , width = fill
                      , view = \state -> text state.state
                      }
                    , { header = tableHeader "Gun Law Strictness"
                      , width = fill
                      , view = \state -> text (String.fromInt state.rating)
                      }
                    , { header = tableHeader "Incidents"
                      , width = fill
                      , view = \state -> text (String.fromInt state.incidents)
                      }
                    ]
                }

tableHeader : String -> Element msg
tableHeader header =
    el 
        [ Font.size 20
        , Font.heavy
        , padding 10
        , Border.width 1
        , Border.color (rgb255 0 0 0)
        ] 
        (text header)


-- Helpers
borderBottom -> 
-- DECODERS


posixDecoder : Decoder Posix
posixDecoder =
    Decode.map Time.millisToPosix Decode.int


initFlagsDecoder : Decoder InitFlags
initFlagsDecoder =
    Decode.map2 InitFlags
        (field "incidents" gunIncidentsDecoder)
        (field "gunLaws" gunLawsDecoder)


gunLawsDecoder : Decoder (List GunLawRaw)
gunLawsDecoder =
    Decode.list <|
        Decode.map2 GunLawRaw
            (field "state" Decode.string)
            (field "rating" Decode.int)


gunIncidentsDecoder : Decoder (List GunIncidentRaw)
gunIncidentsDecoder =
    Decode.list <|
        Decode.map6 GunIncidentRaw
            (field "incidentId" Decode.int)
            (field "incidentDate" posixDecoder)
            (field "state" Decode.string)
            (field "cityOrCounty" Decode.string)
            (field "killed" Decode.int)
            (field "injured" Decode.int)
