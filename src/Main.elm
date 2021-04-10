module Main exposing (..)

import Browser exposing (Document)
import Element exposing (Attribute, Element, column, el, fill, height, padding, px, shrink, spacing, table, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder, field)
import List.Extra as L
import Plot exposing (circle, decentPositions, dots, interval, simpleLabel, simpleLine, simpleTick)
import Time exposing (Posix)
import UI.Colors
import UI.Helpers exposing (border)


main : Program Decode.Value Model Msg
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


type alias UIAttrs msg =
    List (Attribute msg)



-- INIT


type alias Model =
    { gunIncidentsRaw : List GunIncidentRaw
    , gunLawsRaw : List GunLawRaw
    , stateIncidents : List StateIncidents
    , paginatedStateIncidents : List (List StateIncidents)
    , currentPage : Int
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        { incidentsRaw, gunLawsRaw } =
            Result.withDefault { incidentsRaw = [], gunLawsRaw = [] } (Decode.decodeValue initFlagsDecoder flags)

        stateIncidents =
            getStateData gunLawsRaw incidentsRaw
    in
    ( { gunIncidentsRaw = incidentsRaw
      , gunLawsRaw = gunLawsRaw
      , stateIncidents = stateIncidents
      , paginatedStateIncidents = L.greedyGroupsOf 11 stateIncidents
      , currentPage = 0
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


type PageDirection
    = Prev
    | Next


type Msg
    = ChangePage PageDirection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage dir ->
            handlePageChange model dir


handlePageChange : Model -> PageDirection -> ( Model, Cmd Msg )
handlePageChange model dir =
    let
        newPage =
            case dir of
                Prev ->
                    model.currentPage - 1

                Next ->
                    model.currentPage + 1
    in
    ( { model | currentPage = clamp 0 (List.length model.paginatedStateIncidents) newPage }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "guns"
    , body = [ layout model ]
    }


layout : Model -> Html Msg
layout model =
    Element.layout [] <|
        column [ width fill, spacing 25 ]
            [ el [ height (px 400), width fill ] <| Element.html (scatterChart model.stateIncidents)
            , column [ width shrink, Element.centerX ]
                [ dataTable (L.getAt model.currentPage model.paginatedStateIncidents)
                , Element.row [ width fill ] [ pageButton Prev "Prev", pageButton Next "Next" ]
                ]
            ]


pageButton : PageDirection -> String -> Element Msg
pageButton direction label =
    let
        alignment : Attribute msg
        alignment =
            case direction of
                Prev ->
                    Element.alignLeft

                Next ->
                    Element.alignRight
    in
    Input.button [ alignment, Element.paddingXY 0 10 ] { onPress = Just (ChangePage direction), label = text label }



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


dataTable : Maybe (List StateIncidents) -> Element msg
dataTable stateIncidents =
    el [ width fill ] <|
        table (combineAttrs [ solidBlackBorder, [ width shrink, Element.centerX ] ])
            { data = Maybe.withDefault [] stateIncidents
            , columns =
                [ { header = tableHeader "State"
                  , width = shrink
                  , view = cell [ solidBlackBorder, [ padding 10 ] ] .state
                  }
                , { header = tableHeader "Gun Law Rating"
                  , width = shrink
                  , view = cell [ solidBlackBorder, [ padding 10, Font.alignRight ] ] (.rating >> String.fromInt)
                  }
                , { header = tableHeader "Incidents"
                  , width = shrink
                  , view = cell [ solidBlackBorder, [ padding 10, Font.alignRight ] ] (.incidents >> String.fromInt)
                  }
                ]
            }


solidBlackBorder : List (Attribute msg)
solidBlackBorder =
    border 1 Border.solid UI.Colors.black


cell : List (UIAttrs msg) -> (data -> String) -> data -> Element msg
cell attrs getter val =
    el (combineAttrs attrs) (text (getter val))


tableHeader : String -> Element msg
tableHeader header =
    el
        (combineAttrs
            [ border 1 Border.solid UI.Colors.black
            , [ Font.size 20
              , Font.heavy
              , padding 10
              ]
            ]
        )
        (text header)



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



-- GENERICS


combineAttrs : List (List (Attribute msg)) -> List (Attribute msg)
combineAttrs =
    listFlatten


listFlatten : List (List a) -> List a
listFlatten list =
    List.foldl (\sub result -> result ++ sub) [] list
