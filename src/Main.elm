module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.paused of
        True ->
            Sub.none

        False ->
            Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        hourInt =
            Time.toHour model.zone model.time

        analogMinute =
            String.fromInt (Time.toMinute model.zone model.time * 360 // 60 - 90)

        analogHour =
            if hourInt > 12 then
                String.fromInt ((hourInt - 12) * 360 // 12 - 90)

            else
                String.fromInt (hourInt * 360 // 12 - 90)

        analogSecond =
            String.fromInt (Time.toSecond model.zone model.time * 360 // 60 - 90)
    in
    div []
        [ button
            [ onClick Pause
            , Html.Attributes.style "display" "block"
            ]
            [ Html.text "Pause" ]
        , svg [ width "150", height "150", viewBox "0 0 150 150" ]
            [ clockFace
            , ticks
            , minuteHand analogMinute
            , hourHand analogHour
            , secondHand analogSecond
            ]
        ]


clockFace : Html msg
clockFace =
    circle [ cx "75", cy "75", r "75" ] []


tick : String -> Svg msg
tick rotation =
    let
        transformAttr =
            "rotate(" ++ rotation ++ ", 75, 75) translate(50, 0)"
    in
    rect
        [ x "75"
        , y "75"
        , width "20"
        , height "1"
        , transform transformAttr
        , Svg.Attributes.style "fill: #fff"
        ]
        []


ticks : Html msg
ticks =
    let
        listRange =
            List.range 1 12

        rotations =
            List.map (\h -> String.fromInt (h * 360 // 12)) listRange
    in
    g [] (List.map tick rotations)


minuteHand : String -> Html msg
minuteHand rotation =
    let
        transformAttr =
            "rotate(" ++ rotation ++ ", 75, 75) translate(-3, -3)"
    in
    rect
        [ x "75"
        , y "75"
        , width "75"
        , height "6"
        , transform transformAttr
        , Svg.Attributes.style "fill: #fff"
        ]
        []


hourHand : String -> Html msg
hourHand rotation =
    let
        transformAttr =
            "rotate(" ++ rotation ++ ", 75, 75) translate(-3, -3)"
    in
    rect
        [ x "75"
        , y "75"
        , width "42"
        , height "6"
        , transform transformAttr
        , Svg.Attributes.style "fill: #999"
        ]
        []


secondHand : String -> Html msg
secondHand rotation =
    let
        transformAttr =
            "rotate(" ++ rotation ++ ", 75, 75) translate(-3, -1)"
    in
    rect
        [ x "75"
        , y "75"
        , width "75"
        , height "2"
        , transform transformAttr
        , Svg.Attributes.style "fill: #f00"
        ]
        []
