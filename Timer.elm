module Timer exposing (..)

import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, every, second)
import Maybe exposing (withDefault)

-- MODEL

type alias Timer =
    { time : Int
    , counting : Bool
    }

-- INIT

init : (Timer, Cmd Msg)
init =
    ( {time = 0, counting = False}
    , Cmd.none
    )

-- UPDATE

type Msg
    = Zero 
    | Start
    | Tick Time

update : Msg -> Timer -> Timer
update msg timer =
    case msg of
        Zero ->
            { timer |
                time = 0,
                counting = False
            }
        Start ->
            {timer | counting = not timer.counting}
        Tick _ ->
            {timer | time = timer.time + 1}

-- SUBSCRIPTIONS

subscriptions : Timer -> Sub Msg
subscriptions timer =
    every Time.second Tick

-- VIEW

view : Timer -> Html Msg
view timer =
    div [] [viewTimer timer]

viewTimer : Timer -> Html Msg
viewTimer timer =
    div []
        [ button [onClick Start] [text "START"]
        , button [onClick Zero] [text "RESET"]
        , timer.time
            |> toString
            |> text
        ]
