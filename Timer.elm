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
    = Tick
    | Zero 
    | Start

update : Msg -> Timer -> (Timer, Cmd Msg)
update msg timer =
    case msg of
        Tick ->
            if (timer.counting) then
                ( { timer | time = timer.time + 1 }, Cmd.none)
            else
                (timer, Cmd.none)
        Zero ->
            ( { timer |
                    time = 0,
                    counting = False
              }
            , Cmd.none
            )
        Start ->
            ({ timer | counting = not timer.counting } , Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Timer -> Sub Msg
subscriptions timer =
    every Time.second (\_ -> Tick)

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
