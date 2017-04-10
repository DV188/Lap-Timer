module Timer exposing (..)

import Time exposing (Time, every, second)

-- MODEL

type alias Timer =
    { time : Int
    , counting : Bool
    }

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
    if timer.counting then
        every second Tick
    else
        Sub.none
