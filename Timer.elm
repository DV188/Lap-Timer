module Timer exposing (..)

import Time exposing (Time, every, second)

-- MODEL

type alias Timer =
    { time : Time
    , counting : Bool
    }

-- UPDATE

type Msg
    = Zero 
    | Start
    | Tick 

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
        Tick ->
            {timer | time = timer.time + second}

-- SUBSCRIPTIONS

subscriptions : Timer -> Sub Msg
subscriptions timer =
    if timer.counting then
        every second (\_ -> Tick)
    else
        Sub.none
