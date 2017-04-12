module Timer exposing (..)

import Time exposing (Time, every, second, inSeconds)

-- MODEL

type alias Timer =
    { time : Time
    , counting : Bool
    }

-- UPDATE

type Msg
    = Zero 
    | Start

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

step : Timer -> Timer
step timer =
    if timer.counting == True then
        {timer | time = timer.time + (0.01*second)}
    else
        timer

zero : Timer -> Timer
zero timer =
    {timer
        | time = 0
        , counting = False
    }

start : Timer -> Timer
start timer =
    {timer | counting = not timer.counting}
