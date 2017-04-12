module Timer exposing (..)

import Time exposing (Time, second)

-- MODEL

type alias Timer =
    { time : Time
    , counting : Bool
    , name : String
    }

init : Timer
init =
    { time = 0
    , counting = False
    , name = ""
    }

-- UPDATE

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

name : String -> Timer -> Timer
name newName timer =
    {timer | name = newName}
