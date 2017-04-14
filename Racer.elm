module Racer exposing (..)

import Time exposing (Time)
import Timer exposing (Timer)

-- MODEL

type alias Racer =
    { name : String
    , timer : Timer
    , lap : List Time
    }

init : Racer
init = 
    { name = ""
    , timer = Timer.init
    , lap = []
    }

initName : String -> Racer
initName name =
    { name = name
    , timer = Timer.init
    , lap = []
    }
