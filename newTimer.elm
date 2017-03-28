import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, every, second)
import Maybe exposing (withDefault)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { timers : List Timer
    }

type alias Timer =
    { time : Int
    , counting : Bool
    , name : Maybe String
    }

-- INIT

init : (Model, Cmd Msg)
init =
    ({ timers = [] } , Cmd.none)

-- UPDATE

type Msg
    = Add Timer

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add timer ->
            ({model | timers = timer :: model.timers}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    -- every Time.second Tick
    Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    let
        timer = 
            { time = 0
            , counting = False
            , name = Just "name"
            }
    in
        div []
        [ button [onClick (Add timer)] [text "ADD"]
        , withDefault (text "missing list") (List.head (List.map (\timer -> viewTimer timer) model.timers))
        ]

viewTimer : Timer -> Html Msg
viewTimer timer =
    div []
    [ button [] [text "START"]
    , button [] [text "RESET"]
    , timer.name
        |> withDefault "no name set"
        |> text
    ]
