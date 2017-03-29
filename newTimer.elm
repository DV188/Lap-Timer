import Html exposing (Html, div, text, button, input, ul, li)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, value)
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
    , input : String
    }

type alias Timer =
    { time : Int
    , counting : Bool
    , name : Maybe String
    }

-- INIT

init : (Model, Cmd Msg)
init =
    ( { timers = []
      , input = ""
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = Add Timer
    | DeleteInput
    | UpdateInput String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add timer ->
            ({model | timers = timer :: model.timers}, Cmd.none)
        DeleteInput ->
            ({model | input = ""}, Cmd.none)
        UpdateInput input ->
            ({model | input = input}, Cmd.none)

createTimer : String -> Timer
createTimer name =
    { time = 0
    , counting = False
    , name = Just name
    }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    -- every Time.second Tick
    Sub.none

-- VIEW

view : Model -> Html Msg
view model =
        div []
            [ input
                [ type_ "text"
                , onInput UpdateInput
                , onClick DeleteInput
                , value model.input
                ] []
            , button [onClick (Add (createTimer model.input))] [text "ADD"]
            , div [] (viewTimerList model)
            ]

viewTimerList : Model -> List (Html Msg)
viewTimerList model =
    let
        viewTimer timer =
            div [] 
                [ button [] [text "START"]
                , button [] [text "RESET"]
                , timer.name
                    |> withDefault "timer has no name"
                    |> text
                ]
    in
        List.map viewTimer model.timers
