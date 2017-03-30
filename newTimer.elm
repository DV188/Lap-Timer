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
    , input : Maybe String
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
      , input = Nothing
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = Add Timer -- add to list of timers in the model
    | DeleteInput -- clears current text in the model, clears name text field
    | UpdateInput String -- updates the input for the model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add timer ->
            ({model | timers = timer :: model.timers}, Cmd.none)
        DeleteInput ->
            ({model | input = Nothing}, Cmd.none)
        UpdateInput input ->
            ({model | input = Just input}, Cmd.none)

-- constructor for new timer
createTimer : Maybe String -> Timer
createTimer name =
    { time = 0
    , counting = False
    , name = name
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
                , value (withDefault "" model.input)
                ] []
            , button [onClick (Add (createTimer model.input))] [text "ADD"]
            , div [] (viewTimerList model)
            ]

-- div to draw the timers listed in the model
viewTimerList : Model -> List (Html Msg)
viewTimerList model =
    let
        viewTimer timer =
            div [] 
                [ button [] [text "START"]
                , button [] [text "RESET"]
                , timer.name
                    |> withDefault "N/A"
                    |> text
                ]
    in
        List.map viewTimer model.timers
