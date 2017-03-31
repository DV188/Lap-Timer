import Html exposing (Html, div, text, button, input, ul, li)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, value)
import Time exposing (Time, every, second)

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
    , name : String
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
    = Add Timer -- add to list of timers in the model
    | DeleteInput -- clears current text in the model, clears name text field
    | UpdateInput String -- updates the input for the model
    | UpdateTimer Int String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add timer ->
            ({model | timers = timer :: model.timers}, Cmd.none)
        DeleteInput ->
            ({model | input = ""}, Cmd.none)
        UpdateInput input ->
            ({model | input = input}, Cmd.none)
        UpdateTimer indexToUpdate newName ->
            ({model | timers = updateTimer indexToUpdate model.timers newName}, Cmd.none)

-- constructor for new timer
createTimer : String -> Timer
createTimer name =
    { time = 0
    , counting = False
    , name = name
    }

updateTimer : Int -> List Timer -> String -> List Timer
updateTimer indexToUpdate list newName =
    List.indexedMap
        (\index timer ->
            if indexToUpdate == index then
                {timer | name = newName }
            else
                timer
        )
        list

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
        , div [] (List.indexedMap viewTimer model.timers)
        ]

viewTimer : Int -> Timer -> Html Msg
viewTimer timerIndex timer =
    div [] 
        [ button [] [text "START"]
        , button [] [text "RESET"]
        , input
            [ type_ "text"
            , onInput (UpdateTimer timerIndex)
            , value timer.name
            ] []
        ]
