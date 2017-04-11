import Html exposing (Html, div, text, button, input)
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

-- constructor for new timer
createTimer : String -> Timer
createTimer name =
    { time = 0
    , counting = False
    , name = name
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
    | UpdateTimer Int String -- update the list of timers, substituting a new name at given index
    | StartTimer Int -- start and stop timer msg
    | ResetTimer Int -- sets the current timer to 0 and stops counting
    | Tick Time -- internal subscription refreshing the time every second

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add timer ->
            ({model | timers = timer :: model.timers}, Cmd.none)
        DeleteInput ->
            ({model | input = ""}, Cmd.none)
        UpdateInput input ->
            ({model | input = input}, Cmd.none)
        UpdateTimer index newName ->
            ({model | timers = updateTimer index newName model.timers}, Cmd.none)
        StartTimer index ->
            ({model | timers = startTimer index model.timers}, Cmd.none)
        ResetTimer index ->
            ({model | timers = resetTimer index model.timers}, Cmd.none)
        Tick _ ->
            ({model | timers = tickTimer model.timers}, Cmd.none)

-- updates the name field of a timer by checking the index and mapping over the list
updateTimer : Int -> String -> List Timer -> List Timer
updateTimer indexToUpdate newName list =
    List.indexedMap
        (\index timer ->
            if indexToUpdate == index then
                {timer | name = newName}
            else
                timer
        ) list

startTimer : Int -> List Timer -> List Timer
startTimer indexToUpdate list =
    List.indexedMap
    (\index timer ->
        if indexToUpdate == index then
            {timer | counting = not timer.counting}
        else
            timer
    ) list

resetTimer : Int -> List Timer -> List Timer
resetTimer indexToUpdate list =
    List.indexedMap
    (\index timer ->
        if indexToUpdate == index then
            {timer
                | counting = False
                , time = 0}
        else
            timer
    ) list

tickTimer : List Timer -> List Timer
tickTimer list =
    List.indexedMap
        (\index timer ->
            if timer.counting then
                {timer | time = timer.time + 1}
            else
                timer
        ) list

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    every Time.second Tick
    -- Sub.none

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
        [ input
            [ type_ "text"
            , onInput (UpdateTimer timerIndex)
            , value timer.name
            ] []
        , button [onClick (StartTimer timerIndex)] [text "START"]
        , button [onClick (ResetTimer timerIndex)] [text "RESET"]
        , timer.time
            |> toString
            |> text
        ]
