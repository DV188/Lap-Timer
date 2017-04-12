import Html exposing (Html, div, text, button, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Time
import Timer exposing (..)

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
    -- | UpdateTimer Int String -- update the list of timers, substituting a new name at given index
    | StartTimer Int -- start and stop timer msg
    | ResetTimer Int -- sets the current timer to 0 and stops counting
    | Tick -- internal subscription refreshing the time every second

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add timer ->
            ({model | timers = timer :: model.timers}, Cmd.none)
        DeleteInput ->
            ({model | input = ""}, Cmd.none)
        UpdateInput input ->
            ({model | input = input}, Cmd.none)
        -- UpdateTimer index newName ->
        --     ({model | timers = updateTimer index newName model.timers}, Cmd.none)
        StartTimer index ->
            ({model | timers = updateTimer index Timer.start model.timers}, Cmd.none)
        ResetTimer index ->
            ({model | timers = updateTimer index Timer.zero model.timers}, Cmd.none)
        Tick ->
            ( { model | timers =
                    model.timers
                        |> List.map (\timer -> Timer.step timer)
              }
            , Cmd.none
            )

-- updates the name field of a timer by checking the index and mapping over the list
-- updateTimer : Int -> String -> List Timer -> List Timer
-- updateTimer indexToUpdate newName list =
--     List.indexedMap
--         (\index timer ->
--             if indexToUpdate == index then
--                 {timer | name = newName}
--             else
--                 timer
--         ) list

-- updateTimer : Int -> (Timer -> Timer) -> List Timer -> List Timer
-- updateTimer indexToUpdate timerFunction list =
--     List.indexedMap
--         (\index timer ->
--             if indexToUpdate == index then
--                 timerFunction timer
--             else
--                 timer
--         ) list

updateTimer : Int -> (Timer -> Timer) -> List Timer -> List Timer
updateTimer indexToUpdate timerFunction list =
    let
        mappingFunction index timer =
            if indexToUpdate == index then
                timerFunction timer
            else
                timer
    in
        list
            |> List.indexedMap mappingFunction

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (0.01*Time.second) (\_ -> Tick)

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
        , button [onClick (Add (Timer.init))] [text "ADD"]
        , div [] (List.indexedMap viewTimer model.timers)
        ]

viewTimer : Int -> Timer -> Html Msg
viewTimer timerIndex timer =
    div [] 
        [ input
            [ type_ "text"
            -- , onInput (UpdateTimer timerIndex)
            -- , value timer.name
            ] []
        , button [onClick (StartTimer timerIndex)] [text "START"]
        , button [onClick (ResetTimer timerIndex)] [text "RESET"]
        , timer.time
            |> Time.inSeconds
            |> toString
            |> text
        ]
