import Html exposing (Html, div, text, button, input)

import Html.Attributes exposing (type_, value, placeholder)
import Html.Events exposing (onClick, onInput)
import Racer exposing (Racer)
import Time
import Timer exposing (Timer)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { racers : List Racer
    , input : String
    }

-- INIT

init : (Model, Cmd Msg)
init =
    ( { racers = []
      , input = ""
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = DeleteInput -- clears current text in the model, clears name text field
    | UpdateInput String -- updates the input for the model
    | Add Racer -- add to list of timers in the model
    | StartTimer Int -- start and stop timer msg
    | ZeroTimer Int -- sets the current timer to 0 and stops counting
    | NameRacer Int String
    | StartAllTimer
    | Tick -- internal subscription refreshing the time every second

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DeleteInput ->
            ({model | input = ""}, Cmd.none)
        UpdateInput input ->
            ({model | input = input}, Cmd.none)
        Add racer ->
            ({model | racers = racer :: model.racers}, Cmd.none)
        StartTimer index ->
            ({model | racers = updateTimer index Timer.start model.racers}, Cmd.none)
        ZeroTimer index ->
            ({model | racers = updateTimer index Timer.zero model.racers}, Cmd.none)
        NameRacer index newName ->
            ({model | racers = updateName index newName model.racers}, Cmd.none)
        StartAllTimer ->
            -- ({model | racers = List.map (\racer -> Timer.start racer.timer) model.racers}, Cmd.none)
            ({model | racers = updateTimers Timer.start model.racers}, Cmd.none)
        Tick ->
            ( { model | racers = updateTimers Timer.step model.racers
--                         |> List.map
--                             (\racer ->
--                                 {racer | timer = Timer.step racer.timer}
--                             )
              }
            , Cmd.none
            )

updateTimer : Int -> (Timer -> Timer) -> List Racer -> List Racer
updateTimer indexToUpdate timerFunction list =
    let
        mappingFunction index racer =
            if indexToUpdate == index then
                {racer | timer = timerFunction racer.timer}
            else
                racer
    in
        List.indexedMap mappingFunction list

updateName : Int -> String -> List Racer -> List Racer
updateName indexToUpdate name list =
    let
        mappingFunction index racer =
            if indexToUpdate == index then
                {racer | name = name}
            else
                racer
    in
        List.indexedMap mappingFunction list

updateTimers : (Timer -> Timer) -> List Racer -> List Racer
updateTimers timerFunction list =
    let
        mappingFunction index racer =
                {racer | timer = timerFunction racer.timer}
    in
        List.indexedMap mappingFunction list

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
            , placeholder "Enter racer name."
            , onInput UpdateInput
            , onClick DeleteInput
            , value model.input
            ] []
        , button [onClick (Add (Racer.initName model.input))] [text "ADD"]
        , button [onClick (StartAllTimer )] [text "RACE"]
        , div [] (List.indexedMap viewRacer model.racers)
        ]

viewRacer : Int -> Racer -> Html Msg
viewRacer racerIndex racer =
    div [] 
        [ input
            [ type_ "text"
            , onInput (NameRacer racerIndex)
            , value racer.name
            ] []
        , button [onClick (StartTimer racerIndex)] [text "START"]
        , button [onClick (ZeroTimer racerIndex)] [text "RESET"]
        , racer.timer.time
            |> Time.inSeconds
            |> toString
            |> text
        ]
