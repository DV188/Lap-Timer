import Html exposing (Html, div, text, button, input, p)

import Html.Attributes exposing (type_, value, placeholder, style)
import Html.Events exposing (onClick, onInput)
import Racer exposing (Racer)
import Time
import Timer exposing (Timer)

-- CSS boilerplate using the elm-mdl library
import Material
import Material.Button as Button
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import Material.Scheme
import Material.Textfield as Textfield
import Material.Typography as Typo

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
    , mdl : Material.Model -- Boilerplate: model store for any and all Mdl components you use.
    }

type alias Mdl = -- Boilerplate
    Material.Model

-- INIT

init : (Model, Cmd Msg)
init =
    ( { racers = []
      , input = ""
      , mdl = Material.model -- Boilerplate: Always use this initial Mdl model store.
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
    | LapTimer Int
    | Tick -- internal subscription refreshing the time every second
    | Mdl (Material.Msg Msg) -- Boilerplate: Msg clause for internal Mdl messages.

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
            ({model | racers = updateTimers Timer.start model.racers}, Cmd.none)
        LapTimer index ->
            ({model | racers = updateLap index model.racers}, Cmd.none)
        Tick ->
            ( { model | racers = updateTimers Timer.step model.racers
              }
            , Cmd.none
            )
        Mdl msg_ -> -- Boilerplate: Mdl action handler.
            Material.update Mdl msg_ model

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

updateLap : Int -> List Racer -> List Racer
updateLap indexToUpdate list =
    let
        mappingFunction index racer =
            if indexToUpdate == index then
                Racer.lap racer
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
    grid []
        ( cell [size All 12]
            [ div [style [("width", "473px"), ("margin", "auto")]]
                [ Textfield.render Mdl [0] model.mdl
                    [ Textfield.label "Enter racer name:"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Options.onInput UpdateInput
                    , Options.onClick DeleteInput
                    , Textfield.value model.input
                    ]
                    []
                , Button.render Mdl [1] model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick (Add (Racer.initName model.input))
                    , css "margin" "5px 5px"
                    ]
                    [text "ADD"]
                , Button.render Mdl [2] model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick StartAllTimer
                    , css "margin" "5px 5px"
                    ]
                    [text "RACE"]
                ]
            ]
            :: (List.indexedMap viewRacer model.racers)
        )
            |> Material.Scheme.top

viewRacer : Int -> Racer -> Material.Grid.Cell Msg
viewRacer racerIndex racer =
    let
        buttonRaised index msg label =
            Button.render Mdl [index] Material.model
                [ Button.raised
                , Button.colored
                , Button.ripple
                , Options.onClick (msg racerIndex)
                , css "margin" "0 5px"
                ]
                [text label]
        textStyled item =
            Options.styled p [Typo.button , Typo.left] [item]
    in
        cell [size All 4]
            [ Textfield.render Mdl [0] Material.model
                [ Textfield.text_
                , Options.onInput (NameRacer racerIndex)
                , Textfield.value racer.name
                ]
                []
            , buttonRaised 3 StartTimer "START"
            , buttonRaised 4 LapTimer "LAP"
            , buttonRaised 5 ZeroTimer "RESET"
            , textStyled
                ( racer.timer.time
                    |> Time.inSeconds
                    |> toString
                    |> text
                )
            , div []
                [ racer.lap
                    |> List.map (\lap -> toString (Time.inSeconds lap))
                    |> List.intersperse " | "
                    |> String.concat
                    |> text
                    |> textStyled
                ]
            ]
