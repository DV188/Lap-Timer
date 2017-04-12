import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Timer exposing (Timer)
import Time

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
    Timer

init : (Model, Cmd Msg)
init =
    ({time = 0, counting = False}, Cmd.none)

-- UPDATE

type Msg
    = Tick
    | TimerMsg Timer.Msg
    | Start

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick ->
            (Timer.step model, Cmd.none)
        TimerMsg msg ->
            (Timer.update msg model, Cmd.none)
        Start ->
            (Timer.start model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (0.01*Time.second) (\_ -> Tick)

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ model.time
                |> Time.inSeconds
                |> toString
                |> text
           , button [onClick (TimerMsg Timer.Start)] [text "Start"]
           , button [onClick (Start)] [text "Start"]
           , button [onClick (TimerMsg Timer.Zero)] [text "Reset"]
           ]
