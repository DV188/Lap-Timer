import Html exposing (Html, text, div, button)
import Html.Events exposing (..)
import Timer exposing (Timer)
import Time
import Maybe exposing (withDefault)

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
    = TimerMsg Timer.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimerMsg msg ->
            (Timer.update msg model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TimerMsg (Timer.subscriptions model)

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ model.time
                |> Time.inSeconds
                |> toString
                |> text
           , button [onClick (TimerMsg Timer.Start)] [text "Start"]
           , button [onClick (TimerMsg Timer.Zero)] [text "Reset"]
           ]
