import Html exposing (Html, text, div, button)
import Html.Events exposing (..)
import Timer exposing (Timer)
import Time
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
    Timer

init : (Model, Cmd Msg)
init =
    ({time = 0, counting = True}, Cmd.none)

-- UPDATE

type Msg
    = SubModuleMsg Timer.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SubModuleMsg msg ->
            (Timer.update msg model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SubModuleMsg (Timer.subscriptions model)

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ model.time 
                |> toString
                |> text
           ]
