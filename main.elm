import Html exposing (Html, text, div, button)
import Html.Events exposing (..)
import Timer exposing (Timer)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    List Timer

init : (Model, Cmd Msg)
init =
    ([], Cmd.none)

-- UPDATE

type Msg
    = Add Timer

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add timer ->
            ( timer :: model
            , Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick (Add {time = 0,  counting = True})
            ]
            [text "Click Me"]
        , div []
                (List.map (\timer -> text (toString timer.time)) model)
        ]
