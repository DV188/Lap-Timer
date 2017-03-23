import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
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
    { time : Int
    , counting : Bool
    , name : Maybe String
    }

-- INIT

init : (Model, Cmd Msg)
init =
    ( { time = 0
      , counting = False
      , name = Nothing
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = Tick Time
    | Zero 
    | Start
    | Name String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
            if (model.counting) then
                ( { model | time = model.time + 1 }, Cmd.none)
            else
                (model, Cmd.none)
        Zero ->
            ( { model |
                    time = 0,
                    counting = False
              }
            , Cmd.none
            )
        Start ->
            ({ model | counting = not model.counting } , Cmd.none)
        Name name ->
            ({ model | name = Just name }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    every Time.second Tick

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [onInput Name] []
            , viewName model.name
            ]
        , viewTimer model
        ]

viewName : Maybe String -> Html Msg
viewName name =
    case name of
        Nothing -> text ""
        Just nameValue -> text nameValue

viewTimer : Model -> Html Msg
viewTimer model =
    div []
        [ button [onClick Start] [text "START"]
        , button [onClick Zero] [text "RESET"]
        , model.time
            |> toString
            |> text
        ]
