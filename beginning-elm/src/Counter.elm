module Counter exposing (Model)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import VirtualDom


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Int


initialModel : Model
initialModel =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    VirtualDom.node "div"
        []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (Debug.toString model)
        , button [ onClick Increment ] [ text "+" ]
        ]
