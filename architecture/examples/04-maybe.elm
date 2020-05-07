import Browser
import Html exposing (Attribute, Html, br, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { celsiusInput : String
  , fahrenheitInput : String
  , inchesInput : String
  }


init : Model
init =
  { celsiusInput = ""
    , fahrenheitInput = ""
    , inchesInput = ""
  }



-- UPDATE


type Msg
  = ChangeCelsiusInput String
  | ChangeFahrenheitInput String
  | ChangeInchesInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeCelsiusInput newCelsiusInput ->
            { model | celsiusInput = newCelsiusInput }

        ChangeFahrenheitInput newFahrenheitInput ->
            { model | fahrenheitInput = newFahrenheitInput }

        ChangeInchesInput newInchesInput ->
            { model | inchesInput = newInchesInput }



-- VIEW


view : Model -> Html Msg
view model =
  case String.toFloat model.celsiusInput of
    Just celsius ->
      viewConverter model.celsiusInput "blue" (String.fromFloat (celsius * 1.8 + 32))

    Nothing ->
      viewConverter model.celsiusInput "red" "???"


viewConverter : String -> String -> String -> Html Msg
viewConverter userInput color equivalentTemp =
    div []
        [ span []
            [ input [ value userInput, onInput ChangeCelsiusInput, style "width" "40px", style "border-color" color ] []
            , text "°C = "
            , span [ style "color" color ] [ text equivalentTemp ]
            , text "°F"
            ]
        ]
    