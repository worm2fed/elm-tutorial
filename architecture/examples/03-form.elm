import Browser
import Char exposing (isDigit, isLower, isUpper)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Validation
    = None
    | Valid
    | Error String


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , valid : Validation
  }


init : Model
init =
  Model "" "" "" "" None


--pattern : regex
--pattern =
--    "[A-Z]"


-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Check


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
         { model | age = age }

    Check ->
        { model | valid = validate model }


validate: Model -> Validation
validate model =
    if model.password /= model.passwordAgain then
            Error "Passwords don't match"
        else if String.length model.password < 8 then
            Error "Password must be 8 characters or more"
        else if not (String.any isDigit model.password) then
            Error "Password must contain digits"
        else if not (String.any isUpper model.password) then
            Error "Password must contain uppercase"
        else if not (String.any isLower model.password) then
            Error "Password must contain lowercase"
        else if String.length model.age == 0 then
            Error "Enter age"
        else if not (String.all isDigit model.age) then
            Error "Age must be a number"
        else
            Valid


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Age" model.age Age
    , button [ onClick Check ] [ text "Submit" ]
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    let
        ( color, message ) =
            case model.valid of
                Valid ->
                    ( "green", "Ok" )
                Error error ->
                    ( "red", error )
                None ->
                    ( "black", "Fill the form" )
    in
    div [ style "color" color ] [ text message ]
