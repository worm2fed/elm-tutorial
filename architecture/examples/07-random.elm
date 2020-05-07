import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (..)
import Random
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, rx, ry, viewBox, width, x, y)
import Tuple exposing (first, second)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type Face = One | Two | Three | Four | Five | Six

type alias Model =
  { dieFaces : (Face, Face)
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model (One, One)
  , Cmd.none
  )


-- UPDATE


type Msg
  = Roll
  | NewFace (Face, Face)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , (Random.generate NewFace (Random.pair roll roll))
      )

    NewFace newFaces ->
      ( Model newFaces
      , Cmd.none
      )

roll : Random.Generator Face
roll =
  Random.weighted
    (1 / 6, One)
    [ (1 / 6, Two)
    , (1 / 6, Three)
    , (1 / 6, Four)
    , (1 / 6, Five)
    , (1 / 6, Six)
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ dice (first(model.dieFaces)) ]
        , div []
            [ dice (second(model.dieFaces)) ]
        , button [ onClick Roll ] [ text "Roll" ]
    ]

dice value =
    case value of
         One ->
             svg
                [ width "120", height "120", viewBox "0 0 120 120" ]
                [ circle [ cx "50", cy "50", r "5"] [] ]

         Two ->
            svg
                [ width "120", height "120", viewBox "0 0 120 120" ]
                [ circle [ cx "25", cy "50", r "5"] []
                , circle [ cx "75", cy "50", r "5"] []
                ]

         Three ->
            svg
                [ width "120", height "120", viewBox "0 0 120 120" ]
                [ circle [ cx "25", cy "50", r "5"] []
                , circle [ cx "50", cy "50", r "5"] []
                , circle [ cx "75", cy "50", r "5"] []
                ]

         Four ->
            svg
                [ width "120", height "120", viewBox "0 0 120 120" ]
                [ circle [ cx "25", cy "25", r "5"] []
                , circle [ cx "75", cy "25", r "5"] []
                , circle [ cx "25", cy "75", r "5"] []
                , circle [ cx "75", cy "75", r "5"] []
                ]

         Five ->
            svg
                [ width "120", height "120", viewBox "0 0 120 120" ]
                [ circle [ cx "25", cy "25", r "5"] []
                , circle [ cx "75", cy "25", r "5"] []
                , circle [ cx "50", cy "50", r "5"] []
                , circle [ cx "25", cy "75", r "5"] []
                , circle [ cx "75", cy "75", r "5"] []
                ]

         Six ->
            svg
                [ width "120", height "120", viewBox "0 0 120 120" ]
                [ circle [ cx "25", cy "25", r "5"] []
                , circle [ cx "50", cy "25", r "5"] []
                , circle [ cx "75", cy "25", r "5"] []
                , circle [ cx "25", cy "75", r "5"] []
                , circle [ cx "50", cy "75", r "5"] []
                , circle [ cx "75", cy "75", r "5"] []
                ]
