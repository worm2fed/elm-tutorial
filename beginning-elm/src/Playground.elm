{-
   The Playground module is used to experiment with
   various concepts in Elm programming language.
-}


module Playground exposing
    ( Character
    , Greeting(..)
    , TVShows
    , add
    , addOne
    , arya
    , descending
    , doubleScores
    , escapeEarth
    , fromNumberListToString
    , getAdultAge
    , guuardiansWithShortNames
    , list1
    , list2
    , main
    , multiplier
    , multiplyByFive
    , nbaScores
    , revelation
    , sansa
    , sayHello
    , weekday
    , weekdayHashtag
    )

import Html exposing (Html, text)
import MyList exposing (MyList(..), isEmpty, sum)


escapeEarth : Float -> Float -> String -> String
escapeEarth velocity sheepSpeed fuelStatus =
    let
        escapeVelocityInKmPerSec =
            11.186

        orbitalSpeedInKmPerSec =
            7.67

        whereToLand fuel =
            if fuelStatus == "low" then
                "Land on droneship"

            else
                "Land on launchpad"
    in
    if velocity > escapeVelocityInKmPerSec then
        "Godspeed"

    else if sheepSpeed == orbitalSpeedInKmPerSec then
        "Stay in orbit"

    else
        whereToLand fuelStatus


weekday : Int -> String
weekday dayInNumber =
    case dayInNumber of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "Unknown day"


weekdayHashtag : Int -> String
weekdayHashtag dayInNumber =
    case weekday dayInNumber of
        "Sunday" ->
            "#Sunday"

        "Monday" ->
            "#Monday"

        "Tuesday" ->
            "#Tuesday"

        "Wednesday" ->
            "#Wednesday"

        "Thursday" ->
            "#Thursday"

        "Friday" ->
            "#Friday"

        "Saturday" ->
            "#Saturday"

        _ ->
            "#wdatever"


revelation =
    """
    It became very clear to me sitting out there today
    that every decision I've made in my entire life has
    been wrong. My life is the complete "opposite" of
    everything I want it to be. Every instinct I have,
    in every aspect of life, be it something to wear,
    something to eat - it's all been wrong.
    """


descending : Int -> Int -> Order
descending a b =
    case compare a b of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


fromNumberListToString : List Int -> String
fromNumberListToString numberList =
    List.map String.fromInt numberList
        |> String.concat


type alias TVShows =
    { name : String
    , creator : String
    , episodes : Int
    }


multiplyByFive : Int -> Int
multiplyByFive number =
    let
        multiplier5 =
            5
    in
    number * multiplier5


multiplier =
    2


nbaScores =
    [ 316, 320, 312, 370, 337, 318, 314 ]


doubleScores : List Int -> List Int
doubleScores scores =
    List.map (\x -> x * multiplier) scores


addOne : number -> number
addOne x =
    x + 1


guuardiansWithShortNames : List String -> Int
guuardiansWithShortNames guuardians =
    guuardians
        |> List.map String.length
        |> List.filter (\x -> x < 6)
        |> List.length


add : Int -> (Int -> Int)
add x y =
    x + y


type Greeting a
    = Howdy
    | Hola
    | Namaste String
    | NumericalHi Int Int


sayHello : Greeting a -> String
sayHello greeting =
    case greeting of
        Howdy ->
            "How y'all doin'?"

        Hola ->
            "Hola amigo!"

        Namaste message ->
            message

        NumericalHi val1 val2 ->
            val1 + val2 |> String.fromInt


type alias Character =
    { name : String
    , age : Maybe Int
    }


sansa : Character
sansa =
    { name = "Sansa"
    , age = Just 19
    }


arya : Character
arya =
    { name = "Arya"
    , age = Nothing
    }


getAdultAge : Character -> Maybe Int
getAdultAge character =
    case character.age of
        Nothing ->
            Nothing

        Just age ->
            if age >= 18 then
                Just age

            else
                Nothing


list1 : MyList a
list1 =
    Empty


list2 : MyList number
list2 =
    Node 9 Empty


list3 : List a
list3 =
    []


type alias User =
    { name : String
    , email : String
    , age : Int
    , isLoggedIn : Bool
    }


welcomeMessage : { a | isLoggedIn : Bool, name : String } -> String
welcomeMessage { isLoggedIn, name } =
    case isLoggedIn of
        True ->
            "Welcome " ++ name ++ "!"

        False ->
            "Please log in."


main : Html msg
main =
    isEmpty list2
        |> Debug.toString
        |> text
