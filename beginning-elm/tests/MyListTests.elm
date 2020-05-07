module MyListTests exposing (sumTests)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import MyList exposing (..)
import Test exposing (..)


sumTests : Test
sumTests =
    describe "sum tests"
        [ fuzz2 int int "sum list elements" <|
            \num1 num2 ->
                sum (Node num1 (Node num2 Empty))
                    |> Expect.equal (num1 + num2)
        ]
