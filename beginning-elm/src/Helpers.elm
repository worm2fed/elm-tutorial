module Helpers exposing
    ( arrayToRecord
    , digits
    , extractDigits
    , numberFromDigits
    , padZeros
    )

import Array


digits number =
    if number == 0 then
        []

    else
        digits (number // 10) ++ [ Basics.modBy 10 number ]


padZeros total list =
    let
        numberOfZeros =
            total - List.length list
    in
    List.repeat numberOfZeros 0 ++ list


numberFromDigits digitsList =
    List.foldl (\digit number -> digit + 10 * number) 0 digitsList


extractDigits number =
    digits number
        |> padZeros 4
        |> Array.fromList
        |> arrayToRecord


arrayToRecord array =
    let
        firstElement =
            Array.get 3 array
                |> Maybe.withDefault -1

        secondElement =
            Array.get 2 array
                |> Maybe.withDefault -1

        thirdElement =
            Array.get 1 array
                |> Maybe.withDefault -1

        fourthElement =
            Array.get 0 array
                |> Maybe.withDefault -1
    in
    { firstDigit = firstElement
    , secondDigit = secondElement
    , thirdDigit = thirdElement
    , fourthDigit = fourthElement
    }
