module RippleCarryAdder exposing (rippleCarryAdder)

import Array
import Bitwise
import Helpers exposing (..)


andGate a b =
    Bitwise.and a b


orGate a b =
    Bitwise.or a b


inverter a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            -1


halfAdder a b =
    let
        d =
            orGate a b

        e =
            andGate a b
                |> inverter

        sumDigit =
            andGate d e

        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder a b carryIn =
    let
        firstResult =
            halfAdder b carryIn

        secondResult =
            halfAdder a firstResult.sum

        finalCarry =
            orGate firstResult.carry secondResult.carry
    in
    { carry = finalCarry
    , sum = secondResult.sum
    }


rippleCarryAdder a b carryIn =
    let
        -- Extract digits
        aDigits =
            extractDigits a

        bDigits =
            extractDigits b

        -- Compute sum and carry-out
        firstResult =
            fullAdder aDigits.firstDigit bDigits.firstDigit carryIn

        secondResult =
            fullAdder aDigits.secondDigit bDigits.secondDigit firstResult.carry

        thirdResult =
            fullAdder aDigits.thirdDigit bDigits.thirdDigit secondResult.carry

        finalResult =
            fullAdder aDigits.fourthDigit bDigits.fourthDigit thirdResult.carry
    in
    [ finalResult, thirdResult, secondResult, firstResult ]
        |> List.map .sum
        |> (::) finalResult.carry
        |> numberFromDigits
