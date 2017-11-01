module Tests exposing (..)

import Test exposing (..)
import Expect


romanDigit : String -> Int
romanDigit roman =
    case roman of
        "I" ->
            1

        "V" ->
            5

        "X" ->
            10

        "L" ->
            50

        "C" ->
            100

        "D" ->
            500

        "M" ->
            1000

        _ ->
            0


romanOp : String -> String -> Int -> Int -> Int
romanOp first scnd =
    if (romanDigit first) < (romanDigit scnd) then
        (-)
    else
        (+)


fromRoman : String -> Int
fromRoman roman =
    let
        fromRomanList =
            fromRoman << String.join ""
    in
        case String.split "" roman of
            first :: scnd :: rest ->
                (romanOp first scnd)
                    (fromRomanList <| scnd :: rest)
                    (romanDigit first)

            first :: rest ->
                (romanDigit first) + (fromRomanList rest)

            _ ->
                0


testRoman : ( String, Int ) -> Test
testRoman ( roman, expectation ) =
    test ("fromRoman " ++ roman) <|
        \_ ->
            fromRoman roman
                |> Expect.equal expectation


all : Test
all =
    [ ( "I", 1 )
    , ( "II", 2 )
    , ( "III", 3 )
    , ( "IV", 4 )
    , ( "V", 5 )
    , ( "VI", 6 )
    , ( "VII", 7 )
    , ( "VIII", 8 )
    , ( "IX", 9 )
    , ( "X", 10 )
    , ( "XI", 11 )
    , ( "XII", 12 )
    , ( "XIV", 14 )
    , ( "XX", 20 )
    , ( "XXI", 21 )
    , ( "XXIX", 29 )
    , ( "XLV", 45 )
    , ( "L", 50 )
    , ( "LI", 51 )
    , ( "XC", 90 )
    , ( "C", 100 )
    , ( "CI", 101 )
    , ( "MMMCMXCIX", 3999 )
    ]
        |> List.map testRoman
        |> describe "fromRoman"
