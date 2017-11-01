module Tests exposing (..)

import Test exposing (..)
import Expect


x : number
x =
    10


all : Test
all =
    describe "A Test Suite"
        [ test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "x function" <|
            \_ ->
                x
                    |> Expect.equal 10
        ]
