module Tests exposing (..)

import Test exposing (..)
import Expect
import Byte
import Carry
import Fuzz exposing (int)


all : Test
all =
    describe "Byte"
        [ fuzz int "should always be in the range 0 <= x < 256" <|
            \fuzzInt ->
                (Byte.toInt <| Byte.fromInt fuzzInt)
                    |> Expect.all
                        [ Expect.atLeast 0
                        , Expect.lessThan 256
                        ]
        , test "should overflow the byte" <|
            \() ->
                let
                    result =
                        Byte.addc (Byte.fromInt 230)
                            (Byte.fromInt 230)
                in
                    Carry.check result
                        |> Expect.true "Expected to have overflowed the byte"
        ]
