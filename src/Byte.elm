module Byte
    exposing
        ( Byte
        , Carry
        , add
        , addc
        , and
        , complement
        , dec
        , decc
        , fromInt
        , getBit
        , highNibble
        , inc
        , incc
        , isZero
        , lowNibble
        , lsbSet
        , msbSet
        , or
        , reset
        , rotateLeft
        , rotateRight
        , set
        , setIf
        , shiftLeft
        , shiftRight
        , shiftRightZf
        , sub
        , subc
        , swap
        , toInt
        , xor
        )

{-| A collection of utilities for working with a `Byte`.



# Definition
@docs Byte, fromInt, toInt

# Basic Operations
@docs add, inc, sub, dec, isZero

# Bitwise Operations
@docs and, or, xor, shiftLeft, shiftRight, shiftRightZf, rotateLeft, rotateRight, complement, getBit, lsbSet, msbSet, set, reset, setIf, highNibble, lowNibble, swap

# Operations with a `Carry`.
These functions return a `Carry` instead of a `Byte` to check whether or not
the operation overflowed the `Byte`. You can get the resulting byte using
[`Carry.unwrap`](Carry#unwrap).
@docs Carry, addc, incc, subc, decc, shiftLeft, shiftRight, shiftRightZf

-}

import Bitwise
import Carry
import Internal.Carry exposing (Carry(..))


{-| An opaque type respresenting an 8-bit unsigned integer.
-}
type Byte
    = Byte Int


{-| A type alias for `Carry` result of a `Byte` operation.
-}
type alias Carry =
    Internal.Carry.Carry Byte


{-| Returns `True` if the `Byte` is zero.
-}
isZero : Byte -> Bool
isZero =
    (==) 0 << toInt


{-| Converts an `Int` to a `Byte`.

    fromInt 3 : Byte
-}
fromInt : Int -> Byte
fromInt =
    Byte << mask


{-| Converts a `Byte` to an `Int`.

    (fromInt 3 |> toInt) == 3

Guaranteed to be in the range: `0 <= n < 2^8`.
-}
toInt : Byte -> Int
toInt (Byte b) =
    b


{-| Adds two `Byte`s.
-}
add : Byte -> Byte -> Byte
add a b =
    Carry.unwrap <| addc a b


{-| Bitwise and (`&`) two `Byte`s.
-}
and : Byte -> Byte -> Byte
and (Byte a) (Byte b) =
    Byte <| Bitwise.and a b


{-| Bitwise or (`|`) two `Byte`s.
-}
or : Byte -> Byte -> Byte
or (Byte a) (Byte b) =
    Byte <| Bitwise.or a b


{-| Bitwise xor two `Byte`s.
-}
xor : Byte -> Byte -> Byte
xor (Byte a) (Byte b) =
    Byte <| Bitwise.xor a b


{-| Adds two `Byte`s, returning a `Carry`.
-}
addc : Byte -> Byte -> Carry
addc (Byte x) (Byte y) =
    let
        sum =
            x + y
    in
        Carry
            { value = (fromInt sum)
            , carry = (sum > 255)
            , halfCarry = (Bitwise.and ((maskLower x) + (maskLower y)) 0x10 > 0)
            }


{-| Bitwise complement (flips each bit).
-}
complement : Byte -> Byte
complement (Byte b) =
    fromInt <| Bitwise.complement b


{-| Subtracts the second `Byte` from the first.
-}
sub : Byte -> Byte -> Byte
sub a b =
    Carry.unwrap <| subc a b


{-| Subtracts the second `Byte` from the first, returning a carry.
-}
subc : Byte -> Byte -> Carry
subc (Byte x) (Byte y) =
    Carry
        { value = fromInt <| x - y
        , carry = x < y
        , halfCarry = (maskHigher x < (maskHigher y))
        }


{-| Increment a `Byte`.
-}
inc : Byte -> Byte
inc byte =
    add byte <| fromInt 1


{-| Increment a `Byte`, returning a `Carry`.
-}
incc : Byte -> Carry
incc byte =
    addc byte <| fromInt 1


{-| Decrement a `Byte`, returning a `Carry`.
-}
decc : Byte -> Carry
decc byte =
    subc byte <| fromInt 1


{-| Decrement a `Byte`.
-}
dec : Byte -> Byte
dec =
    Carry.unwrap << decc


{-| Returns a `Bool` indicating whether or not the most significant
bit is set.
-}
msbSet : Byte -> Bool
msbSet =
    getBit 7


{-| Returns a `Bool` indicating whether or not the least significant
bit is set.
-}
lsbSet : Byte -> Bool
lsbSet =
    getBit 0


{-| Returns a  `Bool` indicating whether or not the bit is set.
-}
getBit : Int -> Byte -> Bool
getBit n (Byte b) =
    (Bitwise.and 1 <| Bitwise.shiftRightBy n b) == 1


{-| Returns an `Int` respresenting the higher 4 bits.
-}
highNibble : Byte -> Int
highNibble (Byte b) =
    Bitwise.shiftRightZfBy 4 b


{-| Returns an `Int` respresenting the lower 4 bits.
-}
lowNibble : Byte -> Int
lowNibble (Byte b) =
    maskLower b


{-| Rotate a `Byte` left.
-}
rotateLeft : Byte -> Byte
rotateLeft =
    rotate Left


{-| Rotate a `Byte` right.
-}
rotateRight : Byte -> Byte
rotateRight =
    rotate Right


{-| Sets the nth bit of the `Byte`.
-}
set : Int -> Byte -> Byte
set n (Byte b) =
    Bitwise.shiftLeftBy n 1
        |> mask
        |> Bitwise.or b
        |> Byte


{-| Swaps the high and low nibbles of the `Byte`.
-}
swap : Byte -> Byte
swap byte =
    Byte <|
        (Bitwise.shiftLeftBy 4 <| lowNibble byte)
            + (highNibble byte)


{-| Resets the nth bit of the `Byte`.
-}
reset : Int -> Byte -> Byte
reset n (Byte b) =
    Bitwise.shiftLeftBy n 1
        |> Bitwise.complement
        |> Bitwise.and b
        |> fromInt


{-| Sets or resets the nth bit of the `Byte`.
-}
setIf : Int -> Bool -> Byte -> Byte
setIf n shouldSet byte =
    if shouldSet then
        set n byte
    else
        reset n byte


{-| Shifts a `Byte` left.
-}
shiftLeft : Byte -> Carry
shiftLeft ((Byte b) as byte) =
    Carry
        { value = fromInt <| Bitwise.shiftLeftBy 1 b
        , carry = msbSet byte
        , halfCarry = False
        }


{-| Shifts `Byte` right, preserving sign.
-}
shiftRight : Byte -> Carry
shiftRight ((Byte b) as byte) =
    let
        result =
            b
                |> Bitwise.shiftRightBy 1
                |> Byte
                |> setIf 7 (msbSet byte)
    in
        Carry
            { value = result
            , carry = (lsbSet byte)
            , halfCarry = False
            }


{-| Shifts `Byte` right, filling with zeroes.
-}
shiftRightZf : Byte -> Carry
shiftRightZf ((Byte b) as byte) =
    Carry
        { value = (Byte <| Bitwise.shiftRightZfBy 1 b)
        , carry = (lsbSet byte)
        , halfCarry = False
        }


type Rotation
    = Left
    | Right


rotate : Rotation -> Byte -> Byte
rotate rotation (Byte b) =
    let
        ( leftTimes, rightTimes ) =
            case rotation of
                Left ->
                    ( 1, 7 )

                Right ->
                    ( 7, 1 )
    in
        fromInt <|
            Bitwise.or
                (mask <| Bitwise.shiftLeftBy leftTimes b)
                (Bitwise.shiftRightZfBy rightTimes b)


mask : Int -> Int
mask =
    Bitwise.and 0xFF


maskHigher : Int -> Int
maskHigher =
    Bitwise.and 0xF0


maskLower : Int -> Int
maskLower =
    Bitwise.and 0x0F
