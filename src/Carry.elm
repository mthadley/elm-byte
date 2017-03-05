module Carry
    exposing
        ( check
        , checkHalf
        , unwrap
        )

{-| Opaque type representing the result of an operation and whether or not
there were any carries.

# Unwrapping
@docs unwrap, check, checkHalf

-}

import Internal.Carry exposing (Carry(..))


{-| Converts a `Carry` to it's value.
-}
unwrap : Carry a -> a
unwrap (Carry c) =
    c.value


{-| Returns `True` if there was a carry from the resulting operation.
-}
check : Carry a -> Bool
check (Carry c) =
    c.carry


{-| Returns `True` if there was a half-carry from the resulting
operation.
-}
checkHalf : Carry a -> Bool
checkHalf (Carry c) =
    c.halfCarry
