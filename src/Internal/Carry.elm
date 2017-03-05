module Internal.Carry exposing (..)


type Carry a
    = Carry
        { value : a
        , carry : Bool
        , halfCarry : Bool
        }
