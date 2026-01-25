-- expect: 32
module Test exposing (main)

main =
    let
        a = UInt16.fromInt 8
    in
    UInt16.toInt (UInt16.shiftLeftBy 2 a)
