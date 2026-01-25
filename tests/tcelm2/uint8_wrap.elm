-- expect: 4
module Test exposing (main)

main =
    let
        a = UInt8.fromInt 255
        b = UInt8.fromInt 5
    in
    UInt8.toInt (UInt8.add a b)
