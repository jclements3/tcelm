-- expect: 1000000
module Test exposing (main)

main =
    let
        a = Int32.fromInt 1000
        b = Int32.fromInt 1000
    in
    Int32.toInt (Int32.mul a b)
