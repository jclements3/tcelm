-- expect: 10000000000
module Test exposing (main)

main =
    let
        a = Int64.fromInt 100000
        b = Int64.fromInt 100000
    in
    Int64.toInt (Int64.mul a b)
