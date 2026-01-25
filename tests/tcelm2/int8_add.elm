-- expect: 15
module Test exposing (main)

main =
    let
        a = Int8.fromInt 10
        b = Int8.fromInt 5
    in
    Int8.toInt (Int8.add a b)
