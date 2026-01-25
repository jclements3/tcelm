-- expect: -126
module Test exposing (main)

main =
    let
        a = Int8.fromInt 127
        b = Int8.fromInt 3
    in
    Int8.toInt (Int8.add a b)
