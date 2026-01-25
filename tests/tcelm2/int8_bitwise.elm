-- expect: 12
module Test exposing (main)

main =
    let
        a = Int8.fromInt 15
        b = Int8.fromInt 12
    in
    Int8.toInt (Int8.and a b)
