-- expect: -128
module Test exposing (main)

main =
    Int8.toInt (Int8.fromInt 128)
