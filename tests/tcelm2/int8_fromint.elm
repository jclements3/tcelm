-- expect: 127
module Test exposing (main)

main =
    Int8.toInt (Int8.fromInt 127)
