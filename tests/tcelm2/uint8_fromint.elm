-- expect: 255
module Test exposing (main)

main =
    UInt8.toInt (UInt8.fromInt 255)
