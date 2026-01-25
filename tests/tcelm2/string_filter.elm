-- expect: "123"
module Test exposing (main)

main =
    String.filter Char.isDigit "a1b2c3"
