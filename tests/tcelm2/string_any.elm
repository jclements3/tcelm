-- expect: True
module Test exposing (main)

main =
    String.any Char.isDigit "abc123"
