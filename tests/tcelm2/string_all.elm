-- expect: True
module Test exposing (main)

main =
    String.all Char.isDigit "12345"
