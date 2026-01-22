module Main exposing (main)

main : Int
main =
    String.length (String.filter Char.isDigit "a1b2c3")
