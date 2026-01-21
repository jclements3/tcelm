module Main exposing (main)

main : Int
main =
    String.length (String.filter Char.isAlpha "a1b2c3")
