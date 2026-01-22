module Main exposing (main)

main : Int
main =
    if String.all Char.isDigit "12345" then 1 else 0
