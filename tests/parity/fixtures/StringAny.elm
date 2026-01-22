module Main exposing (main)

main : Int
main =
    if String.any Char.isDigit "abc123" then 1 else 0
