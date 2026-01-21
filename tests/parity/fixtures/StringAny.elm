module Main exposing (main)

main : Int
main =
    if String.any Char.isDigit "abc123" && not (String.any Char.isDigit "hello") then 1 else 0
