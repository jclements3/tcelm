module Main exposing (main)

main : Int
main =
    if String.filter Char.isDigit "a1b2c3" == "123" then 1 else 0
