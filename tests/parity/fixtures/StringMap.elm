module Main exposing (main)

main : Int
main =
    if String.map Char.toUpper "hello" == "HELLO" then 1 else 0
