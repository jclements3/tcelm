module Main exposing (main)

main : Int
main =
    if String.map Char.toUpper "abc" == "ABC" then 1 else 0
