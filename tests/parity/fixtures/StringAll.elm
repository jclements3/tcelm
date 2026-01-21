module Main exposing (main)

main : Int
main =
    if String.all Char.isDigit "12345" && not (String.all Char.isDigit "123abc") then 1 else 0
