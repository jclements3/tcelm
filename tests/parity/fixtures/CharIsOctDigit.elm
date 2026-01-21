module Main exposing (main)

main : Int
main =
    if Char.isOctDigit '7' && not (Char.isOctDigit '8') then 1 else 0
