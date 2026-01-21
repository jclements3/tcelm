module Main exposing (main)

main : Int
main =
    if Char.isOctDigit '0' && Char.isOctDigit '7' && not (Char.isOctDigit '8') && not (Char.isOctDigit 'a') then 1 else 0
