module Main exposing (main)

isDigit : Char -> Int
isDigit c =
    if c >= '0' && c <= '9' then
        1
    else
        0

main : Int
main =
    isDigit '5'
