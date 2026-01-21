module Main exposing (main)

main : Int
main =
    if Char.isHexDigit 'a' && Char.isHexDigit 'F' && Char.isHexDigit '9' && not (Char.isHexDigit 'g') then 1 else 0
