module Main exposing (main)

main : Int
main =
    if Char.isSpace ' ' && Char.isSpace '\t' && not (Char.isSpace 'a') then 1 else 0
