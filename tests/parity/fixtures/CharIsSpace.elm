module Main exposing (main)

main : Int
main =
    if Char.isSpace ' ' && Char.isSpace '\t' then 1 else 0
