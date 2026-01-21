module Main exposing (main)

main : Int
main =
    if Char.isAlphaNum 'a' && Char.isAlphaNum '5' && not (Char.isAlphaNum '!') then 1 else 0
