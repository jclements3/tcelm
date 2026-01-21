module Main exposing (main)

main : Int
main =
    String.foldl (\c acc -> acc + Char.toCode c) 0 "ABC"
