module Main exposing (main)

main : Int
main =
    String.foldr (\c acc -> acc * 10 + Char.toCode c - 48) 0 "123"
