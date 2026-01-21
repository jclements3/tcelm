module Main exposing (main)

main : Int
main =
    Tuple.first (Tuple.mapFirst (\x -> x * 2) ( 21, 99 ))
