module Main exposing (main)

main : Int
main =
    Tuple.first (Tuple.mapBoth (\x -> x * 2) (\y -> y + 1) ( 10, 20 )) + Tuple.second (Tuple.mapBoth (\x -> x * 2) (\y -> y + 1) ( 10, 20 ))
