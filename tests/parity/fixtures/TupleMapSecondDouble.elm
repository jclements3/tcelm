module Main exposing (main)

main : Int
main =
    Tuple.second (Tuple.mapSecond (\x -> x * 2) (100, 21))
