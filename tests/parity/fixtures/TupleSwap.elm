module Main exposing (main)

main : Int
main =
    let
        pair = (10, 20)
    in
    Tuple.first pair + Tuple.second pair
