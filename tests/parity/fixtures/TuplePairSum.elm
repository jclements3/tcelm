module Main exposing (main)

main : Int
main =
    let
        t = (10, 20)
    in
    Tuple.first t + Tuple.second t
