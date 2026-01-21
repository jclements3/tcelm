module Main exposing (main)

main : Int
main =
    List.sum (List.indexedMap (\i x -> i + x) [10, 20, 30])
