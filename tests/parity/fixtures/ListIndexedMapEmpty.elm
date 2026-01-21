module Main exposing (main)

main : Int
main =
    List.length (List.indexedMap (\i x -> i + x) [])
