module Main exposing (main)

main : Int
main =
    List.length (List.map (\x -> x * 2) [])
