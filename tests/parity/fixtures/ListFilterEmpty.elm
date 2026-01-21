module Main exposing (main)

main : Int
main =
    List.length (List.filter (\x -> x > 0) [])
