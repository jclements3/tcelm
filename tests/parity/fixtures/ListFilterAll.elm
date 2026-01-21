module Main exposing (main)

main : Int
main =
    List.length (List.filter (\x -> x > 0) [1, 2, 3, 4, 5])
