module Main exposing (main)

main : Int
main =
    List.length (List.filter (\x -> x > 100) [1, 2, 3, 4, 5])
