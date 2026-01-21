module Main exposing (main)

main : Int
main =
    List.sum (List.filter (\x -> modBy 2 x == 1) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
