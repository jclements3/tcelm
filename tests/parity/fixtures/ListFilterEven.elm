module Main exposing (main)

main : Int
main =
    List.sum (List.filter (\x -> modBy 2 x == 0) [1, 2, 3, 4, 5, 6])
