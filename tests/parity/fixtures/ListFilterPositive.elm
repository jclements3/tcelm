module Main exposing (main)

main : Int
main =
    List.sum (List.filter (\x -> x > 0) [-2, -1, 0, 1, 2])
