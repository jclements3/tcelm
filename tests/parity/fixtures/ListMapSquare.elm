module Main exposing (main)

main : Int
main =
    List.sum (List.map (\x -> x * x) [1, 2, 3, 4])
