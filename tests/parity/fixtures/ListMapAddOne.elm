module Main exposing (main)

main : Int
main =
    List.sum (List.map (\x -> x + 1) [1, 2, 3])
