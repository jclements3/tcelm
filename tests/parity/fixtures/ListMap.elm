module Main exposing (main)

main : Int
main =
    List.sum (List.map (\x -> x * 2) [1, 2, 3])
