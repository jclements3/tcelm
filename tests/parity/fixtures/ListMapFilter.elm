module Main exposing (main)

main : Int
main =
    List.sum (List.filter (\x -> x > 5) (List.map (\x -> x * 2) [1, 2, 3, 4, 5]))
