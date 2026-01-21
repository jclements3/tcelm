module Main exposing (main)

main : Int
main =
    List.sum (List.map2 (\a b -> a + b) [1, 2, 3] [10, 20, 30])
