module Main exposing (main)

main : Int
main =
    List.sum (List.map3 (\a b c -> a + b + c) [1, 2] [10, 20] [100, 200])
