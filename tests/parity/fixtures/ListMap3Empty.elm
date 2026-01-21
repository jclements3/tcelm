module Main exposing (main)

main : Int
main =
    List.length (List.map3 (\a b c -> a + b + c) [] [1] [1, 2])
