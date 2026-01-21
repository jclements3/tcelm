module Main exposing (main)

main : Int
main =
    List.length (List.map2 (\a b -> a + b) [] [1, 2, 3])
