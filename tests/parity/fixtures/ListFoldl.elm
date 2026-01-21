module Main exposing (main)

main : Int
main =
    List.foldl (\x acc -> acc + x) 0 [1, 2, 3, 4, 5]
