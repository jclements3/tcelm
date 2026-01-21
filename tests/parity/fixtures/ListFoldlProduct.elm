module Main exposing (main)

main : Int
main =
    List.foldl (\x acc -> x * acc) 1 [2, 3, 4]
