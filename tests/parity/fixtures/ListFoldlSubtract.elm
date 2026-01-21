module Main exposing (main)

main : Int
main =
    List.foldl (\x acc -> x - acc) 10 [1, 2, 3]
