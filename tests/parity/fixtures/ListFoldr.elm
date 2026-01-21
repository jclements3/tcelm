module Main exposing (main)

main : Int
main =
    List.foldr (\x acc -> acc * x) 1 [1, 2, 3, 4]
