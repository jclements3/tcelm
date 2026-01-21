module Main exposing (main)

main : Int
main =
    List.foldr (\x acc -> x * acc) 1 [2, 3, 4]
