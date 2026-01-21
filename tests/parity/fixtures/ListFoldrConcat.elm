module Main exposing (main)

main : Int
main =
    List.foldr (\x acc -> x + acc * 10) 0 [1, 2, 3]
