module Main exposing (main)

main : Int
main =
    List.sum (List.drop 2 [10, 20, 30, 40])
