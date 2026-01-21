module Main exposing (main)

main : Int
main =
    List.sum (List.intersperse 0 [42])
