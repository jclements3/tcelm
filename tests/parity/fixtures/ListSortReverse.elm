module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (List.head (List.reverse (List.sort [3, 1, 4, 1, 5, 9, 2, 6])))
