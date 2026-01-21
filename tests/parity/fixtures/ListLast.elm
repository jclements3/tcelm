module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (List.last [1, 2, 3, 4, 5])
