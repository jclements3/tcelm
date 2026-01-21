module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (List.minimum [3, 7, 2, 9, 1])
