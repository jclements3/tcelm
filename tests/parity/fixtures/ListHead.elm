module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (List.head [42, 1, 2])
