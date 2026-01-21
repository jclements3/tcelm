module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (List.head (List.reverse [1, 2, 3]))
