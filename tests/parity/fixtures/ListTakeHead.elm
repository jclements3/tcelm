module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (List.head (List.take 3 [10, 20, 30, 40, 50]))
