module Main exposing (main)

main : Int
main =
    Maybe.withDefault -1 (List.head [])
