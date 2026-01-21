module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Result.toMaybe (Ok 42))
