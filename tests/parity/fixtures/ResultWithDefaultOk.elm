module Main exposing (main)

main : Int
main =
    Result.withDefault 0 (Ok 42)
