module Main exposing (main)

main : Int
main =
    Result.withDefault 99 (Err 0)
