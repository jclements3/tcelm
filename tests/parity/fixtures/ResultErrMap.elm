module Main exposing (main)

main : Int
main =
    Result.withDefault 42 (Result.map (\x -> x + 1) (Err 0))
