module Main exposing (main)

main : Int
main =
    Result.withDefault 0 (Result.map (\x -> x * 2) (Err 5))
