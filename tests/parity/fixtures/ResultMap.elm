module Main exposing (main)

main : Int
main =
    Result.withDefault 0 (Result.map (\x -> x + 1) (Ok 41))
