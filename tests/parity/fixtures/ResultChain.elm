module Main exposing (main)

main : Int
main =
    Result.withDefault 0 (Result.andThen (\x -> Ok (x * 2)) (Result.map (\x -> x + 1) (Ok 20)))
