module Main exposing (main)

main : Int
main =
    Result.withDefault 0 (Result.andThen (\x -> Ok (x + 1)) (Ok 41))
