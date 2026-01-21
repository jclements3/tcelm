module Main exposing (main)

main : Int
main =
    Result.withDefault 0 (Result.fromMaybe 99 (Just 42))
