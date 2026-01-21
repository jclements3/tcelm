module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Just 42)
