module Main exposing (main)

main : Int
main =
    Maybe.withDefault 42 (Maybe.map (\x -> x + 1) Nothing)
