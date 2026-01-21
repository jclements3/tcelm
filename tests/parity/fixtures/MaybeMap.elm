module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Maybe.map (\x -> x * 2) (Just 21))
