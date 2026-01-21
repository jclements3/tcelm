module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Maybe.andThen (\x -> Just (x * 2)) (Just 21))
