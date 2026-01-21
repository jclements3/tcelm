module Main exposing (main)

type Maybe a
    = Just a
    | Nothing

fromMaybe : Int -> Maybe Int -> Int
fromMaybe default maybe =
    case maybe of
        Just x -> x
        Nothing -> default

main : Int
main =
    fromMaybe 0 (Just 42)
