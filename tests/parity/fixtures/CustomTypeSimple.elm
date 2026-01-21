module Main exposing (main)

type Maybe a
    = Just a
    | Nothing

main : Int
main =
    case Just 42 of
        Just x -> x
        Nothing -> 0
