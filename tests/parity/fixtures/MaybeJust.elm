module Main exposing (main)

main : Int
main =
    case Just 42 of
        Just n -> n
        Nothing -> 0
