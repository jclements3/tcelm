module Main exposing (main)

main : Int
main =
    case List.maximum [42] of
        Just x -> x
        Nothing -> 0
